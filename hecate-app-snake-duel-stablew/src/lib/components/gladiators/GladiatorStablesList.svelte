<script lang="ts">
	import { onMount } from 'svelte';
	import {
		DEFAULTS, COLORS, PRESETS, WEIGHT_BOUNDS, WEIGHT_GROUPS, DEFAULT_FITNESS_WEIGHTS,
		TUNING_BUDGET, computeTuningCost
	} from '../../game/gladiator-constants.js';
	import { fetchStables, initiateStable } from '../../game/client.js';
	import type { Stable, FitnessWeights } from '../../game/gladiator-types.js';

	interface Props {
		onSelectStable: (stableId: string, status: Stable['status']) => void;
		onViewHeroes?: () => void;
		seedStableId?: string | null;
		onClearSeed?: () => void;
	}

	let { onSelectStable, onViewHeroes, seedStableId = null, onClearSeed }: Props = $props();

	let stables = $state<Stable[]>([]);
	let loading = $state(true);
	let starting = $state(false);
	let startError = $state<string | null>(null);

	let populationSize = $state(DEFAULTS.populationSize);
	let maxGenerations = $state(DEFAULTS.maxGenerations);
	let opponentAf = $state(DEFAULTS.opponentAf);
	let episodesPerEval = $state(DEFAULTS.episodesPerEval);
	let championCount = $state(DEFAULTS.championCount);
	let enableLtc = $state(true);
	let enableLcChain = $state(true);

	// Fitness weight state
	let selectedPreset = $state(0); // index into PRESETS
	let showAdvanced = $state(true);
	let weights = $state<FitnessWeights>({ ...DEFAULT_FITNESS_WEIGHTS });

	const tuningCost = $derived(computeTuningCost(weights));
	const overBudget = $derived(tuningCost > TUNING_BUDGET);

	onMount(async () => {
		await loadStables();

		if (seedStableId) {
			const seedStable = stables.find((s) => s.stable_id === seedStableId);
			if (seedStable) {
				populationSize = seedStable.population_size;
				maxGenerations = seedStable.max_generations;
				opponentAf = seedStable.opponent_af;
				episodesPerEval = seedStable.episodes_per_eval;
				// Restore seed stable's weights if present
				if (seedStable.fitness_weights) {
					weights = { ...seedStable.fitness_weights };
					selectedPreset = -1; // Custom
				}
			}
		}
	});

	async function loadStables(): Promise<void> {
		loading = true;
		try {
			stables = await fetchStables();
		} catch (e) {
			console.error('[gladiators] Failed to load stables:', e);
		}
		loading = false;
	}

	function selectPreset(index: number): void {
		selectedPreset = index;
		weights = { ...PRESETS[index].weights };
	}

	async function handleStartTraining(): Promise<void> {
		if (overBudget) return;
		starting = true;
		startError = null;
		try {
			const config: Parameters<typeof initiateStable>[0] = {
				population_size: populationSize,
				max_generations: maxGenerations,
				opponent_af: opponentAf,
				episodes_per_eval: episodesPerEval,
				champion_count: championCount
			};
			if (seedStableId) {
				config.seed_stable_id = seedStableId;
			}
			// Include fitness config if not balanced/defaults
			const isDefault = selectedPreset === 0 && !showAdvanced;
			if (!isDefault || enableLtc || enableLcChain) {
				if (selectedPreset > 0 && !showAdvanced && !enableLtc && !enableLcChain) {
					config.training_config = { fitness_preset: PRESETS[selectedPreset].name };
				} else {
					config.training_config = {
						fitness_weights: weights,
						...(enableLtc ? { enable_ltc: true } : {}),
						...(enableLcChain ? { enable_lc_chain: true } : {})
					};
				}
			}
			const stableId = await initiateStable(config);
			onClearSeed?.();
			onSelectStable(stableId, 'training');
		} catch (e) {
			console.error('[gladiators] Failed to start training:', e);
			startError = e instanceof Error ? e.message : String(e);
		}
		starting = false;
	}

	function handleClearSeed(): void {
		onClearSeed?.();
	}

	function personalityLabel(af: number): string {
		if (af < 20) return 'Gentleman';
		if (af < 40) return 'Chill';
		if (af < 60) return 'Competitive';
		if (af < 80) return 'Aggressive';
		return 'Total Jerk';
	}

	function statusColor(status: Stable['status']): string {
		if (status === 'training') return COLORS.training;
		if (status === 'completed') return COLORS.completed;
		return COLORS.halted;
	}

	function formatTimeAgo(ts: number): string {
		const diff = Date.now() - ts;
		if (diff < 60_000) return 'just now';
		if (diff < 3_600_000) return `${Math.floor(diff / 60_000)}m ago`;
		if (diff < 86_400_000) return `${Math.floor(diff / 3_600_000)}h ago`;
		return `${Math.floor(diff / 86_400_000)}d ago`;
	}

	function presetBadge(stable: Stable): string {
		if (!stable.fitness_weights) return 'Balanced';
		for (const p of PRESETS) {
			if (JSON.stringify(stable.fitness_weights) === JSON.stringify(p.weights)) return p.label;
		}
		return 'Custom';
	}
</script>

<div class="flex flex-col h-full">
	<!-- Header -->
	<div
		class="flex items-center justify-between px-4 py-2.5 bg-surface-800 border-b border-surface-600 shrink-0"
	>
		<div class="w-12"></div>

		<h2
			class="text-sm font-bold tracking-wide"
			style="background: linear-gradient(135deg, #a78bfa, #fbbf24); -webkit-background-clip: text; -webkit-text-fill-color: transparent; background-clip: text;"
		>
			Snake Gladiators
		</h2>

		{#if onViewHeroes}
			<button
				onclick={onViewHeroes}
				class="text-amber-400 hover:text-amber-300 transition-colors text-xs font-semibold"
			>
				Heroes &rarr;
			</button>
		{:else}
			<div class="w-12"></div>
		{/if}
	</div>

	<!-- Body -->
	<div class="flex-1 overflow-y-auto">
		<div class="max-w-xl mx-auto px-4 py-6 flex flex-col gap-5">
			<!-- New Stable Card -->
			<div class="rounded-xl bg-surface-800/80 border border-surface-600/50 p-5">
				<div class="flex items-center justify-between mb-4">
					<div>
						<h3 class="text-sm font-semibold text-surface-100">
							{seedStableId ? 'Continue Training' : 'New Stable'}
						</h3>
						<p class="text-[10px] text-surface-400 mt-0.5">
							{seedStableId
								? 'Start a new run seeded with the champion network'
								: 'Breed a population of neural network snakes'}
						</p>
					</div>
					<button
						onclick={handleStartTraining}
						disabled={starting || overBudget}
						class="px-4 py-2 rounded-lg text-xs font-semibold transition-all duration-200
							bg-gradient-to-r from-purple-600 to-amber-600
							hover:from-purple-500 hover:to-amber-500
							text-white shadow-lg shadow-purple-900/30
							hover:shadow-purple-800/40
							disabled:opacity-50 disabled:cursor-not-allowed"
					>
						{#if overBudget}
							Over Budget
						{:else if starting}
							Starting...
						{:else}
							Start Training
						{/if}
					</button>
				</div>

				<!-- Seed indicator -->
				{#if seedStableId}
					<div class="flex items-center gap-2 mb-3 px-2 py-1.5 rounded-lg bg-purple-900/30 border border-purple-500/20">
						<span class="text-[10px] text-purple-300">
							Seeded from: <span class="font-mono text-purple-200">{seedStableId}</span>
						</span>
						<button
							onclick={handleClearSeed}
							class="text-[10px] text-surface-500 hover:text-surface-300 ml-auto"
						>
							Clear
						</button>
					</div>
				{/if}

				{#if startError}
					<div class="px-3 py-2 rounded-lg bg-red-900/30 border border-red-800/50 text-[11px] text-red-300">
						Failed to start training: {startError}
					</div>
				{/if}

				<!-- Training Settings -->
				<div class="flex flex-col gap-2.5">
					<label class="flex items-center gap-2 text-[11px] text-surface-300">
						<span class="w-24">Population</span>
						<input
							type="range"
							min="10"
							max="200"
							step="10"
							bind:value={populationSize}
							class="flex-1 accent-purple-500"
						/>
						<span class="text-surface-500 w-10 text-right tabular-nums">{populationSize}</span>
					</label>

					<label class="flex items-center gap-2 text-[11px] text-surface-300">
						<span class="w-24">Generations</span>
						<input
							type="range"
							min="10"
							max="300"
							step="10"
							bind:value={maxGenerations}
							class="flex-1 accent-purple-500"
						/>
						<span class="text-surface-500 w-10 text-right tabular-nums">{maxGenerations}</span>
					</label>

					<label class="flex items-center gap-2 text-[11px] text-surface-300">
						<span class="w-24">Opponent AF</span>
						<input
							type="range"
							min="0"
							max="100"
							step="1"
							bind:value={opponentAf}
							class="flex-1 accent-amber-500"
						/>
						<span class="text-surface-500 w-28 text-right">
							{opponentAf}
							<span class="text-surface-600">{personalityLabel(opponentAf)}</span>
						</span>
					</label>

					<label class="flex items-center gap-2 text-[11px] text-surface-300">
						<span class="w-24">Episodes/Eval</span>
						<input
							type="range"
							min="1"
							max="10"
							step="1"
							bind:value={episodesPerEval}
							class="flex-1 accent-purple-500"
						/>
						<span class="text-surface-500 w-10 text-right tabular-nums">{episodesPerEval}</span>
					</label>

					<label class="flex items-center gap-2 text-[11px] text-surface-300">
						<span class="w-24">Champions</span>
						<input
							type="range"
							min="1"
							max="10"
							step="1"
							bind:value={championCount}
							class="flex-1 accent-amber-500"
						/>
						<span class="text-surface-500 w-10 text-right tabular-nums">{championCount}</span>
					</label>

					<label class="flex items-center gap-2 text-[11px] text-surface-300">
						<span class="w-24">LTC Neurons</span>
						<button
							onclick={() => { enableLtc = !enableLtc; }}
							class="relative w-9 h-5 rounded-full transition-colors duration-200
								{enableLtc ? 'bg-cyan-600' : 'bg-surface-700'}"
						>
							<span
								class="absolute top-0.5 left-0.5 w-4 h-4 rounded-full bg-white transition-transform duration-200
									{enableLtc ? 'translate-x-4' : ''}"
							></span>
						</button>
						<span class="text-surface-500 text-[10px]">
							{enableLtc ? 'Enabled' : 'Off'}
						</span>
						{#if enableLtc}
							<span class="text-[9px] px-1.5 py-0.5 rounded bg-cyan-900/40 text-cyan-300 font-semibold border border-cyan-700/30">
								experimental
							</span>
						{/if}
					</label>

					<label class="flex items-center gap-2 text-[11px] text-surface-300">
						<span class="w-24">LC Chain</span>
						<button
							onclick={() => { enableLcChain = !enableLcChain; }}
							class="relative w-9 h-5 rounded-full transition-colors duration-200
								{enableLcChain ? 'bg-emerald-600' : 'bg-surface-700'}"
						>
							<span
								class="absolute top-0.5 left-0.5 w-4 h-4 rounded-full bg-white transition-transform duration-200
									{enableLcChain ? 'translate-x-4' : ''}"
							></span>
						</button>
						<span class="text-surface-500 text-[10px]">
							{enableLcChain ? 'Enabled' : 'Off'}
						</span>
						{#if enableLcChain}
							<span class="text-[9px] px-1.5 py-0.5 rounded bg-emerald-900/40 text-emerald-300 font-semibold border border-emerald-700/30">
								adaptive
							</span>
						{/if}
					</label>
				</div>

				<!-- Fitness Preset Selector -->
				<div class="mt-4">
					<div class="flex items-center gap-1.5 mb-2">
						<span class="text-[10px] text-surface-400 uppercase tracking-wider font-semibold">
							Breeding Philosophy
						</span>
					</div>
					<div class="flex flex-wrap gap-1.5">
						{#each PRESETS as preset, i}
							<button
								onclick={() => selectPreset(i)}
								class="px-3 py-1.5 rounded-lg text-[10px] font-semibold transition-all duration-200 border
									{selectedPreset === i
										? 'bg-purple-600/30 border-purple-500/50 text-purple-200'
										: 'bg-surface-900/50 border-surface-700/50 text-surface-400 hover:text-surface-200 hover:border-surface-600'}"
							>
								{preset.label}
							</button>
						{/each}
					</div>
					{#if selectedPreset >= 0}
						<p class="text-[10px] text-surface-500 mt-1.5 italic">
							{PRESETS[selectedPreset].description}
						</p>
					{/if}
				</div>

				<!-- Budget Bar -->
				<div class="mt-3">
					<div class="flex items-center gap-2 text-[10px]">
						<span class="text-surface-400">Tuning Budget:</span>
						<div class="flex-1 h-2 rounded-full bg-surface-900/50 overflow-hidden">
							<div
								class="h-full rounded-full transition-all duration-300"
								style:width="{Math.min(100, (tuningCost / TUNING_BUDGET) * 100)}%"
								style:background={overBudget ? COLORS.halted : COLORS.training}
							></div>
						</div>
						<span
							class="tabular-nums font-semibold"
							style:color={overBudget ? COLORS.halted : COLORS.training}
						>
							{tuningCost.toFixed(0)}/{TUNING_BUDGET}
						</span>
					</div>
				</div>

				<!-- Advanced Weights Toggle -->
				<button
					onclick={() => { showAdvanced = !showAdvanced; }}
					class="mt-3 text-[10px] text-surface-500 hover:text-surface-300 transition-colors"
				>
					{showAdvanced ? 'Hide' : 'Show'} Advanced Weights
				</button>

				<!-- Advanced Weight Sliders (Grouped) -->
				{#if showAdvanced}
					<div class="mt-3 flex flex-col gap-4">
						{#each WEIGHT_GROUPS as group}
							<div class="flex flex-col gap-2 pl-2 border-l-2" style:border-color="{group.color}40">
								<span class="text-[9px] uppercase tracking-wider font-semibold" style:color={group.color}>
									{group.label}
								</span>
								{#each group.keys as key}
									{@const bound = WEIGHT_BOUNDS[key]}
									<label class="flex items-center gap-2 text-[10px] text-surface-300">
										<span class="w-24">{bound.label}</span>
										<input
											type="range"
											min={bound.min}
											max={bound.max}
											step={bound.step}
											value={bound.invert ? -weights[key] : weights[key]}
											oninput={(e) => {
												const v = parseFloat((e.target as HTMLInputElement).value);
												weights[key] = bound.invert ? -v : v;
												selectedPreset = -1;
											}}
											class="flex-1 accent-purple-500"
										/>
										<span class="text-surface-500 w-14 text-right tabular-nums">
											{(bound.invert ? -weights[key] : weights[key]).toFixed(bound.step < 1 ? 1 : 0)}
										</span>
									</label>
								{/each}
							</div>
						{/each}
					</div>
				{/if}
			</div>

			<!-- Stables History -->
			<div class="rounded-xl bg-surface-800/80 border border-surface-600/50 p-4">
				<h3 class="text-xs font-semibold text-surface-300 mb-3 uppercase tracking-wider">
					Training History
				</h3>

				{#if loading}
					<p class="text-[11px] text-surface-500 italic text-center py-4">Loading...</p>
				{:else if stables.length === 0}
					<p class="text-[11px] text-surface-500 italic text-center py-4">
						No stables yet — start training!
					</p>
				{:else}
					<div class="flex flex-col gap-1.5">
						{#each stables as stable}
							<button
								onclick={() => onSelectStable(stable.stable_id, stable.status)}
								class="flex items-center gap-3 px-3 py-2 rounded-lg bg-surface-900/50 text-[11px]
									hover:bg-surface-900/80 transition-colors cursor-pointer w-full text-left"
							>
								<!-- Status badge -->
								<span
									class="w-16 font-semibold text-center capitalize"
									style:color={statusColor(stable.status)}
								>
									{stable.status}
								</span>

								<!-- Best fitness -->
								<span class="tabular-nums" style:color={COLORS.fitness}>
									{stable.best_fitness.toFixed(1)}
								</span>

								<!-- Generations -->
								<span class="text-surface-500 tabular-nums">
									Gen {stable.generations_completed}/{stable.max_generations}
								</span>

								<!-- Preset badge -->
								<span class="text-[9px] px-1.5 py-0.5 rounded bg-purple-900/30 text-purple-300">
									{presetBadge(stable)}
								</span>

								{#if stable.enable_ltc}
									<span class="text-[9px] px-1.5 py-0.5 rounded bg-cyan-900/40 text-cyan-300 font-semibold border border-cyan-700/30">
										LTC
									</span>
								{/if}

								{#if stable.enable_lc_chain}
									<span class="text-[9px] px-1.5 py-0.5 rounded bg-emerald-900/40 text-emerald-300 font-semibold border border-emerald-700/30">
										LC
									</span>
								{/if}

								<!-- Config -->
								<span class="text-surface-600 tabular-nums">
									Pop {stable.population_size} AF {stable.opponent_af}
								</span>

								<!-- Time ago -->
								<span class="text-surface-600 ml-auto">
									{formatTimeAgo(stable.completed_at ?? stable.started_at)}
								</span>
							</button>
						{/each}
					</div>
				{/if}
			</div>
		</div>
	</div>
</div>
