<script lang="ts">
	import { onMount, onDestroy } from 'svelte';
	import { COLORS, PRESETS, DEFAULT_FITNESS_WEIGHTS } from '../../game/gladiator-constants.js';
	import {
		connectTrainingStream,
		fetchStable,
		fetchGenerations,
		fetchChampions,
		haltTraining
	} from '../../game/client.js';
	import type { Stable, GenerationStats, Champion, TrainingProgress } from '../../game/gladiator-types.js';
	import GladiatorChampionCard from './GladiatorChampionCard.svelte';

	interface Props {
		stableId: string;
		onBack: () => void;
		onTestDuel?: (stableId: string, rank?: number) => void;
		onContinueTraining?: (stableId: string) => void;
	}

	let { stableId, onBack, onTestDuel, onContinueTraining }: Props = $props();

	let stable = $state<Stable | null>(null);
	let generations = $state<GenerationStats[]>([]);
	let champions = $state<Champion[]>([]);
	let status = $state<'connecting' | 'training' | 'completed' | 'halted' | 'error'>('connecting');
	let error = $state<string | null>(null);
	let halting = $state(false);
	let logExpanded = $state(true);

	// Live progress data accumulated from SSE
	let progressData = $state<TrainingProgress[]>([]);

	let cleanup: (() => void) | null = null;

	onMount(async () => {
		try {
			stable = await fetchStable(stableId);

			// Load existing generation data
			const existingGens = await fetchGenerations(stableId);
			generations = existingGens;

			if (stable.status === 'completed') {
				status = 'completed';
				logExpanded = false;
				try {
					champions = await fetchChampions(stableId);
				} catch {
					// champions may not be exported yet
				}
				return;
			}

			if (stable.status === 'halted') {
				status = 'halted';
				logExpanded = false;
				return;
			}

			// Connect to live stream
			status = 'training';
			cleanup = await connectTrainingStream(
				stableId,
				handleProgress,
				handleDone,
				handleError
			);
		} catch (e) {
			console.error('[gladiators] Failed to initialize monitor:', e);
			status = 'error';
			error = e instanceof Error ? e.message : String(e);
		}
	});

	onDestroy(() => {
		if (cleanup) cleanup();
	});

	function handleProgress(data: TrainingProgress): void {
		progressData = [...progressData, data];

		if (stable) {
			stable.generations_completed = data.generation;
			stable.best_fitness = Math.max(stable.best_fitness, data.best_fitness);
		}

		if (!data.running) {
			markComplete();
		}
	}

	function handleDone(): void {
		markComplete();
	}

	function markComplete(): void {
		if (status === 'completed' || status === 'halted') return;
		status = 'completed';
		logExpanded = false;
		if (cleanup) { cleanup(); cleanup = null; }

		// Fetch latest stable in background
		fetchStable(stableId).then((fresh) => {
			stable = fresh;
			if (fresh.status === 'halted') status = 'halted';
		}).catch(() => {});

		// Champions may not be recorded yet (training_complete fires after last generation_complete).
		// Retry with increasing delays.
		loadChampions(0);
	}

	function loadChampions(attempt: number): void {
		const delay = attempt === 0 ? 500 : attempt === 1 ? 2000 : 5000;
		setTimeout(() => {
			fetchChampions(stableId).then((c) => {
				if (c.length > 0) champions = c;
				else if (attempt < 3) loadChampions(attempt + 1);
			}).catch(() => {
				if (attempt < 3) loadChampions(attempt + 1);
			});
		}, delay);
	}

	function handleError(msg: string): void {
		status = 'error';
		error = msg;
	}

	async function handleHalt(): Promise<void> {
		halting = true;
		try {
			await haltTraining(stableId);
			status = 'halted';
			if (cleanup) { cleanup(); cleanup = null; }
			stable = await fetchStable(stableId);
		} catch (e) {
			console.error('[gladiators] Failed to halt:', e);
		}
		halting = false;
	}

	// Detect completion: if we have all generations but status is still 'training'
	$effect(() => {
		if (status !== 'training' || !stable) return;
		const totalGens = mergedRows().length;
		if (stable.max_generations > 0 && totalGens >= stable.max_generations) {
			markComplete();
		}
	});

	// Chart dimensions
	const chartW = 500;
	const chartH = 180;
	const chartPad = { top: 10, right: 10, bottom: 20, left: 40 };
	const plotW = chartW - chartPad.left - chartPad.right;
	const plotH = chartH - chartPad.top - chartPad.bottom;

	// Merge generations + live progress into sorted rows
	function mergedRows(): { gen: number; best: number; avg: number; worst: number }[] {
		const map = new Map<number, { gen: number; best: number; avg: number; worst: number }>();
		for (const g of generations) {
			map.set(g.generation, { gen: g.generation, best: g.best_fitness, avg: g.avg_fitness, worst: g.worst_fitness });
		}
		for (const p of progressData) {
			map.set(p.generation, { gen: p.generation, best: p.best_fitness, avg: p.avg_fitness, worst: p.worst_fitness });
		}
		return [...map.values()].sort((a, b) => a.gen - b.gen);
	}

	function buildPolyline(
		data: { gen: number; best: number; avg: number; worst: number }[],
		key: 'best' | 'avg' | 'worst'
	): string {
		if (data.length === 0) return '';

		const maxGen = Math.max(1, data[data.length - 1].gen);
		const allValues = data.flatMap((d) => [d.best, d.avg, d.worst]);
		const maxVal = Math.max(1, ...allValues);
		const minVal = Math.min(0, ...allValues);
		const range = maxVal - minVal || 1;

		return data
			.map((d) => {
				const x = chartPad.left + (d.gen / maxGen) * plotW;
				const y = chartPad.top + plotH - ((d[key] - minVal) / range) * plotH;
				return `${x},${y}`;
			})
			.join(' ');
	}

	const currentGen = $derived(
		progressData.length > 0
			? progressData[progressData.length - 1].generation
			: stable?.generations_completed ?? 0
	);
	const currentBest = $derived(
		progressData.length > 0
			? progressData[progressData.length - 1].best_fitness
			: stable?.best_fitness ?? 0
	);
	const currentAvg = $derived(
		progressData.length > 0 ? progressData[progressData.length - 1].avg_fitness : 0
	);
	const maxGens = $derived(stable?.max_generations ?? 0);
	const shortId = $derived(stableId.length > 8 ? stableId.slice(0, 8) : stableId);

	const presetLabel = $derived.by(() => {
		if (!stable?.fitness_weights) return 'Balanced';
		for (const p of PRESETS) {
			if (JSON.stringify(stable.fitness_weights) === JSON.stringify(p.weights)) return p.label;
		}
		return 'Custom';
	});

	function statusLabel(s: typeof status): string {
		if (s === 'connecting') return 'Connecting...';
		if (s === 'training') return 'Training';
		if (s === 'completed') return 'Completed';
		if (s === 'halted') return 'Halted';
		return 'Error';
	}

	function statusColor(s: typeof status): string {
		if (s === 'training' || s === 'connecting') return COLORS.training;
		if (s === 'completed') return COLORS.completed;
		return COLORS.halted;
	}
</script>

<div class="flex flex-col h-full">
	<!-- Header -->
	<div
		class="flex items-center justify-between px-4 py-2.5 bg-surface-800 border-b border-surface-600 shrink-0"
	>
		<button
			onclick={onBack}
			class="text-surface-400 hover:text-surface-100 transition-colors text-xs flex items-center gap-1"
		>
			&larr; Back
		</button>

		<h2
			class="text-sm font-bold tracking-wide"
			style="background: linear-gradient(135deg, #a78bfa, #fbbf24); -webkit-background-clip: text; -webkit-text-fill-color: transparent; background-clip: text;"
		>
			Training: {shortId}
		</h2>

		{#if status === 'training'}
			<button
				onclick={handleHalt}
				disabled={halting}
				class="text-[11px] px-3 py-1 rounded-lg font-semibold
					bg-red-900/60 text-red-300 hover:bg-red-800/60 transition-colors
					disabled:opacity-50 disabled:cursor-not-allowed"
			>
				{halting ? 'Halting...' : 'Halt'}
			</button>
		{:else}
			<div class="w-12"></div>
		{/if}
	</div>

	<!-- Body -->
	<div class="flex-1 overflow-y-auto">
		<div class="max-w-xl mx-auto px-4 py-6 flex flex-col gap-5">
			<!-- Progress Stats Row -->
			<div class="grid grid-cols-4 gap-2">
				<div class="rounded-lg bg-surface-800/60 border border-surface-700/50 px-3 py-2 text-center">
					<div class="text-lg font-bold text-surface-100 tabular-nums">
						{currentGen}/{maxGens}
					</div>
					<div class="text-[9px] text-surface-500 uppercase tracking-wider">Generation</div>
				</div>
				<div class="rounded-lg bg-surface-800/60 border border-surface-700/50 px-3 py-2 text-center">
					<div class="text-lg font-bold tabular-nums" style:color={COLORS.fitness}>
						{currentBest.toFixed(1)}
					</div>
					<div class="text-[9px] text-surface-500 uppercase tracking-wider">Best</div>
				</div>
				<div class="rounded-lg bg-surface-800/60 border border-surface-700/50 px-3 py-2 text-center">
					<div class="text-lg font-bold tabular-nums" style:color={COLORS.avgFitness}>
						{currentAvg.toFixed(1)}
					</div>
					<div class="text-[9px] text-surface-500 uppercase tracking-wider">Average</div>
				</div>
				<div class="rounded-lg bg-surface-800/60 border border-surface-700/50 px-3 py-2 text-center">
					<div class="text-lg font-bold tabular-nums" style:color={statusColor(status)}>
						{statusLabel(status)}
					</div>
					<div class="text-[9px] text-surface-500 uppercase tracking-wider">Status</div>
				</div>
			</div>

			<!-- Fitness Philosophy Badge -->
			{#if stable}
				<div class="flex items-center gap-2">
					<span class="text-[9px] px-2 py-0.5 rounded bg-purple-900/30 text-purple-300 font-semibold">
						{presetLabel}
					</span>
					{#if stable.fitness_weights && presetLabel === 'Custom'}
						<span class="text-[9px] text-surface-500">
							W:{stable.fitness_weights.win_bonus}
							K:{stable.fitness_weights.kill_bonus}
							F:{stable.fitness_weights.food_weight}
						</span>
					{/if}
				</div>
			{/if}

			{#if error}
				<div class="rounded-lg bg-red-900/30 border border-red-800/50 px-4 py-3 text-[11px] text-red-300">
					{error}
				</div>
			{/if}

			<!-- Chart + Log: share computed rows via {#if} block -->
			{#if mergedRows().length > 0}
				{@const rows = mergedRows()}
				{@const bestLine = buildPolyline(rows, 'best')}
				{@const avgLine = buildPolyline(rows, 'avg')}
				{@const worstLine = buildPolyline(rows, 'worst')}

				<!-- Fitness Chart -->
				<div class="rounded-xl bg-surface-800/80 border border-surface-600/50 p-4">
					<h3 class="text-xs font-semibold text-surface-300 mb-3 uppercase tracking-wider">
						Fitness Over Generations
					</h3>

					<svg viewBox="0 0 {chartW} {chartH}" class="w-full" preserveAspectRatio="xMidYMid meet">
						<!-- Grid lines -->
						{#each [0, 0.25, 0.5, 0.75, 1] as frac}
							<line
								x1={chartPad.left}
								y1={chartPad.top + plotH * (1 - frac)}
								x2={chartPad.left + plotW}
								y2={chartPad.top + plotH * (1 - frac)}
								stroke="#2a2a50"
								stroke-width="0.5"
							/>
						{/each}

						<!-- Data lines -->
						{#if worstLine}
							<polyline
								points={worstLine}
								fill="none"
								stroke={COLORS.worstFitness}
								stroke-width="1.5"
								stroke-opacity="0.5"
							/>
						{/if}
						{#if avgLine}
							<polyline
								points={avgLine}
								fill="none"
								stroke={COLORS.avgFitness}
								stroke-width="1.5"
								stroke-opacity="0.7"
							/>
						{/if}
						{#if bestLine}
							<polyline
								points={bestLine}
								fill="none"
								stroke={COLORS.fitness}
								stroke-width="2"
							/>
						{/if}

						<!-- X axis label -->
						<text
							x={chartPad.left + plotW / 2}
							y={chartH - 2}
							fill="#6b7280"
							font-size="10"
							text-anchor="middle"
						>
							Generation
						</text>
					</svg>

					<!-- Legend -->
					<div class="flex items-center gap-4 mt-2 justify-center">
						<span class="flex items-center gap-1 text-[10px]">
							<span class="inline-block w-3 h-0.5 rounded" style:background={COLORS.fitness}></span>
							<span class="text-surface-400">Best</span>
						</span>
						<span class="flex items-center gap-1 text-[10px]">
							<span class="inline-block w-3 h-0.5 rounded" style:background={COLORS.avgFitness}></span>
							<span class="text-surface-400">Average</span>
						</span>
						<span class="flex items-center gap-1 text-[10px]">
							<span class="inline-block w-3 h-0.5 rounded" style:background={COLORS.worstFitness}></span>
							<span class="text-surface-400">Worst</span>
						</span>
					</div>
				</div>

				<!-- Generation Log (collapsible) -->
				<div class="rounded-xl bg-surface-800/80 border border-surface-600/50">
					<button
						onclick={() => { logExpanded = !logExpanded; }}
						class="w-full flex items-center justify-between p-4 text-left hover:bg-surface-700/20 transition-colors rounded-xl"
					>
						<h3 class="text-xs font-semibold text-surface-300 uppercase tracking-wider">
							Generation Log
							<span class="text-surface-500 normal-case font-normal ml-1">({rows.length})</span>
						</h3>
						<span class="text-surface-500 text-xs transition-transform duration-200"
							class:rotate-180={logExpanded}
						>&darr;</span>
					</button>

					{#if logExpanded}
						<div class="max-h-48 overflow-y-auto px-4 pb-4">
							<div class="flex flex-col gap-1">
								{#each [...rows].reverse() as row}
									<div class="flex items-center gap-4 px-3 py-1.5 rounded-lg bg-surface-900/50 text-[11px]">
										<span class="text-surface-500 w-12 tabular-nums">Gen {row.gen}</span>
										<span class="tabular-nums" style:color={COLORS.fitness}>
											{row.best.toFixed(1)}
										</span>
										<span class="tabular-nums" style:color={COLORS.avgFitness}>
											{row.avg.toFixed(1)}
										</span>
										<span class="tabular-nums" style:color={COLORS.worstFitness}>
											{row.worst.toFixed(1)}
										</span>
									</div>
								{/each}
							</div>
						</div>
					{/if}
				</div>
			{:else}
				<div class="rounded-xl bg-surface-800/80 border border-surface-600/50 p-4">
					<h3 class="text-xs font-semibold text-surface-300 mb-3 uppercase tracking-wider">
						Fitness Over Generations
					</h3>
					<p class="text-[11px] text-surface-500 italic text-center py-8">
						Waiting for first generation...
					</p>
				</div>
			{/if}

			<!-- Champion Card (when completed) -->
			{#if status === 'completed'}
				<GladiatorChampionCard {stableId} {champions} {stable} {onTestDuel} {onContinueTraining} />
			{/if}
		</div>
	</div>
</div>
