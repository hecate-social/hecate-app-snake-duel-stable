<script lang="ts">
	import { COLORS, DEFAULT_FITNESS_WEIGHTS, WEIGHT_BOUNDS } from '../../game/gladiator-constants.js';
	import { exportChampion, fetchChampions, promoteChampion, batchTestChampion } from '../../game/client.js';
	import type { Champion, Stable, FitnessWeights, BatchTestResult } from '../../game/gladiator-types.js';

	interface Props {
		stableId: string;
		champions: Champion[];
		stable?: Stable | null;
		onTestDuel?: (stableId: string, rank?: number) => void;
		onContinueTraining?: (stableId: string) => void;
		onPromoted?: () => void;
	}

	let { stableId, champions = $bindable(), stable, onTestDuel, onContinueTraining, onPromoted }: Props = $props();

	let selectedRank = $state(1);
	let exporting = $state(false);
	let exported = $state(false);
	let error = $state<string | null>(null);
	let loading = $state(false);
	let promoting = $state(false);
	let promoted = $state(false);
	let heroName = $state('');
	let showPromoteForm = $state(false);

	// Batch test state
	let batchTesting = $state(false);
	let batchResult = $state<BatchTestResult | null>(null);
	let batchDuels = $state(20);
	let batchOpponentAf = $state(50);
	let prevSelectedRank = $state(1);

	const selectedChampion = $derived(
		champions.find((c) => c.rank === selectedRank) ?? champions[0] ?? null
	);

	async function handleExport(): Promise<void> {
		exporting = true;
		error = null;
		try {
			await exportChampion(stableId);
			exported = true;
		} catch (e) {
			error = e instanceof Error ? e.message : String(e);
		}
		exporting = false;
	}

	async function handleRetry(): Promise<void> {
		loading = true;
		error = null;
		try {
			champions = await fetchChampions(stableId);
		} catch (e) {
			error = 'Champions not found yet. Training may still be finalizing.';
		}
		loading = false;
	}

	async function handlePromote(): Promise<void> {
		if (!heroName.trim()) return;
		promoting = true;
		error = null;
		try {
			await promoteChampion(stableId, heroName.trim());
			promoted = true;
			showPromoteForm = false;
			onPromoted?.();
		} catch (e) {
			error = e instanceof Error ? e.message : String(e);
		}
		promoting = false;
	}

	async function handleBatchTest(): Promise<void> {
		batchTesting = true;
		error = null;
		batchResult = null;
		try {
			batchResult = await batchTestChampion(stableId, selectedRank, batchOpponentAf, batchDuels);
		} catch (e) {
			error = e instanceof Error ? e.message : String(e);
		}
		batchTesting = false;
	}

	// Clear batch results when rank changes
	$effect(() => {
		if (selectedRank !== prevSelectedRank) {
			batchResult = null;
			prevSelectedRank = selectedRank;
		}
	});

	const batchWinRateColor = $derived(
		batchResult
			? batchResult.win_rate >= 60 ? '#22c55e'
			  : batchResult.win_rate >= 40 ? '#eab308'
			  : '#ef4444'
			: '#6b7280'
	);

	const winRate = $derived(
		selectedChampion && selectedChampion.wins + selectedChampion.losses + selectedChampion.draws > 0
			? Math.round((selectedChampion.wins / (selectedChampion.wins + selectedChampion.losses + selectedChampion.draws)) * 100)
			: 0
	);

	// --- Radar chart: fitness weight profile ---
	const RADAR_AXES: { key: keyof FitnessWeights; label: string }[] = [
		{ key: 'survival_weight', label: 'Survival' },
		{ key: 'food_weight', label: 'Foraging' },
		{ key: 'win_bonus', label: 'Aggression' },
		{ key: 'kill_bonus', label: 'Lethality' },
		{ key: 'proximity_weight', label: 'Tracking' },
		{ key: 'circle_penalty', label: 'Exploration' },
	];

	const RADAR_CX = 80;
	const RADAR_CY = 80;
	const RADAR_R = 55;

	function radarAngle(i: number): number {
		return (i * 2 * Math.PI / RADAR_AXES.length) - Math.PI / 2;
	}

	const radarPoints = $derived.by(() => {
		const weights = stable?.fitness_weights ?? DEFAULT_FITNESS_WEIGHTS;
		return RADAR_AXES.map(({ key }, i) => {
			const bounds = WEIGHT_BOUNDS[key];
			const raw = Math.abs(weights[key]);
			const norm = Math.min(1, Math.max(0, (raw - bounds.min) / (bounds.max - bounds.min)));
			const angle = radarAngle(i);
			return `${RADAR_CX + RADAR_R * norm * Math.cos(angle)},${RADAR_CY + RADAR_R * norm * Math.sin(angle)}`;
		}).join(' ');
	});

	// --- Network topology diagram ---
	const topology = $derived.by(() => {
		if (!selectedChampion?.network_json) return null;
		try {
			const net = JSON.parse(selectedChampion.network_json);
			const layers = net.layers as { weights: number[][]; biases: number[] }[];
			if (!layers?.length || !layers[0]?.weights?.[0]) return null;
			const inputSize = layers[0].weights[0].length;
			const layerSizes = layers.map((l: { biases: number[] }) => l.biases.length);
			const version = (net.version ?? 1) as number;
			return {
				sizes: [inputSize, ...layerSizes] as number[],
				activation: (net.activation ?? 'tanh') as string,
				version,
			};
		} catch { return null; }
	});

	const isLtcNetwork = $derived(topology !== null && topology.version >= 3);

	const TOPO_W = 240;
	const TOPO_H = 110;
	const TOPO_PAD_X = 30;
	const TOPO_PAD_Y = 14;
	const TOPO_MAX_DISPLAY = 7;
	const OUTPUT_LABELS = ['U', 'D', 'L', 'R', 'Drop'];

	interface TopoLayer {
		x: number;
		neurons: { y: number }[];
		size: number;
		label: string;
		showEllipsis: boolean;
		isOutput: boolean;
	}

	const topoLayers = $derived.by((): TopoLayer[] | null => {
		if (!topology) return null;
		const { sizes } = topology;
		const n = sizes.length;
		const availH = TOPO_H - TOPO_PAD_Y * 2;

		return sizes.map((size, i): TopoLayer => {
			const x = n === 1 ? TOPO_W / 2 : TOPO_PAD_X + (i * (TOPO_W - TOPO_PAD_X * 2)) / (n - 1);
			const displayed = Math.min(size, TOPO_MAX_DISPLAY);
			const gap = displayed > 1 ? availH / (displayed - 1) : 0;
			const startY = displayed > 1 ? TOPO_PAD_Y : TOPO_H / 2;
			const neurons = Array.from({ length: displayed }, (_, j) => ({
				y: startY + j * gap,
			}));
			return {
				x,
				neurons,
				size,
				label: i === 0 ? 'In' : i === n - 1 ? 'Out' : `H${i}`,
				showEllipsis: size > TOPO_MAX_DISPLAY,
				isOutput: i === n - 1,
			};
		});
	});
</script>

{#if champions.length > 0 && selectedChampion}
	<div class="rounded-xl bg-surface-800/80 border border-surface-600/50 p-5">
		<div class="flex items-center justify-between mb-4">
			<div>
				<h3 class="text-sm font-semibold text-surface-100">
					Champions
					{#if champions.length > 1}
						<span class="text-surface-500 font-normal">({champions.length})</span>
					{/if}
				</h3>
				<p class="text-[10px] text-surface-400 mt-0.5">
					Top neural networks from this stable
				</p>
			</div>
			<div class="flex items-center gap-2">
				{#if isLtcNetwork}
					<span class="text-[9px] px-1.5 py-0.5 rounded bg-cyan-900/40 text-cyan-300 font-semibold border border-cyan-700/30">
						LTC
					</span>
				{/if}
				{#if !exported}
					<button
						onclick={handleExport}
						disabled={exporting}
						class="px-3 py-1.5 rounded-lg text-[11px] font-semibold transition-all duration-200
							bg-gradient-to-r from-purple-600 to-amber-600
							hover:from-purple-500 hover:to-amber-500
							text-white shadow-lg shadow-purple-900/30
							disabled:opacity-50 disabled:cursor-not-allowed"
					>
						{exporting ? 'Exporting...' : 'Export Champion'}
					</button>
				{:else}
					<span class="text-[11px] font-semibold" style:color={COLORS.completed}>Exported</span>
				{/if}
			</div>
		</div>

		{#if error}
			<p class="text-[11px] text-red-400 mb-3">{error}</p>
		{/if}

		<!-- Champion Rank Tabs (only if multiple) -->
		{#if champions.length > 1}
			<div class="flex gap-1 mb-3 overflow-x-auto">
				{#each champions as champ}
					<button
						onclick={() => { selectedRank = champ.rank; }}
						class="shrink-0 px-2.5 py-1.5 rounded-lg text-[10px] font-semibold transition-all duration-200 border
							{selectedRank === champ.rank
								? 'bg-purple-600/30 border-purple-500/50 text-purple-200'
								: 'bg-surface-900/50 border-surface-700/50 text-surface-400 hover:text-surface-200 hover:border-surface-600'}"
					>
						<span class="tabular-nums">#{champ.rank}</span>
						<span class="ml-1 tabular-nums" style:color={COLORS.fitness}>{champ.fitness.toFixed(0)}</span>
					</button>
				{/each}
			</div>
		{/if}

		<!-- Selected Champion Stats -->
		<div class="grid grid-cols-4 gap-2">
			<div class="rounded-lg bg-surface-900/50 px-3 py-2 text-center">
				<div class="text-lg font-bold tabular-nums" style:color={COLORS.fitness}>
					{selectedChampion.fitness.toFixed(1)}
				</div>
				<div class="text-[9px] text-surface-500 uppercase tracking-wider">Fitness</div>
			</div>
			<div class="rounded-lg bg-surface-900/50 px-3 py-2 text-center">
				<div class="text-lg font-bold text-surface-100 tabular-nums">
					{selectedChampion.generation}
				</div>
				<div class="text-[9px] text-surface-500 uppercase tracking-wider">Generation</div>
			</div>
			<div class="rounded-lg bg-surface-900/50 px-3 py-2 text-center">
				<div class="text-lg font-bold tabular-nums" style:color={COLORS.completed}>
					{selectedChampion.wins}
				</div>
				<div class="text-[9px] text-surface-500 uppercase tracking-wider">Wins</div>
			</div>
			<div class="rounded-lg bg-surface-900/50 px-3 py-2 text-center">
				<div class="text-lg font-bold text-surface-100 tabular-nums">
					{winRate}%
				</div>
				<div class="text-[9px] text-surface-500 uppercase tracking-wider">Win Rate</div>
			</div>
		</div>

		<div class="flex items-center gap-3 mt-3 text-[11px] text-surface-400">
			<span>W: <span class="text-surface-200 tabular-nums">{selectedChampion.wins}</span></span>
			<span>L: <span class="text-surface-200 tabular-nums">{selectedChampion.losses}</span></span>
			<span>D: <span class="text-surface-200 tabular-nums">{selectedChampion.draws}</span></span>
		</div>

		<!-- Visualizations: Radar Chart + Network Topology -->
		<div class="grid grid-cols-2 gap-3 mt-3">
			<!-- Radar Chart: Training Profile -->
			<div class="rounded-lg bg-surface-900/50 p-3">
				<div class="text-[9px] text-surface-500 uppercase tracking-wider mb-1">Training Profile</div>
				<svg viewBox="0 0 160 160" class="w-full" preserveAspectRatio="xMidYMid meet">
					<!-- Grid polygons at 25/50/75/100% -->
					{#each [0.25, 0.5, 0.75, 1] as scale}
						<polygon
							points={Array.from({ length: RADAR_AXES.length }, (_, i) => {
								const a = radarAngle(i);
								return `${RADAR_CX + RADAR_R * scale * Math.cos(a)},${RADAR_CY + RADAR_R * scale * Math.sin(a)}`;
							}).join(' ')}
							fill="none"
							stroke="#2a2a50"
							stroke-width="0.5"
						/>
					{/each}

					<!-- Axis spokes -->
					{#each RADAR_AXES as _, i}
						{@const a = radarAngle(i)}
						<line
							x1={RADAR_CX} y1={RADAR_CY}
							x2={RADAR_CX + RADAR_R * Math.cos(a)} y2={RADAR_CY + RADAR_R * Math.sin(a)}
							stroke="#2a2a50" stroke-width="0.5"
						/>
					{/each}

					<!-- Data polygon -->
					<polygon
						points={radarPoints}
						fill="rgba(167, 139, 250, 0.15)"
						stroke="#a78bfa"
						stroke-width="1.5"
					/>

					<!-- Axis labels -->
					{#each RADAR_AXES as axis, i}
						{@const a = radarAngle(i)}
						{@const lx = RADAR_CX + (RADAR_R + 15) * Math.cos(a)}
						{@const ly = RADAR_CY + (RADAR_R + 15) * Math.sin(a)}
						<text
							x={lx} y={ly}
							fill="#6b7280" font-size="7"
							text-anchor="middle" dominant-baseline="middle"
						>{axis.label}</text>
					{/each}
				</svg>
			</div>

			<!-- Network Topology -->
			<div class="rounded-lg bg-surface-900/50 p-3">
				<div class="text-[9px] text-surface-500 uppercase tracking-wider mb-1">
					Network
					{#if topology}
						<span class="normal-case text-surface-600 ml-1">{topology.sizes.join('\u2192')}</span>
					{/if}
					{#if isLtcNetwork}
						<span class="ml-1 text-cyan-400">LTC</span>
					{/if}
				</div>
				{#if topoLayers}
					<svg viewBox="0 0 {TOPO_W} {TOPO_H}" class="w-full" preserveAspectRatio="xMidYMid meet">
						<!-- Connection lines (draw first, behind neurons) -->
						{#each topoLayers.slice(0, -1) as layer, li}
							{@const nextLayer = topoLayers[li + 1]}
							{#each layer.neurons as src}
								{#each nextLayer.neurons as dst}
									<line
										x1={layer.x} y1={src.y}
										x2={nextLayer.x} y2={dst.y}
										stroke="#4a4a6a" stroke-width="0.4" opacity="0.12"
									/>
								{/each}
							{/each}
						{/each}

						<!-- Neurons and labels -->
						{#each topoLayers as layer, li}
							<!-- Neuron circles -->
							{#each layer.neurons as neuron, ni}
								<circle
									cx={layer.x} cy={neuron.y} r="3"
									fill={layer.isOutput ? '#fbbf24' : isLtcNetwork && li > 0 && !layer.isOutput ? '#22d3ee' : '#a78bfa'}
									opacity={layer.isOutput ? 0.9 : 0.7}
								/>
								<!-- Output neuron action labels -->
								{#if layer.isOutput && layer.size <= OUTPUT_LABELS.length}
									<text
										x={layer.x + 7} y={neuron.y}
										fill="#9ca3af" font-size="6"
										dominant-baseline="middle"
									>{OUTPUT_LABELS[ni]}</text>
								{/if}
							{/each}

							<!-- Ellipsis indicator for condensed layers -->
							{#if layer.showEllipsis}
								{@const lastNeuron = layer.neurons[layer.neurons.length - 1]}
								<text
									x={layer.x} y={lastNeuron.y + 10}
									fill="#6b7280" font-size="7"
									text-anchor="middle"
								>...</text>
							{/if}

							<!-- Layer label + size -->
							<text
								x={layer.x} y={TOPO_H - 1}
								fill="#6b7280" font-size="7"
								text-anchor="middle"
							>{layer.label} ({layer.size})</text>
						{/each}
					</svg>
				{:else}
					<div class="flex items-center justify-center h-20">
						<span class="text-[10px] text-surface-600 italic">No network data</span>
					</div>
				{/if}
			</div>
		</div>

		<!-- Action buttons -->
		<div class="flex items-center gap-2 mt-4">
			{#if onTestDuel}
				<button
					onclick={() => onTestDuel?.(stableId, selectedRank)}
					class="flex-1 px-3 py-2 rounded-lg text-[11px] font-semibold transition-all duration-200
						bg-blue-600/20 hover:bg-blue-600/30 text-blue-400 border border-blue-500/30
						hover:border-blue-500/50"
				>
					Test #{selectedRank} in Duel
				</button>
			{/if}
			<button
				onclick={handleBatchTest}
				disabled={batchTesting}
				class="flex-1 px-3 py-2 rounded-lg text-[11px] font-semibold transition-all duration-200
					bg-emerald-600/20 hover:bg-emerald-600/30 text-emerald-400 border border-emerald-500/30
					hover:border-emerald-500/50
					disabled:opacity-50 disabled:cursor-not-allowed"
			>
				{batchTesting ? `Testing (${batchDuels})...` : `Batch Test #${selectedRank}`}
			</button>
			{#if onContinueTraining}
				<button
					onclick={() => onContinueTraining?.(stableId)}
					class="flex-1 px-3 py-2 rounded-lg text-[11px] font-semibold transition-all duration-200
						bg-purple-600/20 hover:bg-purple-600/30 text-purple-400 border border-purple-500/30
						hover:border-purple-500/50"
				>
					Continue Training
				</button>
			{/if}
			{#if !promoted}
				<button
					onclick={() => { showPromoteForm = !showPromoteForm; }}
					class="flex-1 px-3 py-2 rounded-lg text-[11px] font-semibold transition-all duration-200
						bg-amber-600/20 hover:bg-amber-600/30 text-amber-400 border border-amber-500/30
						hover:border-amber-500/50"
				>
					Promote to Hero
				</button>
			{:else}
				<span class="text-[11px] font-semibold text-amber-400">Promoted!</span>
			{/if}
		</div>

		<!-- Batch Test Config -->
		<div class="flex items-center gap-3 mt-2 text-[10px] text-surface-400">
			<label class="flex items-center gap-1.5">
				<span>Opponent AF:</span>
				<select
					bind:value={batchOpponentAf}
					class="bg-surface-900 border border-surface-700/50 rounded px-1.5 py-0.5 text-[10px] text-surface-200
						focus:border-emerald-500/50 focus:outline-none"
				>
					{#each [10, 20, 30, 40, 50, 60, 70, 80, 90, 100] as af}
						<option value={af}>{af}</option>
					{/each}
				</select>
			</label>
			<label class="flex items-center gap-1.5">
				<span>Duels:</span>
				<select
					bind:value={batchDuels}
					class="bg-surface-900 border border-surface-700/50 rounded px-1.5 py-0.5 text-[10px] text-surface-200
						focus:border-emerald-500/50 focus:outline-none"
				>
					{#each [10, 20, 30, 50] as n}
						<option value={n}>{n}</option>
					{/each}
				</select>
			</label>
		</div>

		<!-- Batch Test Results -->
		{#if batchResult}
			<div class="mt-3 rounded-lg bg-surface-900/60 border border-surface-700/40 p-3">
				<div class="flex items-center justify-between mb-2">
					<span class="text-[9px] text-surface-500 uppercase tracking-wider">
						Batch Test Results ({batchResult.total} duels)
					</span>
					<span class="text-[9px] text-surface-500">
						vs AF {batchOpponentAf}
					</span>
				</div>
				<div class="flex items-baseline gap-3">
					<div class="text-2xl font-bold tabular-nums" style:color={batchWinRateColor}>
						{batchResult.win_rate}%
					</div>
					<div class="flex items-center gap-2 text-[11px]">
						<span class="text-emerald-400 tabular-nums">{batchResult.wins}W</span>
						<span class="text-red-400 tabular-nums">{batchResult.losses}L</span>
						<span class="text-surface-400 tabular-nums">{batchResult.draws}D</span>
					</div>
				</div>
				<div class="flex items-center gap-4 mt-1.5 text-[10px] text-surface-500">
					<span>Avg ticks: <span class="text-surface-300 tabular-nums">{batchResult.avg_ticks}</span></span>
					<span>Avg food: <span class="text-surface-300 tabular-nums">{batchResult.avg_food}</span></span>
					<span>Wall kills: <span class="text-surface-300 tabular-nums">{batchResult.wall_kills}</span></span>
				</div>
			</div>
		{/if}

		<!-- Promote Form -->
		{#if showPromoteForm && !promoted}
			<div class="mt-3 flex items-center gap-2">
				<input
					type="text"
					bind:value={heroName}
					placeholder="Hero name..."
					maxlength="30"
					class="flex-1 px-3 py-1.5 rounded-lg text-[11px] bg-surface-900 border border-amber-500/30
						text-surface-100 placeholder:text-surface-600 focus:border-amber-500/50 focus:outline-none"
				/>
				<button
					onclick={handlePromote}
					disabled={promoting || !heroName.trim()}
					class="px-3 py-1.5 rounded-lg text-[11px] font-semibold transition-all duration-200
						bg-amber-600 hover:bg-amber-500 text-white
						disabled:opacity-50 disabled:cursor-not-allowed"
				>
					{promoting ? 'Promoting...' : 'Confirm'}
				</button>
			</div>
		{/if}
	</div>
{:else}
	<div class="rounded-xl bg-surface-800/80 border border-surface-600/50 p-5 text-center">
		<h3 class="text-sm font-semibold text-surface-100 mb-2">Champions</h3>
		{#if error}
			<p class="text-[11px] text-red-400 mb-3">{error}</p>
		{/if}
		<p class="text-[11px] text-surface-400 mb-3">Loading champion data...</p>
		<button
			onclick={handleRetry}
			disabled={loading}
			class="px-3 py-1.5 rounded-lg text-[11px] font-semibold transition-all duration-200
				bg-surface-700 hover:bg-surface-600 text-surface-200
				disabled:opacity-50 disabled:cursor-not-allowed"
		>
			{loading ? 'Loading...' : 'Retry'}
		</button>
	</div>
{/if}
