<script lang="ts">
	import { onDestroy } from 'svelte';
	import { GRID_WIDTH, GRID_HEIGHT } from '../../game/snake-constants.js';
	import { drawFrame, drawCountdown, drawGameOver } from '../../game/snake-renderer.js';
	import { connectDuelStream } from '../../game/duel-client.js';
	import { startChampionDuel } from '../../game/client.js';
	import { COLORS } from '../../game/gladiator-constants.js';
	import type { GameState } from '../../game/snake-types.js';

	interface Props {
		stableId: string;
		rank?: number;
		onBack: () => void;
	}

	let { stableId, rank = 1, onBack }: Props = $props();

	type Phase = 'configure' | 'playing' | 'result';

	let phase = $state<Phase>('configure');
	let opponentAf = $state(50);
	let tickMs = $state(100);
	let starting = $state(false);
	let error = $state<string | null>(null);

	let gameState = $state<GameState | null>(null);
	let disconnectStream: (() => void) | null = null;

	let wrapper: HTMLDivElement;
	let canvas: HTMLCanvasElement;
	let ctx: CanvasRenderingContext2D | null = null;
	let resizeObserver: ResizeObserver | null = null;

	function personalityLabel(af: number): string {
		if (af < 20) return 'Gentleman';
		if (af < 40) return 'Chill';
		if (af < 60) return 'Competitive';
		if (af < 80) return 'Aggressive';
		return 'Total Jerk';
	}

	async function handleStartDuel(): Promise<void> {
		starting = true;
		error = null;
		try {
			const matchId = await startChampionDuel(stableId, opponentAf, tickMs, rank);
			phase = 'playing';

			// Wait for canvas to mount before connecting stream
			await new Promise((r) => requestAnimationFrame(r));
			initCanvas();

			disconnectStream = await connectDuelStream(
				matchId,
				(state) => {
					gameState = state;
					render();
					if (state.status === 'finished') {
						phase = 'result';
					}
				},
				() => {
					render();
				},
				(err) => {
					console.warn('[gladiator-duel] stream error:', err);
				}
			);
		} catch (e) {
			error = e instanceof Error ? e.message : String(e);
			phase = 'configure';
		}
		starting = false;
	}

	function handlePlayAgain(): void {
		cleanup();
		gameState = null;
		phase = 'configure';
	}

	function cleanup(): void {
		if (disconnectStream) {
			disconnectStream();
			disconnectStream = null;
		}
	}

	function initCanvas(): void {
		if (!canvas) return;
		ctx = canvas.getContext('2d');
		sizeCanvas();
		resizeObserver?.disconnect();
		if (wrapper) {
			resizeObserver = new ResizeObserver(() => sizeCanvas());
			resizeObserver.observe(wrapper);
		}
	}

	function sizeCanvas(): void {
		if (!wrapper || !canvas) return;
		const rect = wrapper.getBoundingClientRect();
		const aspect = GRID_WIDTH / GRID_HEIGHT;
		const maxW = 720;
		const maxH = 576;
		let w = Math.min(rect.width, maxW);
		let h = w / aspect;
		if (h > Math.min(rect.height, maxH)) {
			h = Math.min(rect.height, maxH);
			w = h * aspect;
		}
		canvas.width = Math.floor(w);
		canvas.height = Math.floor(h);
		render();
	}

	function getEndReason(): string | undefined {
		if (!gameState || gameState.status !== 'finished') return undefined;
		const s1Collision = gameState.snake1.events.find((e) => e.type === 'collision');
		const s2Collision = gameState.snake2.events.find((e) => e.type === 'collision');
		if (gameState.winner === 'player1' && s2Collision) return `AI: ${s2Collision.value}`;
		if (gameState.winner === 'player2' && s1Collision)
			return `Champion: ${s1Collision.value}`;
		if (s1Collision) return s1Collision.value;
		return s2Collision?.value;
	}

	function render(): void {
		if (!ctx || !gameState) return;
		drawFrame(ctx, gameState);
		if (gameState.status === 'countdown') {
			drawCountdown(ctx, gameState.countdown);
		} else if (gameState.status === 'finished') {
			drawGameOver(ctx, gameState.winner, getEndReason());
		}
	}

	const winnerLabel = $derived(
		gameState?.winner === 'player1'
			? 'Champion Wins!'
			: gameState?.winner === 'player2'
				? 'AI Wins!'
				: "It's a Draw!"
	);

	onDestroy(() => {
		cleanup();
		resizeObserver?.disconnect();
	});
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
			Champion Duel
		</h2>

		<div class="w-12"></div>
	</div>

	<!-- Body -->
	<div class="flex-1 overflow-y-auto flex flex-col">
		{#if phase === 'configure'}
			<div class="max-w-md mx-auto px-4 py-6 flex flex-col gap-5 w-full">
				<div class="rounded-xl bg-surface-800/80 border border-surface-600/50 p-5">
					<div class="mb-4">
						<h3 class="text-sm font-semibold text-surface-100">Test Champion</h3>
						<p class="text-[10px] text-surface-400 mt-0.5">
							Watch your trained champion play against a heuristic AI
						</p>
					</div>

					{#if error}
						<p class="text-[11px] text-red-400 mb-3">{error}</p>
					{/if}

					<div class="flex flex-col gap-2.5 mb-4">
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
							<span class="w-24">Tick Speed</span>
							<input
								type="range"
								min="50"
								max="200"
								step="10"
								bind:value={tickMs}
								class="flex-1 accent-purple-500"
							/>
							<span class="text-surface-500 w-14 text-right tabular-nums">{tickMs}ms</span>
						</label>
					</div>

					<button
						onclick={handleStartDuel}
						disabled={starting}
						class="w-full px-4 py-2.5 rounded-lg text-xs font-semibold transition-all duration-200
							bg-gradient-to-r from-purple-600 to-amber-600
							hover:from-purple-500 hover:to-amber-500
							text-white shadow-lg shadow-purple-900/30
							disabled:opacity-50 disabled:cursor-not-allowed"
					>
						{starting ? 'Starting...' : 'Start Duel'}
					</button>
				</div>
			</div>
		{:else}
			<!-- Playing / Result phase -->
			<div class="flex-1 flex flex-col min-h-0 px-4 py-3 gap-3">
				<!-- Player labels + scores -->
				<div class="flex items-center justify-between text-[11px] shrink-0">
					<div class="flex items-center gap-2">
						<span
							class="px-2 py-0.5 rounded text-[10px] font-bold"
							style="background: rgba(96, 165, 250, 0.2); color: #60a5fa;"
						>
							Champion (NN)
						</span>
						<span class="text-surface-300 tabular-nums">
							{gameState?.snake1.score ?? 0}
						</span>
					</div>
					<div class="text-surface-500 tabular-nums">
						Tick {gameState?.tick ?? 0}
					</div>
					<div class="flex items-center gap-2">
						<span class="text-surface-300 tabular-nums">
							{gameState?.snake2.score ?? 0}
						</span>
						<span
							class="px-2 py-0.5 rounded text-[10px] font-bold"
							style="background: rgba(239, 68, 68, 0.2); color: #ef4444;"
						>
							AI (AF: {opponentAf})
						</span>
					</div>
				</div>

				<!-- Canvas -->
				<div bind:this={wrapper} class="flex-1 min-h-0 min-w-0 flex items-center justify-center">
					<canvas
						bind:this={canvas}
						class="rounded-lg border border-surface-600/30"
						style="image-rendering: pixelated;"
					></canvas>
				</div>

				<!-- Result actions -->
				{#if phase === 'result'}
					<div class="flex items-center justify-center gap-3 shrink-0 py-2">
						<span class="text-sm font-bold" style:color={
							gameState?.winner === 'player1' ? '#60a5fa' :
							gameState?.winner === 'player2' ? '#ef4444' : '#ffffff'
						}>
							{winnerLabel}
						</span>
						<button
							onclick={handlePlayAgain}
							class="px-4 py-1.5 rounded-lg text-[11px] font-semibold transition-all duration-200
								bg-gradient-to-r from-purple-600 to-amber-600
								hover:from-purple-500 hover:to-amber-500
								text-white shadow-lg shadow-purple-900/30"
						>
							Play Again
						</button>
						<button
							onclick={onBack}
							class="px-4 py-1.5 rounded-lg text-[11px] font-semibold transition-all duration-200
								bg-surface-700 hover:bg-surface-600 text-surface-200"
						>
							Back
						</button>
					</div>
				{/if}
			</div>
		{/if}
	</div>
</div>
