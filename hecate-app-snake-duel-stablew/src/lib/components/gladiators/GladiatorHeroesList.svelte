<script lang="ts">
	import { onMount } from 'svelte';
	import { COLORS } from '../../game/gladiator-constants.js';
	import { fetchHeroes, startHeroDuel } from '../../game/client.js';
	import type { Hero } from '../../game/gladiator-types.js';

	interface Props {
		onBack: () => void;
		onDuel?: (matchId: string) => void;
	}

	let { onBack, onDuel }: Props = $props();

	let heroes = $state<Hero[]>([]);
	let loading = $state(true);
	let startingDuel = $state<string | null>(null);

	onMount(async () => {
		await loadHeroes();
	});

	async function loadHeroes(): Promise<void> {
		loading = true;
		try {
			heroes = await fetchHeroes();
		} catch (e) {
			console.error('[gladiators] Failed to load heroes:', e);
		}
		loading = false;
	}

	async function handleDuel(hero: Hero): Promise<void> {
		startingDuel = hero.hero_id;
		try {
			const matchId = await startHeroDuel(hero.hero_id, 50, 100);
			onDuel?.(matchId);
		} catch (e) {
			console.error('[gladiators] Failed to start hero duel:', e);
		}
		startingDuel = null;
	}

	function formatTimeAgo(ts: number): string {
		const diff = Date.now() - ts;
		if (diff < 60_000) return 'just now';
		if (diff < 3_600_000) return `${Math.floor(diff / 60_000)}m ago`;
		if (diff < 86_400_000) return `${Math.floor(diff / 3_600_000)}h ago`;
		return `${Math.floor(diff / 86_400_000)}d ago`;
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
			&larr; Stables
		</button>

		<h2
			class="text-sm font-bold tracking-wide"
			style="background: linear-gradient(135deg, #f59e0b, #ef4444); -webkit-background-clip: text; -webkit-text-fill-color: transparent; background-clip: text;"
		>
			Heroes
		</h2>

		<div class="w-12"></div>
	</div>

	<!-- Body -->
	<div class="flex-1 overflow-y-auto">
		<div class="max-w-xl mx-auto px-4 py-6 flex flex-col gap-5">
			{#if loading}
				<p class="text-[11px] text-surface-500 italic text-center py-8">Loading heroes...</p>
			{:else if heroes.length === 0}
				<div class="rounded-xl bg-surface-800/80 border border-surface-600/50 p-8 text-center">
					<p class="text-sm text-surface-400 mb-2">No heroes yet</p>
					<p class="text-[11px] text-surface-500">
						Complete a training stable and promote the champion to create a hero.
					</p>
				</div>
			{:else}
				<div class="flex flex-col gap-3">
					{#each heroes as hero}
						<div class="rounded-xl bg-surface-800/80 border border-surface-600/50 p-4">
							<div class="flex items-center justify-between mb-3">
								<div>
									<h3 class="text-sm font-bold text-amber-300">{hero.name}</h3>
									<p class="text-[10px] text-surface-500">
										Promoted {formatTimeAgo(hero.promoted_at)}
									</p>
								</div>
								<button
									onclick={() => handleDuel(hero)}
									disabled={startingDuel === hero.hero_id}
									class="px-3 py-1.5 rounded-lg text-[11px] font-semibold transition-all duration-200
										bg-gradient-to-r from-amber-600 to-red-600
										hover:from-amber-500 hover:to-red-500
										text-white shadow-lg shadow-amber-900/30
										disabled:opacity-50 disabled:cursor-not-allowed"
								>
									{startingDuel === hero.hero_id ? 'Starting...' : 'Duel vs AI'}
								</button>
							</div>

							<div class="grid grid-cols-4 gap-2">
								<div class="rounded-lg bg-surface-900/50 px-3 py-2 text-center">
									<div class="text-lg font-bold tabular-nums" style:color={COLORS.fitness}>
										{hero.fitness.toFixed(1)}
									</div>
									<div class="text-[9px] text-surface-500 uppercase tracking-wider">Fitness</div>
								</div>
								<div class="rounded-lg bg-surface-900/50 px-3 py-2 text-center">
									<div class="text-lg font-bold text-surface-100 tabular-nums">
										{hero.generation}
									</div>
									<div class="text-[9px] text-surface-500 uppercase tracking-wider">Gen</div>
								</div>
								<div class="rounded-lg bg-surface-900/50 px-3 py-2 text-center">
									<div class="text-lg font-bold tabular-nums" style:color={COLORS.completed}>
										{hero.wins}
									</div>
									<div class="text-[9px] text-surface-500 uppercase tracking-wider">Wins</div>
								</div>
								<div class="rounded-lg bg-surface-900/50 px-3 py-2 text-center">
									<div class="text-lg font-bold tabular-nums" style:color={COLORS.halted}>
										{hero.losses}
									</div>
									<div class="text-[9px] text-surface-500 uppercase tracking-wider">Losses</div>
								</div>
							</div>

							<div class="flex items-center gap-3 mt-2 text-[10px] text-surface-500">
								<span>Origin: <span class="text-surface-400 font-mono">{hero.origin_stable_id}</span></span>
								<span class="ml-auto">D:{hero.draws}</span>
							</div>
						</div>
					{/each}
				</div>
			{/if}
		</div>
	</div>
</div>
