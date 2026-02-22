<script lang="ts">
	import GladiatorStablesList from './gladiators/GladiatorStablesList.svelte';
	import GladiatorTrainingMonitor from './gladiators/GladiatorTrainingMonitor.svelte';
	import GladiatorDuelView from './gladiators/GladiatorDuelView.svelte';
	import GladiatorHeroesList from './gladiators/GladiatorHeroesList.svelte';
	import type { Stable } from '../game/gladiator-types.js';

	let view: 'stables' | 'training' | 'duel' | 'heroes' = $state('stables');
	let selectedStableId = $state<string | null>(null);
	let duelRank = $state(1);

	function handleSelectStable(stableId: string, _status: Stable['status']): void {
		selectedStableId = stableId;
		view = 'training';
	}

	function handleTestDuel(stableId: string, rank?: number): void {
		selectedStableId = stableId;
		duelRank = rank ?? 1;
		view = 'duel';
	}

	function handleContinueTraining(stableId: string): void {
		selectedStableId = stableId;
		view = 'stables';
	}

	function handleBackToStables(): void {
		view = 'stables';
		selectedStableId = null;
	}

	function handleBackFromDuel(): void {
		view = 'training';
	}

	function handleViewHeroes(): void {
		view = 'heroes';
	}

	function handleHeroDuel(_matchId: string): void {
		// Hero duels could use the same duel view in the future
		// For now, the duel starts and plays out on the server
	}
</script>

{#if view === 'duel' && selectedStableId}
	<GladiatorDuelView
		stableId={selectedStableId}
		rank={duelRank}
		onBack={handleBackFromDuel}
	/>
{:else if view === 'training' && selectedStableId}
	<GladiatorTrainingMonitor
		stableId={selectedStableId}
		onBack={handleBackToStables}
		onTestDuel={handleTestDuel}
		onContinueTraining={handleContinueTraining}
	/>
{:else if view === 'heroes'}
	<GladiatorHeroesList
		onBack={handleBackToStables}
		onDuel={handleHeroDuel}
	/>
{:else}
	<GladiatorStablesList
		onSelectStable={handleSelectStable}
		onViewHeroes={handleViewHeroes}
		seedStableId={selectedStableId}
		onClearSeed={() => { selectedStableId = null; }}
	/>
{/if}
