// API client for snake gladiators training.
// Request-response via PluginApi, SSE via connectPluginSse.

import { connectPluginSse, type SseStream } from '../stream.js';
import type { Stable, Champion, GenerationStats, TrainingProgress, Hero, FitnessWeights, BatchTestResult } from './gladiator-types.js';

interface PluginApi {
	get: <T>(path: string) => Promise<T>;
	post: <T>(path: string, body: unknown) => Promise<T>;
}

let pluginApi: PluginApi | null = null;

export function setApi(api: PluginApi) {
	pluginApi = api;
}

interface InitiateStableResponse {
	ok: boolean;
	stable_id: string;
}

/** List all training stables. */
export async function fetchStables(): Promise<Stable[]> {
	const resp = await pluginApi!.get<{ ok: boolean; stables: Stable[] }>(
		'/api/arcade/gladiators/stables'
	);
	return resp.stables ?? [];
}

/** Get a single stable by ID. */
export async function fetchStable(stableId: string): Promise<Stable> {
	return pluginApi!.get<Stable>(`/api/arcade/gladiators/stables/${stableId}`);
}

/** Get the rank-1 champion from a completed stable. */
export async function fetchChampion(stableId: string): Promise<Champion> {
	return pluginApi!.get<Champion>(`/api/arcade/gladiators/stables/${stableId}/champion`);
}

/** Get all ranked champions from a completed stable. */
export async function fetchChampions(stableId: string): Promise<Champion[]> {
	const resp = await pluginApi!.get<{ ok: boolean; champions: Champion[] }>(
		`/api/arcade/gladiators/stables/${stableId}/champions`
	);
	return resp.champions ?? [];
}

/** Get generation-by-generation stats for a stable. */
export async function fetchGenerations(stableId: string): Promise<GenerationStats[]> {
	const resp = await pluginApi!.get<{ ok: boolean; generations: GenerationStats[] }>(
		`/api/arcade/gladiators/stables/${stableId}/generations`
	);
	return resp.generations ?? [];
}

/** Start a new training stable. Returns the stable_id. */
export async function initiateStable(config: {
	population_size: number;
	max_generations: number;
	opponent_af: number;
	episodes_per_eval: number;
	champion_count?: number;
	seed_stable_id?: string;
	training_config?: {
		fitness_weights?: FitnessWeights;
		fitness_preset?: string;
		enable_ltc?: boolean;
		enable_lc_chain?: boolean;
	};
}): Promise<string> {
	const resp = await pluginApi!.post<InitiateStableResponse>(
		'/api/arcade/gladiators/stables',
		config
	);
	return resp.stable_id;
}

/** Start a champion duel match. Returns the match_id for SSE streaming. */
export async function startChampionDuel(
	stableId: string,
	opponentAf: number,
	tickMs: number,
	rank: number = 1
): Promise<string> {
	const resp = await pluginApi!.post<{ ok: boolean; match_id: string }>(
		`/api/arcade/gladiators/stables/${stableId}/duel`,
		{ opponent_af: opponentAf, tick_ms: tickMs, rank }
	);
	return resp.match_id;
}

/** Run N headless duels and return aggregated stats. */
export async function batchTestChampion(
	stableId: string,
	rank: number,
	opponentAf: number,
	numDuels: number
): Promise<BatchTestResult> {
	const resp = await pluginApi!.post<{ ok: boolean; results: BatchTestResult }>(
		`/api/arcade/gladiators/stables/${stableId}/batch-test`,
		{ rank, opponent_af: opponentAf, num_duels: numDuels }
	);
	return resp.results;
}

/** Halt an active training run. */
export async function haltTraining(stableId: string): Promise<void> {
	await pluginApi!.post<{ ok: boolean }>(`/api/arcade/gladiators/stables/${stableId}/halt`, {});
}

/** Export the champion from a completed stable. */
export async function exportChampion(stableId: string): Promise<Champion> {
	return pluginApi!.post<Champion>(`/api/arcade/gladiators/stables/${stableId}/export`, {});
}

// ─── Heroes API ──────────────────────────────────────────────────

/** List all heroes. */
export async function fetchHeroes(): Promise<Hero[]> {
	const resp = await pluginApi!.get<{ ok: boolean; heroes: Hero[] }>(
		'/api/arcade/gladiators/heroes'
	);
	return resp.heroes ?? [];
}

/** Get a single hero by ID. */
export async function fetchHero(heroId: string): Promise<Hero> {
	return pluginApi!.get<Hero>(`/api/arcade/gladiators/heroes/${heroId}`);
}

/** Promote a champion to a permanent hero. */
export async function promoteChampion(
	stableId: string,
	name: string
): Promise<Hero> {
	return pluginApi!.post<Hero>('/api/arcade/gladiators/heroes', {
		stable_id: stableId,
		name
	});
}

/** Start a hero duel vs AI. Returns match_id for SSE streaming. */
export async function startHeroDuel(
	heroId: string,
	opponentAf: number,
	tickMs: number
): Promise<string> {
	const resp = await pluginApi!.post<{ ok: boolean; match_id: string }>(
		`/api/arcade/gladiators/heroes/${heroId}`,
		{ opponent_af: opponentAf, tick_ms: tickMs }
	);
	return resp.match_id;
}

// ─── SSE Streams ─────────────────────────────────────────────────

/** Connect to the SSE stream for live training progress via plugin SSE.
 *  Returns a cleanup function that stops the stream.
 */
export async function connectTrainingStream(
	stableId: string,
	onProgress: (data: TrainingProgress) => void,
	onDone?: () => void,
	onError?: (error: string) => void
): Promise<() => void> {
	const stream = connectPluginSse<TrainingProgress>(
		'snake-duel-stable',
		`/api/arcade/gladiators/stables/${stableId}/stream`,
		(data) => { onProgress(data); },
		onDone,
		onError
	);
	return () => stream.stop();
}
