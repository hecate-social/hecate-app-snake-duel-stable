// SSE client for streaming champion duel match state from hecate-daemon.
// Uses plugin SSE streaming (not direct Tauri invoke).

import { connectPluginSse } from '../stream.js';
import type { GameState, GameEvent } from './snake-types.js';

interface PluginApi {
	post: <T>(path: string, body: unknown) => Promise<T>;
}

let pluginApi: PluginApi | null = null;

export function setDuelApi(api: PluginApi) {
	pluginApi = api;
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

/** Connect to the SSE stream for a live duel match via plugin SSE.
 *  Returns a cleanup function that stops the stream.
 */
export async function connectDuelStream(
	matchId: string,
	onState: (state: GameState) => void,
	onDone?: () => void,
	onError?: (error: string) => void
): Promise<() => void> {
	const stream = connectPluginSse<Record<string, unknown>>(
		'snake-duel-stable',
		`/api/arcade/gladiators/matches/${matchId}/stream`,
		(data) => {
			try {
				const state = daemonToGameState(data);
				onState(state);
			} catch (e) {
				console.error('[gladiator-duel] Failed to parse SSE:', e);
			}
		},
		onDone,
		onError
	);
	return () => stream.stop();
}

/** Convert daemon JSON map to client GameState type.
 * Daemon uses snake_case atoms; client uses camelCase strings.
 */
function daemonToGameState(data: Record<string, unknown>): GameState {
	return {
		snake1: daemonToSnake(data.snake1 as Record<string, unknown>),
		snake2: daemonToSnake(data.snake2 as Record<string, unknown>),
		food: data.food as [number, number],
		poisonApples: (data.poison_apples as Array<Record<string, unknown>>).map((p) => ({
			pos: p.pos as [number, number],
			owner: String(p.owner) as 'player1' | 'player2'
		})),
		walls: ((data.walls as Array<Record<string, unknown>>) ?? []).map((w) => ({
			pos: w.pos as [number, number],
			owner: String(w.owner) as 'player1' | 'player2',
			ttl: w.ttl as number
		})),
		status: String(data.status) as GameState['status'],
		winner:
			data.winner === 'none' || data.winner === null
				? null
				: (String(data.winner) as 'player1' | 'player2' | 'draw'),
		tick: data.tick as number,
		countdown: data.countdown as number
	};
}

function daemonToSnake(s: Record<string, unknown>): GameState['snake1'] {
	return {
		body: (s.body as number[][]).map(([x, y]) => [x, y] as [number, number]),
		direction: String(s.direction) as 'up' | 'down' | 'left' | 'right',
		score: s.score as number,
		assholeFactor: s.asshole_factor as number,
		events: ((s.events as Array<Record<string, unknown>>) || []).map((e) => ({
			type: mapEventType(String(e.type)),
			value: String(e.value),
			tick: e.tick as number
		}))
	};
}

function mapEventType(
	t: string
): GameEvent['type'] {
	const mapping: Record<string, string> = {
		food: 'food',
		turn: 'turn',
		collision: 'collision',
		win: 'win',
		poison_drop: 'poison-drop',
		poison_eat: 'poison-eat',
		wall_drop: 'wall-drop',
		wall_hit: 'wall-hit'
	};
	return (mapping[t] || t) as GameEvent['type'];
}
