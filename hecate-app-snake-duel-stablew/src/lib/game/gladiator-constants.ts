import type { FitnessWeights } from './gladiator-types.js';

export const DEFAULTS = {
	populationSize: 50,
	maxGenerations: 100,
	opponentAf: 50,
	episodesPerEval: 5,
	championCount: 3
};

export const COLORS = {
	fitness: '#a78bfa',
	avgFitness: '#60a5fa',
	worstFitness: '#f87171',
	training: '#fbbf24',
	completed: '#22c55e',
	halted: '#ef4444',
	hero: '#f59e0b'
} as const;

export const DEFAULT_FITNESS_WEIGHTS: FitnessWeights = {
	survival_weight: 0.1,
	food_weight: 50.0,
	win_bonus: 200.0,
	draw_bonus: 50.0,
	kill_bonus: 100.0,
	proximity_weight: 0.5,
	circle_penalty: -1.0,
	wall_kill_bonus: 75.0
};

export interface WeightBound {
	min: number;
	max: number;
	step: number;
	label: string;
	invert?: boolean;
}

export const WEIGHT_BOUNDS: Record<keyof FitnessWeights, WeightBound> = {
	survival_weight:  { min: 0,    max: 1,    step: 0.05, label: 'Survival' },
	food_weight:      { min: 0,    max: 200,  step: 5,    label: 'Food' },
	win_bonus:        { min: 0,    max: 500,  step: 10,   label: 'Win Bonus' },
	draw_bonus:       { min: 0,    max: 200,  step: 5,    label: 'Draw Bonus' },
	kill_bonus:       { min: 0,    max: 300,  step: 10,   label: 'Kill Bonus' },
	proximity_weight: { min: 0,    max: 5,    step: 0.1,  label: 'Proximity' },
	circle_penalty:   { min: 0,    max: 2,    step: 0.1,  label: 'Exploration', invert: true },
	wall_kill_bonus:  { min: 0,    max: 200,  step: 5,    label: 'Wall Kill' }
};

export interface WeightGroup {
	label: string;
	keys: (keyof FitnessWeights)[];
	color: string;
}

export const WEIGHT_GROUPS: WeightGroup[] = [
	{
		label: 'Combat',
		keys: ['win_bonus', 'kill_bonus', 'draw_bonus', 'wall_kill_bonus'],
		color: '#ef4444'
	},
	{
		label: 'Survival & Foraging',
		keys: ['survival_weight', 'food_weight', 'proximity_weight', 'circle_penalty'],
		color: '#22c55e'
	}
];

export const WEIGHT_IMPACTS: Record<keyof FitnessWeights, number> = {
	win_bonus: 3.0,
	kill_bonus: 2.5,
	food_weight: 2.0,
	wall_kill_bonus: 2.0,
	draw_bonus: 1.5,
	proximity_weight: 1.0,
	survival_weight: 1.0,
	circle_penalty: 0.5
};

export const TUNING_BUDGET = 100.0;

export interface FitnessPreset {
	name: string;
	label: string;
	description: string;
	weights: FitnessWeights;
}

export const PRESETS: FitnessPreset[] = [
	{
		name: 'balanced',
		label: 'Balanced',
		description: 'Default weights — well-rounded gladiator',
		weights: { ...DEFAULT_FITNESS_WEIGHTS }
	},
	{
		name: 'aggressive',
		label: 'Aggressive',
		description: 'High kill & win bonuses — bred to dominate',
		weights: {
			survival_weight: 0.1, food_weight: 20.0, win_bonus: 400.0,
			draw_bonus: 50.0, kill_bonus: 250.0, proximity_weight: 0.5, circle_penalty: -0.5,
			wall_kill_bonus: 150.0
		}
	},
	{
		name: 'forager',
		label: 'Forager',
		description: 'High food weight — bred to eat everything',
		weights: {
			survival_weight: 0.1, food_weight: 150.0, win_bonus: 50.0,
			draw_bonus: 50.0, kill_bonus: 100.0, proximity_weight: 3.0, circle_penalty: -1.0,
			wall_kill_bonus: 25.0
		}
	},
	{
		name: 'survivor',
		label: 'Survivor',
		description: 'High survival — bred to outlast opponents',
		weights: {
			survival_weight: 0.8, food_weight: 50.0, win_bonus: 200.0,
			draw_bonus: 50.0, kill_bonus: 20.0, proximity_weight: 0.5, circle_penalty: -1.5,
			wall_kill_bonus: 50.0
		}
	},
	{
		name: 'assassin',
		label: 'Assassin',
		description: 'Max kill & win — ignores food, pure aggression',
		weights: {
			survival_weight: 0.1, food_weight: 0.0, win_bonus: 500.0,
			draw_bonus: 0.0, kill_bonus: 300.0, proximity_weight: 0.0, circle_penalty: 0.0,
			wall_kill_bonus: 200.0
		}
	},
	{
		name: 'hybrid',
		label: 'Hybrid',
		description: 'Win-focused with food awareness — best duel win rate',
		weights: {
			survival_weight: 0.1, food_weight: 15.0, win_bonus: 500.0,
			draw_bonus: 25.0, kill_bonus: 250.0, proximity_weight: 0.3, circle_penalty: -0.3,
			wall_kill_bonus: 150.0
		}
	}
];

/** Compute tuning cost for a set of weights */
export function computeTuningCost(weights: FitnessWeights): number {
	let total = 0;
	for (const key of Object.keys(DEFAULT_FITNESS_WEIGHTS) as (keyof FitnessWeights)[]) {
		const actual = weights[key];
		const def = DEFAULT_FITNESS_WEIGHTS[key];
		const { min, max } = WEIGHT_BOUNDS[key];
		const range = max - min;
		const impact = WEIGHT_IMPACTS[key];
		if (range === 0) continue;
		total += (Math.abs(actual - def) / range) * impact * 10;
	}
	return total;
}
