export interface FitnessWeights {
	survival_weight: number;
	food_weight: number;
	win_bonus: number;
	draw_bonus: number;
	kill_bonus: number;
	proximity_weight: number;
	circle_penalty: number;
	wall_kill_bonus: number;
}

export interface Stable {
	stable_id: string;
	status: 'training' | 'completed' | 'halted';
	population_size: number;
	max_generations: number;
	opponent_af: number;
	episodes_per_eval: number;
	best_fitness: number;
	generations_completed: number;
	started_at: number;
	completed_at: number | null;
	fitness_weights: FitnessWeights | null;
	champion_count: number;
	enable_ltc: boolean;
	enable_lc_chain: boolean;
}

export interface GenerationStats {
	stable_id: string;
	generation: number;
	best_fitness: number;
	avg_fitness: number;
	worst_fitness: number;
	timestamp: number;
}

export interface Champion {
	stable_id: string;
	rank: number;
	network_json: string;
	fitness: number;
	generation: number;
	wins: number;
	losses: number;
	draws: number;
	exported_at: number;
}

export interface Hero {
	hero_id: string;
	name: string;
	fitness: number;
	origin_stable_id: string;
	generation: number;
	wins: number;
	losses: number;
	draws: number;
	promoted_at: number;
}

/** Aggregated results from a batch of headless duels */
export interface BatchTestResult {
	wins: number;
	losses: number;
	draws: number;
	total: number;
	win_rate: number;
	avg_ticks: number;
	avg_food: number;
	wall_kills: number;
}

/** SSE progress event from training stream */
export interface TrainingProgress {
	generation: number;
	best_fitness: number;
	avg_fitness: number;
	worst_fitness: number;
	running: boolean;
}
