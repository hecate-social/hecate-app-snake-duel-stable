export type Direction = 'up' | 'down' | 'left' | 'right';
export type GameStatus = 'idle' | 'countdown' | 'running' | 'finished';
export type Point = [number, number];

export interface Snake {
	body: Point[];
	direction: Direction;
	score: number;
	assholeFactor: number;
	events: GameEvent[];
}

export interface PoisonApple {
	pos: Point;
	owner: 'player1' | 'player2';
}

export interface WallTile {
	pos: Point;
	owner: 'player1' | 'player2';
	ttl: number;
}

export interface GameEvent {
	type: 'food' | 'turn' | 'collision' | 'win' | 'poison-drop' | 'poison-eat' | 'wall-drop' | 'wall-hit';
	value: string;
	tick: number;
}

export interface GameState {
	snake1: Snake;
	snake2: Snake;
	food: Point;
	poisonApples: PoisonApple[];
	walls: WallTile[];
	status: GameStatus;
	winner: 'player1' | 'player2' | 'draw' | null;
	tick: number;
	countdown: number;
}
