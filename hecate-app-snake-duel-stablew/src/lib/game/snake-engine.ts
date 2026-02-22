import { GRID_WIDTH, GRID_HEIGHT, INITIAL_SNAKE_1, INITIAL_SNAKE_2, WALL_TTL } from './snake-constants.js';
import { chooseDirection, shouldDropPoison, shouldDropWall } from './snake-ai.js';
import type { GameState, Snake, Point, Direction, PoisonApple, WallTile, GameEvent } from './snake-types.js';

export function createGame(af1: number, af2: number): GameState {
	return {
		snake1: createSnake(INITIAL_SNAKE_1, 'right', af1),
		snake2: createSnake(INITIAL_SNAKE_2, 'left', af2),
		food: spawnFood(INITIAL_SNAKE_1, INITIAL_SNAKE_2),
		poisonApples: [],
		walls: [],
		status: 'idle',
		winner: null,
		tick: 0,
		countdown: 3
	};
}

function createSnake(body: Point[], direction: Direction, assholeFactor: number): Snake {
	return {
		body: body.map(([x, y]) => [x, y] as Point),
		direction,
		score: 0,
		assholeFactor,
		events: []
	};
}

export function tickGame(state: GameState): GameState {
	if (state.status !== 'running') return state;

	const tick = state.tick + 1;
	let poisonApples = [...state.poisonApples];

	// Decay walls (before movement)
	let walls = decayWalls(state.walls);

	// AI chooses directions (wall-aware)
	const dir1 = chooseDirection(state.snake1, state.snake2, state.food, poisonApples, 'player1', walls);
	const dir2 = chooseDirection(state.snake2, state.snake1, state.food, poisonApples, 'player2', walls);

	const s1 = moveSnake(state.snake1, dir1, tick);
	const s2 = moveSnake(state.snake2, dir2, tick);

	// Check collisions (walls, self, opponent, wall tiles)
	const collision = checkCollisions(s1, s2, walls);
	if (collision) {
		addEvent(collision.loser === 'draw' ? s1 : collision.loser === 'player1' ? s1 : s2, {
			type: 'collision',
			value: collision.reason,
			tick
		});
		const winnerSnake =
			collision.loser === 'player1' ? s2 : collision.loser === 'player2' ? s1 : null;
		if (winnerSnake) {
			addEvent(winnerSnake, { type: 'win', value: 'Victory!', tick });
		}
		return {
			...state,
			snake1: s1,
			snake2: s2,
			poisonApples,
			walls,
			tick,
			status: 'finished',
			winner:
				collision.loser === 'draw'
					? 'draw'
					: collision.loser === 'player1'
						? 'player2'
						: 'player1'
		};
	}

	// Check poison apple consumption
	poisonApples = checkPoisonConsumption(s1, 'player1', poisonApples, tick);
	poisonApples = checkPoisonConsumption(s2, 'player2', poisonApples, tick);

	// Check food consumption
	let food = state.food;
	const ate1 = pointsEqual(s1.body[0], food);
	const ate2 = pointsEqual(s2.body[0], food);

	if (ate1) {
		s1.score++;
		s1.body.push(s1.body[s1.body.length - 1]);
		addEvent(s1, { type: 'food', value: `Ate food (${s1.score})`, tick });
	}
	if (ate2) {
		s2.score++;
		s2.body.push(s2.body[s2.body.length - 1]);
		addEvent(s2, { type: 'food', value: `Ate food (${s2.score})`, tick });
	}
	if (ate1 || ate2) {
		food = spawnFood(s1.body, s2.body, walls);
	}

	// AI decides whether to drop poison apples
	if (shouldDropPoison(s1, s2, poisonApples, 'player1')) {
		const dropPos: Point = s1.body[s1.body.length - 1];
		poisonApples.push({ pos: dropPos, owner: 'player1' });
		s1.score--;
		if (s1.body.length > 1) s1.body.pop();
		addEvent(s1, { type: 'poison-drop', value: `Dropped poison (${s1.score})`, tick });
	}
	if (shouldDropPoison(s2, s1, poisonApples, 'player2')) {
		const dropPos: Point = s2.body[s2.body.length - 1];
		poisonApples.push({ pos: dropPos, owner: 'player2' });
		s2.score--;
		if (s2.body.length > 1) s2.body.pop();
		addEvent(s2, { type: 'poison-drop', value: `Dropped poison (${s2.score})`, tick });
	}

	// AI decides whether to drop tail as wall
	walls = maybeDropWall(s1, 'player1', walls, tick);
	walls = maybeDropWall(s2, 'player2', walls, tick);

	return { ...state, snake1: s1, snake2: s2, food, poisonApples, walls, tick };
}

function decayWalls(walls: WallTile[]): WallTile[] {
	return walls
		.map((w) => ({ ...w, ttl: w.ttl - 1 }))
		.filter((w) => w.ttl > 0);
}

function maybeDropWall(
	snake: Snake,
	tag: 'player1' | 'player2',
	walls: WallTile[],
	tick: number
): WallTile[] {
	if (!shouldDropWall(snake, walls, tag)) return walls;

	const dropPos = snake.body[snake.body.length - 1];
	const wallSet = new Set(walls.map((w) => `${w.pos[0]},${w.pos[1]}`));
	if (wallSet.has(`${dropPos[0]},${dropPos[1]}`)) return walls;

	if (snake.body.length > 1) snake.body.pop();
	addEvent(snake, { type: 'wall-drop', value: 'Dropped wall', tick });
	return [...walls, { pos: dropPos, owner: tag, ttl: WALL_TTL }];
}

function checkPoisonConsumption(
	snake: Snake,
	tag: 'player1' | 'player2',
	poisonApples: PoisonApple[],
	tick: number
): PoisonApple[] {
	const head = snake.body[0];
	const hitIdx = poisonApples.findIndex((p) => pointsEqual(head, p.pos));
	if (hitIdx === -1) return poisonApples;

	// Ate a poison apple — lose a point and shrink
	snake.score = Math.max(0, snake.score - 1);
	if (snake.body.length > 1) snake.body.pop();
	const dropper = poisonApples[hitIdx].owner;
	const ownPoison = dropper === tag;
	addEvent(snake, {
		type: 'poison-eat',
		value: ownPoison ? `Ate own poison! (${snake.score})` : `Poisoned! (${snake.score})`,
		tick
	});
	return poisonApples.filter((_, i) => i !== hitIdx);
}

function moveSnake(snake: Snake, newDirection: Direction, tick: number): Snake {
	const head = snake.body[0];
	const newHead = applyDirection(head, newDirection);
	const newBody: Point[] = [newHead, ...snake.body.slice(0, -1)];

	const turned = newDirection !== snake.direction;
	const s: Snake = { ...snake, body: newBody, direction: newDirection, events: [...snake.events] };
	if (turned) {
		addEvent(s, { type: 'turn', value: `Turn ${newDirection}`, tick });
	}
	return s;
}

function applyDirection(point: Point, dir: Direction): Point {
	const [x, y] = point;
	switch (dir) {
		case 'up':
			return [x, y - 1];
		case 'down':
			return [x, y + 1];
		case 'left':
			return [x - 1, y];
		case 'right':
			return [x + 1, y];
	}
}

interface CollisionResult {
	loser: 'player1' | 'player2' | 'draw';
	reason: string;
}

function scoreBreaker(s1: Snake, s2: Snake, reason: string): CollisionResult {
	if (s1.score > s2.score) return { loser: 'player2', reason: `${reason} (Blue leads ${s1.score}-${s2.score})` };
	if (s2.score > s1.score) return { loser: 'player1', reason: `${reason} (Red leads ${s2.score}-${s1.score})` };
	return { loser: 'draw', reason };
}

function checkCollisions(s1: Snake, s2: Snake, walls: WallTile[]): CollisionResult | null {
	const h1 = s1.body[0];
	const h2 = s2.body[0];

	const h1Wall = isOutOfBounds(h1);
	const h2Wall = isOutOfBounds(h2);

	if (pointsEqual(h1, h2)) return scoreBreaker(s1, s2, 'Head-to-head collision');
	if (h1Wall && h2Wall) return scoreBreaker(s1, s2, 'Both hit walls');
	if (h1Wall) return { loser: 'player1', reason: 'Hit wall' };
	if (h2Wall) return { loser: 'player2', reason: 'Hit wall' };

	// Wall tile collisions
	const wallSet = new Set(walls.map((w) => `${w.pos[0]},${w.pos[1]}`));
	const h1WallTile = wallSet.has(`${h1[0]},${h1[1]}`);
	const h2WallTile = wallSet.has(`${h2[0]},${h2[1]}`);
	if (h1WallTile && h2WallTile) return scoreBreaker(s1, s2, 'Both hit wall tiles');
	if (h1WallTile) return { loser: 'player1', reason: 'Hit wall tile' };
	if (h2WallTile) return { loser: 'player2', reason: 'Hit wall tile' };

	const h1Self = hitsBody(h1, s1.body.slice(1));
	const h2Self = hitsBody(h2, s2.body.slice(1));
	if (h1Self && h2Self) return scoreBreaker(s1, s2, 'Both self-collided');
	if (h1Self) return { loser: 'player1', reason: 'Self collision' };
	if (h2Self) return { loser: 'player2', reason: 'Self collision' };

	const h1Opp = hitsBody(h1, s2.body.slice(1));
	const h2Opp = hitsBody(h2, s1.body.slice(1));
	if (h1Opp && h2Opp) return scoreBreaker(s1, s2, 'Both hit opponent');
	if (h1Opp) return { loser: 'player1', reason: 'Hit opponent body' };
	if (h2Opp) return { loser: 'player2', reason: 'Hit opponent body' };

	return null;
}

function isOutOfBounds([x, y]: Point): boolean {
	return x < 0 || x >= GRID_WIDTH || y < 0 || y >= GRID_HEIGHT;
}

function hitsBody(head: Point, body: Point[]): boolean {
	return body.some((seg) => pointsEqual(head, seg));
}

function pointsEqual(a: Point, b: Point): boolean {
	return a[0] === b[0] && a[1] === b[1];
}

function addEvent(
	snake: Snake,
	event: GameEvent
) {
	snake.events = [event, ...snake.events].slice(0, 10);
}

export function spawnFood(body1: Point[], body2: Point[], walls: WallTile[] = []): Point {
	const wallPositions = walls.map((w) => `${w.pos[0]},${w.pos[1]}`);
	const occupied = new Set([...body1, ...body2].map(([x, y]) => `${x},${y}`).concat(wallPositions));
	let attempts = 0;
	while (attempts < 1000) {
		const x = Math.floor(Math.random() * GRID_WIDTH);
		const y = Math.floor(Math.random() * GRID_HEIGHT);
		if (!occupied.has(`${x},${y}`)) return [x, y];
		attempts++;
	}
	return [Math.floor(GRID_WIDTH / 2), Math.floor(GRID_HEIGHT / 2)];
}
