import { GRID_WIDTH, GRID_HEIGHT } from './snake-constants.js';
import type { Snake, Point, Direction, PoisonApple, WallTile } from './snake-types.js';

const DIRECTIONS: Direction[] = ['up', 'down', 'left', 'right'];
const OPPOSITE: Record<Direction, Direction> = {
	up: 'down',
	down: 'up',
	left: 'right',
	right: 'left'
};

export function chooseDirection(
	snake: Snake,
	opponent: Snake,
	food: Point,
	poisonApples: PoisonApple[],
	ownTag: 'player1' | 'player2',
	walls: WallTile[] = []
): Direction {
	const validDirs = DIRECTIONS.filter((d) => d !== OPPOSITE[snake.direction]);

	let bestDir = snake.direction;
	let bestScore = -Infinity;

	const wallPositions: Point[] = walls.map((w) => w.pos);
	const obstacles = buildObstacleSet(snake.body.slice(1), opponent.body, wallPositions);
	const ownPoisons = new Set(
		poisonApples.filter((p) => p.owner === ownTag).map((p) => `${p.pos[0]},${p.pos[1]}`)
	);
	const oppPoisons = new Set(
		poisonApples.filter((p) => p.owner !== ownTag).map((p) => `${p.pos[0]},${p.pos[1]}`)
	);

	for (const dir of validDirs) {
		const score = scoreDirection(dir, snake, opponent, food, obstacles, ownPoisons, oppPoisons);
		if (score > bestScore) {
			bestScore = score;
			bestDir = dir;
		}
	}

	return bestDir;
}

/** Decide whether the snake should drop a poison apple this tick */
export function shouldDropPoison(
	snake: Snake,
	opponent: Snake,
	poisonApples: PoisonApple[],
	ownTag: 'player1' | 'player2'
): boolean {
	if (snake.score < 3) return false;
	if (snake.body.length <= 5) return false;

	const af = snake.assholeFactor / 100;
	// Gentlemen never drop; jerks drop occasionally
	if (af < 0.2) return false;
	const baseChance = af * 0.03; // 0% at AF=0, 3% at AF=100

	// Bonus: drop more when opponent is nearby
	const oppHead = opponent.body[0];
	const tail = snake.body[snake.body.length - 1];
	const distToOpp = manhattan(tail, oppHead);
	const proximityBonus = distToOpp < 5 ? af * 0.02 : 0;

	// Limit: max 2 own poison apples on the field
	const ownCount = poisonApples.filter((p) => p.owner === ownTag).length;
	if (ownCount >= 2) return false;

	return Math.random() < baseChance + proximityBonus;
}

/** Decide whether the snake should drop its tail as a wall tile */
export function shouldDropWall(
	snake: Snake,
	walls: WallTile[],
	ownTag: 'player1' | 'player2'
): boolean {
	const af = snake.assholeFactor / 100;
	if (af < 0.3) return false;
	if (snake.body.length < 6) return false;

	const ownCount = walls.filter((w) => w.owner === ownTag).length;
	if (ownCount >= 3) return false;

	const baseChance = af * 0.02;
	return Math.random() < baseChance;
}

function scoreDirection(
	dir: Direction,
	snake: Snake,
	opponent: Snake,
	food: Point,
	obstacles: Set<string>,
	ownPoisons: Set<string>,
	oppPoisons: Set<string>
): number {
	const head = snake.body[0];
	const newHead = applyDir(head, dir);
	const af = snake.assholeFactor / 100;
	const key = `${newHead[0]},${newHead[1]}`;

	if (isDeadly(newHead, obstacles)) return -1000;

	let score = 0;

	// Avoid opponent's poison apples
	if (oppPoisons.has(key)) {
		score -= 200;
	}

	// Avoid own poison apples too (less penalty)
	if (ownPoisons.has(key)) {
		score -= 80;
	}

	// Reachable space (flood fill, capped at 50)
	const reachable = floodFill(newHead, obstacles, 50);
	score += reachable * 2;

	// Food attraction
	const oldFoodDist = manhattan(head, food);
	const newFoodDist = manhattan(newHead, food);
	score += (oldFoodDist - newFoodDist) * 15;

	// Wall avoidance
	if (
		newHead[0] < 2 ||
		newHead[0] >= GRID_WIDTH - 2 ||
		newHead[1] < 2 ||
		newHead[1] >= GRID_HEIGHT - 2
	) {
		score -= 5;
	}

	// Escape routes: count safe neighbors
	const safeNeighbors = DIRECTIONS.filter((d) => {
		const n = applyDir(newHead, d);
		return !isDeadly(n, obstacles);
	}).length;
	score += safeNeighbors * 5;

	// Trap detection
	if (reachable < snake.body.length + 3) {
		score -= 100;
	}

	// --- Asshole factor bonuses ---

	// Space cutoff: bonus for reducing opponent reachable space
	const oppHead = opponent.body[0];
	const oppObstaclesWithout = buildObstacleSet(opponent.body.slice(1), snake.body);
	const oppSpaceNow = floodFill(oppHead, oppObstaclesWithout, 50);

	const newSnakeBody: Point[] = [newHead, ...snake.body.slice(0, -1)];
	const oppObstaclesWith = buildObstacleSet(opponent.body.slice(1), newSnakeBody);
	const oppSpaceAfter = floodFill(oppHead, oppObstaclesWith, 50);
	const spaceCutoff = oppSpaceNow - oppSpaceAfter;
	if (spaceCutoff > 0) {
		score += spaceCutoff * 3 * af;
	}

	// Food stealing: bonus when intercepting opponent's food path
	const oppFoodDist = manhattan(oppHead, food);
	if (newFoodDist < oppFoodDist) {
		score += 10 * af;
	}

	// Body blocking: bonus for reducing opponent escape routes
	const oppSafeNeighbors = DIRECTIONS.filter((d) => {
		const n = applyDir(oppHead, d);
		return !isDeadly(n, oppObstaclesWith);
	}).length;
	const oppSafeNow = DIRECTIONS.filter((d) => {
		const n = applyDir(oppHead, d);
		return !isDeadly(n, oppObstaclesWithout);
	}).length;
	const blockedRoutes = oppSafeNow - oppSafeNeighbors;
	if (blockedRoutes > 0) {
		score += blockedRoutes * 8 * af;
	}

	// Lure opponent toward poison: bonus for being between opponent and poison
	for (const poisonKey of ownPoisons) {
		const [px, py] = poisonKey.split(',').map(Number) as [number, number];
		const oppDistToPoison = manhattan(oppHead, [px, py]);
		if (oppDistToPoison < 8) {
			score += af * 5;
		}
	}

	// Head-to-head proximity penalty (nice snakes avoid, assholes don't care)
	const headDist = manhattan(newHead, oppHead);
	if (headDist <= 2) {
		score -= 50 - af * 25;
	}

	// Random jitter
	score += (Math.random() - 0.5) * (50 + af * 12.5);

	// Risky aggression
	if (Math.random() < (5 + af * 10) / 100) {
		score += Math.random() * 30;
	}

	return score;
}

function applyDir([x, y]: Point, dir: Direction): Point {
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

function isDeadly([x, y]: Point, obstacles: Set<string>): boolean {
	if (x < 0 || x >= GRID_WIDTH || y < 0 || y >= GRID_HEIGHT) return true;
	return obstacles.has(`${x},${y}`);
}

function buildObstacleSet(...bodies: Point[][]): Set<string> {
	const set = new Set<string>();
	for (const body of bodies) {
		for (const [x, y] of body) {
			set.add(`${x},${y}`);
		}
	}
	return set;
}

function floodFill(start: Point, obstacles: Set<string>, max: number): number {
	const visited = new Set<string>();
	const queue: Point[] = [start];
	const startKey = `${start[0]},${start[1]}`;

	if (obstacles.has(startKey)) return 0;
	visited.add(startKey);

	let count = 0;
	while (queue.length > 0 && count < max) {
		const current = queue.shift()!;
		count++;

		for (const dir of DIRECTIONS) {
			const next = applyDir(current, dir);
			const key = `${next[0]},${next[1]}`;
			if (!visited.has(key) && !isDeadly(next, obstacles)) {
				visited.add(key);
				queue.push(next);
			}
		}
	}

	return count;
}

function manhattan(a: Point, b: Point): number {
	return Math.abs(a[0] - b[0]) + Math.abs(a[1] - b[1]);
}
