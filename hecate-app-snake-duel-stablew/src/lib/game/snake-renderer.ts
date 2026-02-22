import { COLORS, GRID_WIDTH, GRID_HEIGHT, WALL_TTL } from './snake-constants.js';
import type { GameState, Point, WallTile } from './snake-types.js';

export function drawFrame(ctx: CanvasRenderingContext2D, state: GameState): void {
	const cw = ctx.canvas.width;
	const ch = ctx.canvas.height;
	const cellW = cw / GRID_WIDTH;
	const cellH = ch / GRID_HEIGHT;

	drawBackground(ctx, cw, ch);
	drawGrid(ctx, cw, ch, cellW, cellH);
	for (const wall of state.walls) {
		drawWallTile(ctx, wall, cellW, cellH);
	}
	for (const pa of state.poisonApples) {
		drawPoisonApple(ctx, pa.pos, cellW, cellH);
	}
	drawFood(ctx, state.food, cellW, cellH);
	drawSnake(ctx, state.snake1.body, COLORS.player1, COLORS.player1Head, cellW, cellH);
	drawSnake(ctx, state.snake2.body, COLORS.player2, COLORS.player2Head, cellW, cellH);
}

function drawBackground(ctx: CanvasRenderingContext2D, cw: number, ch: number): void {
	ctx.fillStyle = COLORS.background;
	ctx.fillRect(0, 0, cw, ch);
}

function drawGrid(ctx: CanvasRenderingContext2D, cw: number, ch: number, cellW: number, cellH: number): void {
	ctx.strokeStyle = COLORS.grid;
	ctx.lineWidth = 1;

	for (let x = 0; x <= GRID_WIDTH; x++) {
		ctx.beginPath();
		ctx.moveTo(x * cellW, 0);
		ctx.lineTo(x * cellW, ch);
		ctx.stroke();
	}
	for (let y = 0; y <= GRID_HEIGHT; y++) {
		ctx.beginPath();
		ctx.moveTo(0, y * cellH);
		ctx.lineTo(cw, y * cellH);
		ctx.stroke();
	}
}

function drawFood(ctx: CanvasRenderingContext2D, food: Point, cellW: number, cellH: number): void {
	const cx = food[0] * cellW + cellW / 2;
	const cy = food[1] * cellH + cellH / 2;
	const r = Math.min(cellW, cellH) * 0.35;

	ctx.save();
	ctx.shadowColor = COLORS.food;
	ctx.shadowBlur = 8;
	ctx.fillStyle = COLORS.food;
	ctx.beginPath();
	ctx.arc(cx, cy, r, 0, Math.PI * 2);
	ctx.fill();
	ctx.restore();
}

function drawPoisonApple(ctx: CanvasRenderingContext2D, pos: Point, cellW: number, cellH: number): void {
	const cx = pos[0] * cellW + cellW / 2;
	const cy = pos[1] * cellH + cellH / 2;
	const r = Math.min(cellW, cellH) * 0.35;

	ctx.save();
	ctx.shadowColor = COLORS.poisonGlow;
	ctx.shadowBlur = 6;
	ctx.fillStyle = COLORS.poison;
	ctx.beginPath();
	ctx.arc(cx, cy, r, 0, Math.PI * 2);
	ctx.fill();
	ctx.restore();

	const xr = r * 0.45;
	ctx.save();
	ctx.strokeStyle = '#000';
	ctx.lineWidth = 1.5;
	ctx.beginPath();
	ctx.moveTo(cx - xr, cy - xr);
	ctx.lineTo(cx + xr, cy + xr);
	ctx.moveTo(cx + xr, cy - xr);
	ctx.lineTo(cx - xr, cy + xr);
	ctx.stroke();
	ctx.restore();
}

function drawWallTile(
	ctx: CanvasRenderingContext2D,
	wall: WallTile,
	cellW: number,
	cellH: number
): void {
	const [x, y] = wall.pos;
	const gap = 1;
	const px = x * cellW + gap;
	const py = y * cellH + gap;
	const sw = cellW - gap * 2;
	const sh = cellH - gap * 2;

	const opacity = 0.3 + 0.7 * (wall.ttl / WALL_TTL);
	const borderColor = wall.owner === 'player1' ? COLORS.player1 : COLORS.player2;

	ctx.save();
	ctx.globalAlpha = opacity;

	// Filled rectangle
	ctx.fillStyle = COLORS.wall;
	ctx.fillRect(px, py, sw, sh);

	// Brick cross-hatch pattern
	ctx.strokeStyle = COLORS.wallDecay;
	ctx.lineWidth = 0.5;
	const midX = px + sw / 2;
	const midY = py + sh / 2;
	ctx.beginPath();
	ctx.moveTo(px, midY);
	ctx.lineTo(px + sw, midY);
	ctx.moveTo(midX, py);
	ctx.lineTo(midX, midY);
	ctx.moveTo(px, py + sh * 0.25);
	ctx.lineTo(px + sw, py + sh * 0.25);
	ctx.moveTo(px, py + sh * 0.75);
	ctx.lineTo(px + sw, py + sh * 0.75);
	ctx.stroke();

	// Owner-colored border
	ctx.strokeStyle = borderColor;
	ctx.lineWidth = 1.5;
	ctx.strokeRect(px, py, sw, sh);

	ctx.restore();
}

function drawSnake(
	ctx: CanvasRenderingContext2D,
	body: Point[],
	color: string,
	headColor: string,
	cellW: number,
	cellH: number
): void {
	const gap = 1;
	for (let i = body.length - 1; i >= 0; i--) {
		const [x, y] = body[i];
		const px = x * cellW + gap;
		const py = y * cellH + gap;
		const sw = cellW - gap * 2;
		const sh = cellH - gap * 2;

		if (i === 0) {
			ctx.fillStyle = headColor;
			ctx.beginPath();
			ctx.roundRect(px, py, sw, sh, 3);
			ctx.fill();

			const eyeR = Math.min(cellW, cellH) * 0.1;
			const eyeY = y * cellH + cellH * 0.35;
			const eye1X = x * cellW + cellW * 0.33;
			const eye2X = x * cellW + cellW * 0.67;

			ctx.fillStyle = COLORS.eye;
			ctx.beginPath();
			ctx.arc(eye1X, eyeY, eyeR, 0, Math.PI * 2);
			ctx.fill();
			ctx.beginPath();
			ctx.arc(eye2X, eyeY, eyeR, 0, Math.PI * 2);
			ctx.fill();

			ctx.fillStyle = '#000';
			const pupilR = eyeR * 0.5;
			ctx.beginPath();
			ctx.arc(eye1X, eyeY, pupilR, 0, Math.PI * 2);
			ctx.fill();
			ctx.beginPath();
			ctx.arc(eye2X, eyeY, pupilR, 0, Math.PI * 2);
			ctx.fill();
		} else {
			ctx.fillStyle = color;
			ctx.fillRect(px, py, sw, sh);
		}
	}
}

export function drawIdle(ctx: CanvasRenderingContext2D): void {
	const cw = ctx.canvas.width;
	const ch = ctx.canvas.height;

	ctx.fillStyle = 'rgba(0, 0, 0, 0.4)';
	ctx.fillRect(0, 0, cw, ch);

	const fontSize = Math.min(cw, ch) * 0.05;
	ctx.font = `${fontSize}px monospace`;
	ctx.textAlign = 'center';
	ctx.textBaseline = 'middle';
	ctx.fillStyle = '#888888';
	ctx.fillText('Press "New Match" to start', cw / 2, ch / 2);
}

export function drawCountdown(ctx: CanvasRenderingContext2D, count: number): void {
	const cw = ctx.canvas.width;
	const ch = ctx.canvas.height;

	ctx.fillStyle = 'rgba(0, 0, 0, 0.6)';
	ctx.fillRect(0, 0, cw, ch);

	const text = count > 0 ? String(count) : 'GO!';
	const fontSize = Math.min(cw, ch) * (count > 0 ? 0.2 : 0.15);
	ctx.fillStyle = count > 0 ? '#ffffff' : '#fbbf24';
	ctx.font = `bold ${fontSize}px monospace`;
	ctx.textAlign = 'center';
	ctx.textBaseline = 'middle';
	ctx.fillText(text, cw / 2, ch / 2);
}

export function drawGameOver(
	ctx: CanvasRenderingContext2D,
	winner: 'player1' | 'player2' | 'draw' | null,
	reason?: string
): void {
	const cw = ctx.canvas.width;
	const ch = ctx.canvas.height;

	ctx.fillStyle = 'rgba(0, 0, 0, 0.7)';
	ctx.fillRect(0, 0, cw, ch);

	const fontSize = Math.min(cw, ch) * 0.08;
	ctx.font = `bold ${fontSize}px monospace`;
	ctx.textAlign = 'center';
	ctx.textBaseline = 'middle';

	const centerY = ch / 2 - (reason ? fontSize * 0.4 : 0);

	if (winner === 'player1') {
		ctx.fillStyle = COLORS.player1Head;
		ctx.fillText('Blue Wins!', cw / 2, centerY - fontSize * 0.6);
	} else if (winner === 'player2') {
		ctx.fillStyle = COLORS.player2Head;
		ctx.fillText('Red Wins!', cw / 2, centerY - fontSize * 0.6);
	} else {
		ctx.fillStyle = '#ffffff';
		ctx.fillText("It's a Draw!", cw / 2, centerY - fontSize * 0.6);
	}

	if (reason) {
		ctx.font = `${fontSize * 0.35}px monospace`;
		ctx.fillStyle = '#aaaaaa';
		ctx.fillText(reason, cw / 2, centerY + fontSize * 0.4);
	}

	ctx.font = `${fontSize * 0.35}px monospace`;
	ctx.fillStyle = '#666666';
	ctx.fillText('Press "New Match" to play again', cw / 2, centerY + fontSize * 1.2);
}
