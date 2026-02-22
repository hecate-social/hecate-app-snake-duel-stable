import { invoke, listen } from './tauri.js';

export interface SseStream {
	stop(): void;
}

export function connectPluginSse<T>(
	plugin: string,
	path: string,
	onEvent: (data: T) => void,
	onDone?: () => void,
	onError?: (error: string) => void
): SseStream {
	const streamId = crypto.randomUUID();
	const eventName = `plugin-sse-${streamId}`;
	const doneEvent = `plugin-sse-done-${streamId}`;
	const errorEvent = `plugin-sse-error-${streamId}`;
	const unlisteners: (() => void)[] = [];
	let stopped = false;

	async function start() {
		const unData = await listen<T>(eventName, (event) => {
			if (!stopped) onEvent(event.payload);
		});
		unlisteners.push(unData);

		const unDone = await listen(doneEvent, () => {
			if (!stopped && onDone) onDone();
			cleanup();
		});
		unlisteners.push(unDone);

		const unErr = await listen<{ type: string; error: string }>(errorEvent, (event) => {
			if (!stopped && onError) onError(event.payload.error);
			cleanup();
		});
		unlisteners.push(unErr);

		try {
			await invoke('plugin_sse_stream', {
				streamId, plugin, path, eventName, doneEvent, errorEvent
			});
		} catch (e) {
			if (onError) onError(String(e));
			cleanup();
		}
	}

	function cleanup() {
		for (const un of unlisteners) un();
		unlisteners.length = 0;
	}

	start();

	return {
		stop() {
			stopped = true;
			cleanup();
		}
	};
}
