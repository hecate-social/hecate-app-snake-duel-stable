<svelte:options customElement={{ tag: "snake-duel-stable-studio", shadow: "none" }} />

<script lang="ts">
	import { onMount } from 'svelte';
	import { setApi } from './game/client.js';
	import { setDuelApi } from './game/duel-client.js';
	import SnakeGladiatorsView from './components/SnakeGladiatorsView.svelte';

	interface PluginApi {
		get: <T>(path: string) => Promise<T>;
		post: <T>(path: string, body: unknown) => Promise<T>;
		del: <T>(path: string) => Promise<T>;
	}

	let { api }: { api: PluginApi } = $props();

	onMount(() => {
		setApi(api);
		setDuelApi(api);
	});
</script>

<div class="flex flex-col h-full">
	<SnakeGladiatorsView />
</div>
