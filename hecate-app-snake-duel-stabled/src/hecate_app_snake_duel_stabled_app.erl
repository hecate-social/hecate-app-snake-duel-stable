-module(hecate_app_snake_duel_stabled_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case application:get_env(hecate_app_snake_duel_stabled, enabled, true) of
        false ->
            logger:info("[hecate_app_snake_duel_stabled] Disabled by config"),
            {ok, spawn(fun() -> receive stop -> ok end end)};
        true ->
            ok = app_snake_duel_stabled_paths:ensure_layout(),
            ok = start_cowboy(),
            logger:info("[hecate_app_snake_duel_stabled] Started, socket at ~s",
                        [app_snake_duel_stabled_paths:socket_path("api.sock")]),
            hecate_app_snake_duel_stabled_sup:start_link()
    end.

stop(_State) ->
    ok = cowboy:stop_listener(app_snake_duel_stabled_http),
    cleanup_socket(),
    ok.

start_cowboy() ->
    SocketPath = app_snake_duel_stabled_paths:socket_path("api.sock"),
    cleanup_socket_file(SocketPath),
    StaticDir = static_dir(),
    Routes = [
        {"/health", app_snake_duel_stabled_health_api, []},
        {"/manifest", app_snake_duel_stabled_manifest_api, []},
        {"/ui/[...]", cowboy_static, {dir, StaticDir, [{mimetypes, cow_mimetypes, all}]}},
        {"/api/arcade/gladiators/stables", initiate_stable_api, []},
        {"/api/arcade/gladiators/stables/:stable_id", get_stable_by_id_api, []},
        {"/api/arcade/gladiators/stables/:stable_id/champion", get_champion_api, []},
        {"/api/arcade/gladiators/stables/:stable_id/champions", get_champions_api, []},
        {"/api/arcade/gladiators/stables/:stable_id/generations", get_training_history_api, []},
        {"/api/arcade/gladiators/stables/:stable_id/stream", stream_training_api, []},
        {"/api/arcade/gladiators/stables/:stable_id/halt", halt_training_api, []},
        {"/api/arcade/gladiators/stables/:stable_id/export", export_champion_api, []},
        {"/api/arcade/gladiators/stables/:stable_id/duel", start_champion_duel_api, []},
        {"/api/arcade/gladiators/stables/:stable_id/batch-test", batch_test_champion_api, []},
        {"/api/arcade/gladiators/fitness-presets", fitness_presets_api, []},
        {"/api/arcade/gladiators/heroes", heroes_api, []},
        {"/api/arcade/gladiators/heroes/:hero_id/promote", heroes_api, []},
        {"/api/arcade/gladiators/heroes/:hero_id", hero_detail_api, []},
        {"/api/arcade/gladiators/heroes/:hero_id/duel", hero_detail_api, []}
    ],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    TransOpts = #{
        socket_opts => [{ifaddr, {local, SocketPath}}],
        num_acceptors => 5
    },
    ProtoOpts = #{
        env => #{dispatch => Dispatch},
        idle_timeout => 600000,
        request_timeout => 600000
    },
    {ok, _} = cowboy:start_clear(app_snake_duel_stabled_http, TransOpts, ProtoOpts),
    ok.

cleanup_socket() ->
    SocketPath = app_snake_duel_stabled_paths:socket_path("api.sock"),
    cleanup_socket_file(SocketPath).

cleanup_socket_file(Path) ->
    case file:delete(Path) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Reason} ->
            logger:warning("[hecate_app_snake_duel_stabled] Failed to remove socket ~s: ~p", [Path, Reason]),
            ok
    end.

static_dir() ->
    PrivDir = code:priv_dir(hecate_app_snake_duel_stabled),
    filename:join(PrivDir, "static").
