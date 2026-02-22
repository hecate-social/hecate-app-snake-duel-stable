-module(hecate_app_snake_duel_stabled_app).
-behaviour(application).

-include_lib("reckon_db/include/reckon_db.hrl").

-export([start/2, stop/1]).

-dialyzer({nowarn_function, start_gladiator_store/0}).

start(_StartType, _StartArgs) ->
    case application:get_env(hecate_app_snake_duel_stabled, enabled, true) of
        false ->
            logger:info("[hecate_app_snake_duel_stabled] Disabled by config"),
            {ok, spawn(fun() -> receive stop -> ok end end)};
        true ->
            ok = app_snake_duel_stabled_paths:ensure_layout(),
            ok = ensure_pg_scope(),
            ok = start_gladiator_store(),
            ok = start_cowboy(),
            logger:info("[hecate_app_snake_duel_stabled] Started, socket at ~s",
                        [app_snake_duel_stabled_paths:socket_path("api.sock")]),
            hecate_app_snake_duel_stabled_sup:start_link()
    end.

stop(_State) ->
    ok = cowboy:stop_listener(app_snake_duel_stabled_http),
    cleanup_socket(),
    ok.

ensure_pg_scope() ->
    case pg:start_link(hecate_app_snake_duel_stabled) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end.

start_gladiator_store() ->
    DataDir = app_snake_duel_stabled_paths:reckon_path("gladiator"),
    ok = filelib:ensure_path(DataDir),
    Config = #store_config{
        store_id = gladiator_store,
        data_dir = DataDir,
        mode = single,
        writer_pool_size = 5,
        reader_pool_size = 5,
        gateway_pool_size = 2,
        options = #{}
    },
    case reckon_db_sup:start_store(Config) of
        {ok, _Pid} ->
            logger:info("[hecate_app_snake_duel_stabled] gladiator_store ready"),
            ok;
        {error, {already_started, _Pid}} ->
            logger:info("[hecate_app_snake_duel_stabled] gladiator_store already running"),
            ok;
        {error, Reason} ->
            logger:error("[hecate_app_snake_duel_stabled] Failed to start gladiator_store: ~p", [Reason]),
            error({gladiator_store_start_failed, Reason})
    end.

start_cowboy() ->
    SocketPath = app_snake_duel_stabled_paths:socket_path("api.sock"),
    cleanup_socket_file(SocketPath),
    Routes = [
        {"/health", app_snake_duel_stabled_health_api, []},
        {"/manifest", app_snake_duel_stabled_manifest_api, []},
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
