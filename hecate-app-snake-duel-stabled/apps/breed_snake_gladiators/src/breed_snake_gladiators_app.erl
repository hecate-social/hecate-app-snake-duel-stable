%%% @doc breed_snake_gladiators application behaviour.
%%% Starts the ReckonDB gladiator_store and supervisor tree.
-module(breed_snake_gladiators_app).
-behaviour(application).

-include_lib("reckon_db/include/reckon_db.hrl").

-export([start/2, stop/1]).

-dialyzer({nowarn_function, start_gladiator_store/0}).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    ok = start_gladiator_store(),
    breed_snake_gladiators_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

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
            logger:info("[breed_snake_gladiators] gladiator_store ready"),
            ok;
        {error, {already_started, _Pid}} ->
            logger:info("[breed_snake_gladiators] gladiator_store already running"),
            ok;
        {error, Reason} ->
            logger:error("[breed_snake_gladiators] Failed to start gladiator_store: ~p", [Reason]),
            error({gladiator_store_start_failed, Reason})
    end.
