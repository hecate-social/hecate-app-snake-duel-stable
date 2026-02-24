%%% @doc run_snake_duel application behaviour.
%%% Starts pg scope for duel process groups.
-module(run_snake_duel_app).
-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    ok = ensure_pg(),
    run_snake_duel_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

ensure_pg() ->
    case pg:start(pg) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.
