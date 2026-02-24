%%% @doc run_snake_duel top-level supervisor.
%%% Engine modules (snake_duel_engine, snake_duel_ai, duel_proc) are
%%% stateless or dynamically spawned -- no permanent children needed.
-module(run_snake_duel_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10
    },
    {ok, {SupFlags, []}}.
