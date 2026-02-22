-module(hecate_app_snake_duel_stabled_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},
    ChildSpecs = [
        #{
            id => training_proc_sup,
            start => {training_proc_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        #{
            id => gladiator_duel_proc_sup,
            start => {gladiator_duel_proc_sup, start_link, []},
            restart => permanent,
            type => supervisor
        },
        #{
            id => query_snake_gladiators_store,
            start => {query_snake_gladiators_store, start_link, []},
            restart => permanent,
            type => worker
        },
        #{
            id => app_snake_duel_stabled_plugin_registrar,
            start => {app_snake_duel_stabled_plugin_registrar, start_link, []},
            restart => transient,
            type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
