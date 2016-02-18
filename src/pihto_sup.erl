-module(pihto_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Pools} = application:get_env(pihto, pools),
    PoolSpecs = lists:map(fun({PoolName, WorkerName, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, PoolName}},
                    {worker_module, WorkerName}] ++ SizeArgs,
        poolboy:child_spec(PoolName, PoolArgs, WorkerArgs)
                          end, Pools),

    {ok, { {one_for_one, 10, 10}, PoolSpecs} }.
