
-module(smtp_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(POOLSPEC(POOL_NAME, ARGS), {POOL_NAME, {poolboy, start_link, [ARGS]}, permanent, 5000, worker, [poolboy]}.

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%% Remember we need to have the following in each pool's config
%% {name, {local, PoolName}},
%% {worker_module, example_worker}
%%
init([]) ->
    {ok, Pools} = application:get_env(smtp_server, pools),
    PoolSpecs = lists:map(fun ({PoolName, PoolConf}) -> ?POOLSPEC(PoolName, PoolConf) end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.


