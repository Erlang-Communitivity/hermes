
-module(smtp_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("smtp_server.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(POOLSPEC(POOL_NAME, ARGS), {POOL_NAME, {poolboy, start_link, [ARGS]}, permanent, 5000, worker, [poolboy]}).

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
    ?SAY("Starting smtp_server_sup supervision tree...",[]),
    {ok, {
       {one_for_one, 10, 10},
	  [
	   {conn_pool,
	    {poolboy, start_link, [[{name, {local, conn_pool}}, {worker_module, tcp_connection}]]},
	    permanent,
	    5000,
	    worker,
	    []
	   },
	   {fsm_pool,
	    {poolboy, start_link, [[{name, {local, fsm_pool}}, {worker_module, smtp_connection_fsm}]]},
	    permanent,
	    5000,
	    worker,
	    []
	   },
	   {controller_pool,
	    {poolboy, start_link, [[{name, {local, controller_pool}}, {worker_module, conn_fsm_controller}]]},
	    permanent,
	    5000,
	    worker,
	    []
	   },
	   {gen_nb_fsm_server,
	    {gen_nb_fsm_server, start_link, [normal,[]]},
	    permanent,
	    5000,
	    worker,
	    [gen_nb_fsm_server]			   
	   }
	  ]
      }
    }.


