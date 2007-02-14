-module(pop3_server).

-export([start/0, stop/0, start/2, stop/1]).

start() -> application:start(?MODULE).
stop() -> application:stop(?MODULE).

start(normal, []) ->
    {ok, Host} = application:get_env(listen_host),
    {ok, Port} = application:get_env(listen_port),
    generic_tcp_server:start_link(pop3_server_session, Host, Port,
				  [list,
				   {active, false},
				   {packet, line},
				   {reuseaddr, true}],
				  []).

stop(_State) ->
    ok.
