-module(smtp_server).

-behaviour(gen_server).

-export([start/0, stop/0, start/2, stop/1]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

start() ->
    application:start(smtp_server).

stop() ->
    application:stop(smtp_server).

start(normal, []) ->
    {ok, Host} = application:get_env(listen_host),
    {ok, Port} = application:get_env(listen_port),
    gen_server:start_link(?MODULE, [Host, Port], []).

stop(_State) ->
    ok.

%---------------------------------------------------------------------------
    
init([Host, Port]) ->
    {ok, IP} =  inet:getaddr(Host, inet),
    {ok, LSock} = gen_tcp:listen(Port, [list,
					{active, false},
					{packet, line},
					{reuseaddr, true},
                                        {ip, IP}]),
    smtp_server_session:accept_and_start(LSock),
    {ok, LSock}.

terminate(_Reason, State) ->
    LSock = State,
    gen_tcp:close(LSock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.
