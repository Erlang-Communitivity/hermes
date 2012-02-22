%%%-------------------------------------------------------------------
%%% @author Bill Barnhill <>
%%% @copyright (C) 2012, Bill Barnhill
%%% @doc
%%%
%%% @end
%%% Created : 11 Feb 2012 by Bill Barnhill <>
%%%-------------------------------------------------------------------
-module(tcp_connection).

-behaviour(gen_server).

%% API

-export([start_link/1,
	 start/1,
	close/1,
	reply/2,
	notify/4,
	test/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("smtp_server.hrl").

-define(SERVER, ?MODULE). 

-record(state, {socket, controller}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(_Args) ->
    gen_server:start_link(?MODULE, [], []).

start(_Args) ->
    gen_server:start(?MODULE, [], []).

close(ServerSpec) ->
    gen_server:cast(ServerSpec, close).
reply(ServerSpec, MessageText) ->
    gen_server:cast(ServerSpec, {reply, [MessageText, "\n"]}).
notify(ServerSpec, NextState, PreviousState, TransitionInfo) ->
    gen_server:cast(ServerSpec, {next_state, NextState, PreviousState, TransitionInfo}).
test(ServerSpec) ->
    gen_server:call(ServerSpec, test).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ?SAY("initializing tcp_connection"),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(test, _From, State=#state{socket=Socket}) ->
    case inet_db:lookup_socket(Socket) of
	{ok, _Mod} -> {reply, {ok, true}, State};
	_ -> {reply, {ok, false}, State}
    end;
handle_call(Request, _From, State) ->
    Error = {unknown_call, Request},
    {stop, Error, {error, Error}, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({tcp_do_close, Controller}, State=#state{socket=Socket}) ->
    gen_tcp:close(Socket),
    {noreply, State};
handle_cast({tcp_do_send, Controller, MessageText}, State=#state{socket=Socket}) ->
    ?SAY("SEND >~p", [MessageText]),
    gen_tcp:send(Socket, MessageText++"\r\n"),
    {noreply, State};
handle_cast({set_controller, Controller}, State) ->
    {noreply, State#state{controller=Controller}};
handle_cast({socket_control_transferred, Socket}, State=#state{controller=Controller}) ->
    ?SAY("Got a socke transferred, Controller: ~p", [Controller]),
    inet:setopts(Socket, [{active, true}]),
    case Controller of
	undefined -> erlang:throw({error,controller_not_set});
	_ -> gen_server:cast(Controller, {tcp_opened, self()})
    end, 
    {noreply, State#state{socket=Socket}};
handle_cast(Other, State) ->
    ?SAY("unknown cast: ~w", [Other]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({tcp, _Socket, Data}, State=#state{controller=Controller}) ->
    ?SAY("Got data: ~p", [Data]),
    gen_server:cast(Controller, {tcp_data, self(), Data}),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State=#state{controller=Controller}) ->
    ?SAY("Got tcp_closed"),
    gen_server:cast(Controller, {tcp_closed, self()}),
    {noreply, State};
handle_info({tcp_error, _Socket, Reason}, State=#state{controller=Controller}) ->
    ?SAY("Got error: ~p", [Reason]),
    gen_server:cast(Controller, {tcp_error, self(), Reason}),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, #state{socket=Socket}) ->
    ?SAY("terminate called with reason ~p", [Reason]),
    gen_tcp:close(Socket),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
