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
	test/1,
	attach_fsm/2,
	detach_fsm/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("smtp_server.hrl").

-define(SERVER, ?MODULE). 

-record(state, {socket,fsm=none,cb=none}).

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
start_link(CallbackPid) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [CallbackPid], []).

start(CallbackPid) ->
    gen_server:start({local, ?SERVER}, ?MODULE, [CallbackPid], []).

close(ServerSpec) ->
    gen_server:cast(ServerSpec, close).
reply(ServerSpec, MessageText) ->
    gen_server:cast(ServerSpec, {reply, [MessageText, "\n"]}).
notify(ServerSpec, NextState, PreviousState, TransitionInfo) ->
    gen_server:cast(ServerSpec, {next_state, NextState, PreviousState, TransitionInfo}).
test(ServerSpec) ->
    gen_server:call(ServerSpec, test).
attach_fsm(ServerSpec, FSM) ->
    gen_server:cast(ServerSpec, {attach_fsm, FSM}).
detach_fsm(ServerSpec) ->
    gen_server:cast(ServerSpec, detach_fsm).

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
init([CallbackPid]) ->
    say("initializing tcp_connection"),
    {ok, #state{fsm=none, cb=CallbackPid},0 }.

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
handle_call(detach_fsm, _From, State=#state{fsm=FSM}) ->
    {reply, {ok, FSM}, State#state{fsm=none}};
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
handle_cast(close, State=#state{socket=Socket}) ->
    gen_tcp:close(Socket),
    {noreply, State};
handle_cast({reply, MessageText}, State=#state{socket=Socket}) ->
    say("SEND >~p", [MessageText]),
    gen_tcp:send(Socket, MessageText),
    {noreply, State};
handle_cast({attach_fsm, FSM}, State) ->
    {noreply, State#state{fsm=FSM}};
handle_cast({socket_control_transferred, Socket}, State=#state{fsm=FSM}) ->
    say("Got a socke transferred, FSM: ~p", [FSM]),
    inet:setopts(Socket, [{active, true}]),
    case FSM of
	none -> ok;
	_ -> gen_fsm:send_event(FSM, {tcp_opened, self()})
    end, 
    {noreply, State#state{socket=Socket}};
handle_cast(Other, State) ->
    say("unknown cast: ~w", [Other]),
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
handle_info({tcp, _Socket, Data}, State=#state{fsm=FSM}) ->
    say("Got data: ~p", [Data]),
    gen_fsm:send_event(FSM, {tcp, self(), Data}), 
    {noreply, State};
handle_info({tcp_closed, _Socket}, State=#state{fsm=FSM}) ->
    say("Got tcp_closed"),
    gen_fsm:send_event(FSM, {tcp_closed, self()}), 
    {noreply, State};
handle_info({tcp_error, _Socket, Reason}, State=#state{fsm=FSM}) ->
    say("Got error: ~p", [Reason]),
    gen_fsm:send_event(FSM, {tcp_error, self(), Reason}), 
    {stop, normal, State};
handle_info(timeout, State=#state{cb=Callback}) ->
    say("Got timeout"),
    Callback ! {up, ?MODULE, self()},
    {noreply, State}.




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
terminate(_Reason, #state{socket=Socket}) ->
    say("terminate called"),
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

say(Msg) ->
    io:format("~p:~p > ~p~n",[?FILE,?LINE,Msg]).
say(Format, Args) ->
    io:format("~p:~p > "++Format++"~n",[?FILE,?LINE]++Args).
