%%%-------------------------------------------------------------------
%%% @author Bill Barnhill <>
%%% @copyright (C) 2012, Bill Barnhill
%%% @doc
%%%
%%% @end
%%% Created : 12 Feb 2012 by Bill Barnhill <>
%%%-------------------------------------------------------------------
-module(gen_nb_fsm_server).

-behaviour(gen_nb_server).

%% API
-export([start_link/2,start/2,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3,sock_opts/0, new_connection/2]).

-include("smtp_server.hrl").

-define(SERVER, ?MODULE). 

-record(state, {ipaddr, port, connection, fsm, listening=false}).

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
start_link(normal, []) ->
    {ok, IpAddr} = get_env(smtp_ipaddr, "0.0.0.0"),
    {ok, Port} = get_env(smtp_port, 2525),
    gen_nb_server:start_link(?MODULE, IpAddr, Port, [IpAddr, Port]).

start(normal, []) ->
    {ok, IpAddr} = get_env(smtp_ipaddr, "0.0.0.0"),
    {ok, Port} = get_env(smtp_port, 2525),
    gen_nb_server:start(?MODULE, IpAddr, Port, [IpAddr, Port]).

stop() ->
    gen_server:cast(?MODULE, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% This function allows getting the env variable from a parent level
%% if it's not found in the current application. Only one level is searched.
%% This needs to get refactored out of modules into its own module
get_env(Key, Default) ->
    case application:get_env(Key) of
	{ok, Value} -> {ok, Value};
	undefined ->
	    case application:get_env(parent_app) of
		{ok, ParentApp} ->
		    case application:get_env(ParentApp, Key) of
			{ok, Value} -> {ok, Value};
			undefined -> 
			    case Default of
				undefined -> undefined;
				_ -> {ok, Default}
			    end
		    end;
		undefined -> 
		    case Default of
			undefined -> undefined;
			_ -> {ok, Default}
		    end
	    end
    end.

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
init([IpAddr, Port]) ->
    {ok, #state{ipaddr=IpAddr,port=Port}}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
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
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
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

sock_opts() ->
    [list,
     {active, false},
     {packet, line},
     {reuseaddr, true}].

new_connection(Socket, State) ->
    ?SAY("got new connection"),
    Connection = case poolboy:checkout(conn_pool) of
		     ConnWorkerPid when is_pid(ConnWorkerPid) -> ConnWorkerPid;
		     ConnOther -> erlang:throw(ConnOther)
		 end,
    FSM = case poolboy:checkout(fsm_pool) of
		     FSMWorkerPid when is_pid(FSMWorkerPid) -> FSMWorkerPid;
		     FSMOther -> erlang:throw(FSMOther)
		 end,
    Controller = case poolboy:checkout(controller_pool) of
		     ControllerPid when is_pid(ControllerPid) -> ControllerPid;
		     ControllerOther -> erlang:throw(ControllerOther)
		 end,
    gen_server:cast(Controller, {attach_fsm, FSM}),
    gen_server:cast(Controller, {attach_connection, Connection}),
    gen_server:cast(Connection, {set_controller, Controller}),
    gen_fsm2:send_all_state_event(FSM, {set_controller, Controller}),
    case gen_tcp:controlling_process(Socket, Connection) of
	ok -> ?SAY("transfer successful to TCP connection handler process"),
	      gen_server:cast(Connection, {socket_control_transferred, Socket}),
	      {ok, State};
	{error, Reason} -> ?SAY("Error during transfer: ~p", [Reason]), 
			   {error, Reason, State}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
