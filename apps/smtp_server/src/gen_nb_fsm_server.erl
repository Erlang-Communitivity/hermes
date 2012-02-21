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
-export([start_link/5,start/5,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3,sock_opts/0, new_connection/2, initializer/4]).

-define(SERVER, ?MODULE). 

-record(state, {ipaddr, port, fsm_module,connection_module,callback, listening=false}).

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
start_link(IpAddr, Port, ModuleFSM, ModuleConnection, CallbackPid) ->
    gen_nb_server:start_link(?MODULE, IpAddr, Port, [IpAddr, Port, ModuleFSM, ModuleConnection, CallbackPid]).

start(IpAddr, Port, ModuleFSM, ModuleConnection, CallbackPid) ->
    gen_nb_server:start(?MODULE, IpAddr, Port, [IpAddr, Port, ModuleFSM, ModuleConnection, CallbackPid]).

stop() ->
    gen_server:cast(?MODULE, stop).

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
init([IpAddr, Port, ModuleFSM, ModuleConnection, CallbackPid]) ->
    {ok, #state{ipaddr=IpAddr,port=Port,fsm_module=ModuleFSM,connection_module=ModuleConnection,callback=CallbackPid},0}.

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
handle_info(timeout, State=#state{listening=true,callback=CallbackPid}) ->
    CallbackPid ! {up, ?MODULE, self()},
    {noreply, State};
%%handle_info(timeout, State=#state{ipaddr=_IpAddr,port=_Port}) ->
    %% below is from Knutin's version of gen_nb_server, which allows multiple ports, IP addresses
    %%case gen_nb_server:add_listen_socket({IpAddr, Port}, State) of
    %%    {ok, State1} ->
    %%        {noreply, State1#state{listening=true}, 0};
    %%    Error ->
    %%        {stop, Error, {error, Error}, State}
    %%end;
%%    {noreply, State};
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
    say("got new connection"),
    %fsm_module,connection_module
    ModuleFSM = State#state.fsm_module,
    ModuleConn = State#state.connection_module,
    Initializer = spawn(fun() -> initializer(Socket, ModuleConn, none, none) end),
    {ok, _FSM } = ModuleFSM:start(Initializer),
    {ok, _Connection } = ModuleConn:start(Initializer),
    case gen_tcp:controlling_process(Socket, Initializer) of
	ok -> say("transfer successful to initializer process"),
	      {ok, State};
	{error, Reason} -> say("Error during transfer: ~p", [Reason]), 
			   {error, Reason, State}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

initializer(Socket,ModuleConn, FSM, Connection) ->
    say("+initializer FSM:~p Connection:~p", [FSM, Connection]),
    if
	(FSM /= none) and (Connection /= none) ->
	    say("both tcp_connection and FSM are up, so let's transfer socket control"),
	    ModuleConn:attach_fsm(Connection, FSM),
	    case gen_tcp:controlling_process(Socket, Connection) of
		ok -> say("transfer successful to tcp_connection instance"),
		      gen_server:cast(Connection, {socket_control_transferred, Socket}),
		      ok;
		{error, Reason} -> say("Error during transfer: ~p", [Reason]), 
				   {error, Reason}
	    end;
	true ->
	    receive
		{up, tcp_connection, Connection2} -> initializer(Socket, ModuleConn, FSM, Connection2);	
		{up, _ModuleFSM, FSM2} -> initializer(Socket, ModuleConn, FSM2, Connection);
		Other -> say("Received unexpected: ~w", [Other])
	    end
    end.
    

say(Msg) ->
    io:format("~p:~p > ~p~n",[?FILE,?LINE,Msg]).
say(Format, Args) ->
    io:format("~p:~p > "++Format++"~n",[?FILE,?LINE]++Args).
