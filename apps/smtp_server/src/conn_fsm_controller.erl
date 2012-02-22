-module(conn_fsm_controller).

-behaviour(gen_server).

%% API
-export([start_link/1,start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("smtp_server.hrl").

-define(SERVER, ?MODULE). 

-record(state, {connection, fsm}).

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
handle_cast(In={attach_fsm, FSM}, State) ->
    ?SAY("Received..~p", [In]),
    {noreply, State#state{fsm=FSM}};
handle_cast(In={attach_connection, Conn}, State) ->
    ?SAY("Received..~p", [In]),
    {noreply, State#state{connection=Conn}};

handle_cast(In={tcp_opened, _ConnectionServer}, State=#state{fsm=FSM}) ->
    ?SAY("Received..~p", [In]),
    Ev = {socket_opened, self()},
    gen_fsm2:send_event(FSM, Ev),
    {noreply, State};
handle_cast(In={tcp_closed, _ConnectionServer}, State=#state{fsm=undefined,connection=Connection}) ->
    ?SAY("Received..~p", [In]),
    poolboy:checkin(conn_pool, Connection),
    {noreply, State#state{connection=undefined}};
handle_cast(In={tcp_closed, _ConnectionServer}, State=#state{fsm=FSM,connection=Connection}) ->
    ?SAY("Received..~p", [In]),
    Ev = {socket_closed, self()},
    gen_fsm2:send_event(FSM, Ev),
    poolboy:checkin(conn_pool, Connection),
    {noreply, State#state{connection=undefined}};
handle_cast(In={tcp_data, _ConnectionServer, Data}, State=#state{fsm=FSM}) ->
    ?SAY("Received..~p", [In]),
    Ev = {socket_data, self(), Data},
    gen_fsm2:send_event(FSM, Ev),
    {noreply, State};

handle_cast(In={fsm_reply, _FSM, Reply}, State=#state{connection=Connection}) ->
    ?SAY("Received..~p", [In]),
    Msg = {tcp_do_send, self(), Reply},
    gen_server:cast(Connection, Msg),
    {noreply, State};
handle_cast(In={fsm_delivery, _FSM, Email}, State) ->
    ?SAY("Received..~p", [In]),
    ?SAY("Got email...~n~p", [Email]),
    {noreply, State};
handle_cast(In={fsm_end, FSM}, State=#state{fsm=FSM,connection=Connection}) ->
    ?SAY("Received..~p", [In]),
    poolboy:checkin(fsm_pool, FSM),
    Msg = {tcp_do_close, self()},
    gen_server:cast(Connection, Msg),
    poolboy:checkin(controller_pool, self()),
    {noreply, State#state{fsm=undefined,connection=undefined}}.


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

%%%===================================================================
%%% Internal functions
%%%===================================================================
