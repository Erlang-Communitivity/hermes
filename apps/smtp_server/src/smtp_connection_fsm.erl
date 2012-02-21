%%%-------------------------------------------------------------------
%%% @author Bill Barnhill <>
%%% @copyright (C) 2012, Bill Barnhill
%%% @doc
%%%
%%% @end
%%% Created : 11 Feb 2012 by Bill Barnhill <>
%%%-------------------------------------------------------------------
-module(smtp_connection_fsm).

-behaviour(gen_fsm2).

%% API
-export([start_link/0,
	 start/0,
	 tcp_ready/2,
	 tcp_closed/2,
	 tcp_error/2,
	 smtp_begin/2,
	 smtp_data/2,
	 smtp_deliver/2
]).

%% gen_fsm2 callbacks
-export([init/1, 
	 handle_event/3,
	 handle_sync_event/4, 
	 handle_info/3, 
	 terminate/3, 
	 code_change/4,	 
	 state_change/3
	]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include_lib("smtp_server.hrl").

-define(SERVER, ?MODULE).

-define(REPLY(M,S), ((S#state.reply_fn)(S#state.conn_server, M)) ).


-record(state, {conn_server, message, reply_fn, deliver_fn, quit_fn}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_fsm2:start_link(?MODULE, [], []).

start() ->
    gen_fsm2:start(?MODULE, [], []).

%%%===================================================================
%%% gen_fsm2 callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init([]) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, tcp_ready, #state{}}.

state_change(EndStateName, none, State) ->
    ?SAY("-~w (final state)",[EndStateName]),
    {next_state, State}; 
state_change(none, FirstStateName, State) ->
    ?SAY("+~w (initial state)",[FirstStateName]),
    {next_state, State};
state_change(OldStateName, smtp_deliver, State) ->
    ?SAY("-~w",[OldStateName]),
    ?SAY("+smtp_deliver"),
    Msg = State#state.message,
    FromLine = ["From: ",Msg#message.from],
    RecipientList = lists:revers(lists:foldl(fun (R, Acc) -> 
						  [";" | [R | Acc]]
			     end, 
			     [],
			     Msg#message.recipients)),
    Body = Msg#message.data,
    Email = [FromLine,RecipientList, Body],
    ?SAY("Got following email...~s~n", [Email]),
    gen_fsm2:trigger_state_change(self(), smtp_begin),
    {next_state, State#state{message=#message{}}};

state_change(OldStateName, NewStateName, State) ->
    ?SAY("-~w",[OldStateName]),
    ?SAY("+~w",[NewStateName]),
    {next_state, State}.
    
tcp_ready( Ev={tcp_opened, ConnectionServer}, State) ->
    ?SAY(" tcp_ready > ~p",[Ev]),
    ReplyFn = case State#state.reply_fn of
		  none -> fun ConnectionServer:reply/2;
		  Fn -> Fn
	      end,
    State1 = State#state{conn_server=ConnectionServer, reply_fn=ReplyFn},
    ?REPLY("220 localhost Erlang SMTP Server Experiment", State1),
    {next_state, smtp_begin, State1}.

tcp_closed(Ev, State) ->
    ?SAY(" tcp_closed > ~p",[Ev]),
    {next_state, tcp_closed,State}.
    
tcp_error(Ev, State) ->
    ?SAY(" tcp_error > ~p",[Ev]),
    {next_state, tcp_error,State}.


smtp_begin( Ev={tcp, _ConnectionServer, Data}, State) ->
    ?SAY(" smtp_begin > ~p",[Ev]),
    CmdStr = string:to_lower(string:sub_word(Data,1)),
    {Reply, StateResult, StateName} = case CmdStr of
					  "helo" -> 
					      case string:sub_word(Data,2) of
						  [] ->
						      {"501 HELO requires domain address", State, smtp_begin};
						  _ ->
						      {"250 OK", State, smtp_begin}
					      end;  
					  "mail" -> 
					      Addr = case Data of
							 "MAIL FROM: <"++Addr1 -> string:left(Addr1, string:len(Addr1)-1);
							 "MAIL FROM:<"++Addr1 -> string:left(Addr1, string:len(Addr1)-1);
							 "mail from: <"++Addr1 -> string:left(Addr1, string:len(Addr1)-1);
							 "mail from:<"++Addr1 -> string:left(Addr1, string:len(Addr1)-1);
							 "MAIL FROM: "++Addr1 -> Addr1;
							 "mail from: "++Addr1 -> Addr1;
							 "MAIL FROM:"++Addr1 -> Addr1;
							 "mail from:"++Addr1 -> Addr1
						     end,
					      %% Yes, we do not handle routes at present
					      %% Yes, no checking for relaying yet. This alone means DO NOT USE in production yet
					      %% We should be verifying address here
					      State1 = State#state{message=#message{from=Addr}},
					      {"250 OK",State1, smtp_begin};
					  "rcpt" ->
					      ?SAY("Got RCPT"),
					      case State#state.message of
						  none -> {"503 Bad sequence of commands", State, smtp_begin};
						  _ ->
						      Addr = case Data of
								 "RCPT TO: <"++Addr1 -> string:left(Addr1, string:len(Addr1)-1);
								 "RCPT TO:<"++Addr1 -> string:left(Addr1, string:len(Addr1)-1);
								 "rcpt to: <"++Addr1 -> string:left(Addr1, string:len(Addr1)-1);
								 "rcpt to:<"++Addr1 -> string:left(Addr1, string:len(Addr1)-1);
								 "RCPT TO: "++Addr1 -> Addr1;
								 "rcpt to: "++Addr1 -> Addr1;
								 "RCPT TO:"++Addr1 -> Addr1;
								 "rcpt to:"++Addr1 -> Addr1;
								 Other -> ?SAY("Invaid RCPT:~s", [Data])
							     end,
						      ?SAY("Parsed RCPT addr: '~s'", [Addr]),
						      Message = State#state.message,
						      ?SAY("Current message: '~p'", [Message]),
						      Recipients = [Addr | Message#message.recipients],
						      ?SAY("New recipients list: '~p'", [Recipients]),
						      State1 = State#state{message=Message#message{recipients=Recipients}},
						      ?SAY("New message: '~p'", [State#state.message]),
						      {"250 "++Addr++"... Recipient ok (will queue)", State1, smtp_begin}
					      end;
					  "data" ->
					      {"354 Enter mail, end with \“.\” on a line by itself", State, smtp_data};
					  "quit" ->
					      (State#state.quit_fn)(State#state.conn_server),
					      {none, #state{}, tcp_ready};
					  OtherCmd ->
					      {"502 Unimplemented command "++OtherCmd, State, smtp_begin}
				      end,
    case Reply of 
	none -> ok;
	ReplyText -> ?REPLY(Reply, StateResult)
    end,
    {next_state, StateName, StateResult}.

smtp_data( _Ev={tcp, _ConnectionServer, ".."++Data}, State) ->
    ?SAY("smtp_data, RECV >~p",[".."++Data]),
    Message =  State#state.message,
    Message1 = Message#message{data=["."++Data | Message#message.data]}, %% remember this needs to be reverse when data portion ends
    {next_state, smtp_data, State#state{message=Message1} };
smtp_data( _Ev={tcp, _ConnectionServer, "."++Data}, State) ->
    ?SAY("smtp_data, RECV >~p",["."++Data]),
    Message =  State#state.message,
    Message1 = Message#message{data=lists:reverse(Message#message.data)},
    case State#state.deliver_fn of
	undefined -> ok;
	DeliveryFn -> DeliveryFn(Message1)
    end,
    {next_state, smtp_begin, State#state{message=undefined} };
smtp_data( _Ev={tcp, _ConnectionServer, Data}, State) ->
    ?SAY("smtp_data, RECV >~p",[Data]),
    Message =  State#state.message,
    Message1 = Message#message{data=[Data | Message#message.data]}, %% remember this needs to be reverse when data portion ends
    {next_state, smtp_data, State#state{message=Message1}}.

smtp_deliver(Ev, State) ->
    ?SAY("smtp_deliver > ~p",[Ev]),
    {next_state, smtp_begin, State}.
    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ?SAY("Got to terminate"),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



-ifdef(TEST).

wait_for_exit(Pid) ->
    MRef = erlang:monitor(process, Pid),
    receive {'DOWN', MRef, _, _, _} -> ok end.

send_events(FSM, EventList) ->
    Fun = fun(Event) ->
		  gen_fsm2:send_event(FSM, Event)
	  end,
    lists:foreach(Fun, EventList),
    ok.

expecter(Expected) ->
    expecter(Expected, #state{}).
expecter(Expected, State) ->
    ReplyFn = fun(_,Received) ->
		      ?assertEqual(Expected, Received)
	      end,
    State#state{reply_fn=ReplyFn}.
    
connection_test() ->
    Result = tcp_ready({tcp_opened, dummy_conn_server}, expecter("220 localhost Erlang SMTP Server Experiment")),
    ?assertMatch({next_state, smtp_begin, #state{conn_server=dummy_conn_server,message=_,reply_fn=_}}, Result).

helo_test() ->
    Result = smtp_begin({tcp, dummy_conn_server, "HELO example.org"}, expecter("250 OK")),
    ?assertMatch({next_state, smtp_begin, #state{conn_server=_,message=_,reply_fn=_}}, Result).
    
mail_test() ->
    Result = smtp_begin({tcp, dummy_conn_server, "MAIL FROM: <alice@example.org>"}, expecter("250 OK")),
    ?assertMatch({next_state, smtp_begin, #state{conn_server=_,message=#message{from="alice@example.org"},reply_fn=_}}, Result).

rcpt_test() ->
    State0 = #state{message=#message{from="alice@example.org"}},
    Result = smtp_begin({tcp, dummy_conn_server, "RCPT TO: <bob@example.org>"}, expecter("250 bob@example.org... Recipient ok (will queue)", State0)),
    ?assertMatch({next_state, smtp_begin, _}, Result),
    {_, _, #state{message=Message}} = Result,
    ?assertMatch(#message{from="alice@example.org",recipients=["bob@example.org"],data=[]}, Message).

data_test() ->
    State0 = #state{message=#message{from="alice@example.org",recipients=["bob@example.org"],data=[]}},
    State1 = #state{message=#message{from="alice@example.org",recipients=["bob@example.org"],data=["Hi Bob,"]}},
    State2 = #state{message=#message{from="alice@example.org",recipients=["bob@example.org"],data=["This is a test email.","Hi Bob,"]}},
    State3 = #state{message=#message{from="alice@example.org",recipients=["bob@example.org"],data=[".xxx","This is a test email.","Hi Bob,"]}},
    State4 = #state{message=#message{from="alice@example.org",recipients=["bob@example.org"],data=["-Alice",".xxx","This is a test email.","Hi Bob,"]}},
    State5 = #state{message=undefined},

    Result0 = smtp_begin({tcp, dummy_conn_server, "DATA "}, expecter("354 Enter mail, end with \“.\” on a line by itself", State0)),
    ?assertMatch({next_state, smtp_data, _}, Result0),
    {next_state, smtp_data,ResultState0} = Result0,
    ?assertEqual(State0, ResultState0#state{reply_fn=undefined}),
 
    Result1 = smtp_data({tcp, dummy_conn_server, "Hi Bob,"}, expecter("", State0)),
    ?assertMatch({next_state, smtp_data, _}, Result1),
    {next_state, smtp_data,ResultState1} = Result1,
    ?assertEqual(State1, ResultState1#state{reply_fn=undefined}),

    Result2 = smtp_data({tcp, dummy_conn_server, "This is a test email."}, expecter("", State1)),
    ?assertMatch({next_state, smtp_data, _}, Result2),
    {next_state, smtp_data,ResultState2} = Result2,
    ?assertEqual(State2, ResultState2#state{reply_fn=undefined}),

    Result3 = smtp_data({tcp, dummy_conn_server, "..xxx"}, expecter("", State2)),
    ?assertMatch({next_state, smtp_data, _}, Result3),
    {next_state, smtp_data,ResultState3} = Result3,
    ?assertEqual(State3, ResultState3#state{reply_fn=undefined}),

    Result4 = smtp_data({tcp, dummy_conn_server, "-Alice"}, expecter("", State3)),
    ?assertMatch({next_state, smtp_data, _}, Result4),
    {next_state, smtp_data,ResultState4} = Result4,
    ?assertEqual(State4, ResultState4#state{reply_fn=undefined}),

    DeliverFn = fun(Message) ->
			?assertMatch(#message{from="alice@example.org",recipients=["bob@example.org"],data=["Hi Bob,","This is a test email.",".xxx","-Alice"]},Message)
		end,
    Result5 = smtp_data({tcp, dummy_conn_server, "."}, expecter("", State4#state{deliver_fn=DeliverFn})),
    ?assertMatch({next_state, smtp_begin, _}, Result5),
    {next_state, smtp_begin,ResultState5} = Result5,
    ?assertEqual(State5, ResultState5#state{reply_fn=undefined,deliver_fn=undefined}).

since_epoch() ->                            
       {Mega, Secs, Micro} = erlang:now(),         
       (Mega * 1000000) + Secs + (Micro / 1000000).

quit_test() ->
    ets:new(calls, [ordered_set, named_table]),
    QuitFn = fun(Name) ->
		     ets:insert(calls, {since_epoch(), quit_fn, [Name]})
	     end,
    Result = smtp_begin({tcp, dummy_conn_server, "QUIT"}, expecter("", #state{conn_server=dummy_conn_server,quit_fn=QuitFn})),
    ?assertMatch({next_state, tcp_ready, #state{}}, Result),
    ?assertEqual(1, ets:info(calls,size)),
    ?assertMatch([{_,quit_fn, [dummy_conn_server]}], ets:lookup(calls, ets:first(calls))),
    ets:delete(calls).
    

-endif.
