-module(logging_smtp_handler).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

init([]) ->
  {ok, []}.

handle_event(Event, State) ->
  error_logger:info_msg("Unknown event rcvd, ignoring...~n~p~n", [Event]),
  {ok, State}.

handle_call({verify, ReversePath, Path}, State) ->
  error_logger:info_msg("New possible recipient: ~p~n", [Path]),
  Reply = true,
  {ok, Reply, State};

handle_call({deliver, ReversePath, ForwardPaths, Message}, State) ->
  MsgTpl = "SMTP delivery:~n - reverse path ~p~n - forward paths ~p~n - ~p~n",
  error_logger:info_msg(MsgTpl, [ReversePath, ForwardPaths, Message]),
  Reply = {ok, {deliver, ReversePath, ForwardPaths, Message}},
  {ok, Reply, State};

handle_call(Request, State) ->
  error_logger:info_msg("Unknown request rcvd, ignoring...~n~p~n", [Request]),
  Reply = ok,
  {ok, Reply, State}.

handle_info(Info, State) ->
  error_logger:info_msg("Unknown message rcvd, ignoring...~n~p~n", [Info]),
  {ok, State}.

terminate(_Reason, _State) ->
  error_logger:info_msg("terminating...~n"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%log_delivery(ReversePath, ForwardPaths, DataLines) ->
%%    {rfc2822, Headers, BodyLines} = Message = rfc2822:parse(DataLines),
%%    io:format("SMTP delivery:~n - reverse path ~p~n - forward paths ~p~n - ~p~n", [ReversePath, ForwardPaths, Message]),
%%	{ok, Db} = couchdb_embedded:dbopen(?DBNAME, initial_docs()),
%%	MailDocBody = {obj, Headers ++ [{body, list_to_tuple(BodyLines)}]},
%%	MailDoc = #doc{id = couch_util:new_uuid(), revs = ["0"], body = MailDocBody},
%%	Options = [],
%%	couch_db:save_docs(Db, [MailDoc], Options),
%%  ok.
