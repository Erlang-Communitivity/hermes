-module(storing_smtp_handler).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

init([]) ->
  {ok, []}.

handle_event(Event, State) ->
  error_logger:info_msg("Unknown event rcvd, ignoring...~n~p~n", [Event]),
  {ok, State}.

% This returns true if and only if one of the following is true:
%  a) The {User, Domain} of the Path matches the "address" field of a Doc with "type" of "user"
%  b) (return false until we enable use for outgoing mail), the reverse path has a User address match
% We really need to use views, otherwise enum'ing docs has no cacheing and is big hit
%
handle_call({verify, ReversePath, {Name, Domain} = Path}, State) ->
	PathLC = {string:to_lower(Name), string:to_lower(Domain)},
	Result = smtp_server:doc_for_address(PathLC),
	error_logger:info_msg("doc_for_address returned ~p", [Result]),
	case Result of
	 	{ok, none} -> error_logger:info_msg("Invalid recipient, no user doc: ~p~n", [Path]),
				{ok, false, State};
		{ok, Doc} -> 
			error_logger:info_msg("New recipient: ~p~n-- Start doc --:~n~p~n--End doc--~n", [Path, Doc]),
			{ok, true, State}
	end;

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
