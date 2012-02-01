%%---------------------------------------------------------------------------
%% Copyright (c) 2007 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
%% Copyright (c) 2007 LShift Ltd. <query@lshift.net>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------

-module(smtp_server_session).
%% Somewhat loosely based on rfc 2821.
%% Doesn't even begin to address rfc 2822 in any serious way.

%% FIXME: SMTP AUTH

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-record(session, {socket, mode, reverse_path, forward_paths, data_buffer,domain}).

reply_line(Code, Text, false) ->
    [integer_to_list(Code), " ", Text, "\r\n"];
reply_line(Code, Text, true) ->
    [integer_to_list(Code), "-", Text, "\r\n"].



reply(Code, Text, State = #session{socket = Socket}) ->
    gen_tcp:send(Socket, reply_line(Code, Text, false)),
    State.

reply_fn_for_socket(Socket) ->
	fun (Code, Text) -> 
		gen_tcp:send(Socket, [integer_to_list(Code), " ", Text, "\r\n"])
	end.
	
reset_buffers(State) ->
    State#session{reverse_path = undefined,
		  forward_paths = [],
		  data_buffer = []}.



%% event handler should return true if verified, false otherwise
verify_with_handlers(ReversePath, Path, Handlers) ->
	error_logger:info_msg("+verify_with_handlers/3 ~p, ~p, ~p", [ReversePath, Path, Handlers]),
	lists:foldl(
		fun (Handler, Acc) ->
			error_logger:info_msg("Called with ~p, ~p", [Handler, Acc]),
			case Acc of
				false -> false;
				true ->
					Request = {verify, ReversePath, Path},
					Result = gen_event:call(?MODULE, Handler, Request),
					error_logger:info_msg("Called Handler with ~p and result of ~p", [Request, Result]),
					Result
			end
		end,
		true, Handlers).
	

%% Event handler should return either
%%	 {ok, {ReversePath, MailBoxes, DataLines}}
%% or
%%   {error, Reason}
%%
deliver_to_handlers(ReversePath, ForwardPaths, DataLines, Handlers) ->
	error_logger:info_msg("+deliver_to_handlers/3 ~p, ~p, ~p", [ReversePath, ForwardPaths, DataLines]),
	lists:foldl(
		fun (Handler, Request) ->
			error_logger:info_msg("Called with ~p, ~p", [Handler, Request]),
			case Request of
				{ok, {deliver, _NextReversePath, _NextForwardPaths, _NextDataLines} = NextRequest} ->
					Result = gen_event:call(?MODULE, Handler, NextRequest),
					error_logger:info_msg("Called Handler with ~p and result of ~p", [NextRequest, Result]),
					Result;
				{error, Reason} ->
					{error, Reason}
			end
		end,
		{ok, {deliver, ReversePath, ForwardPaths, DataLines}}, 
		Handlers).


handle_command_line(Line, State) ->
    {Command, Data} = case httpd_util:split(Line, " ", 2) of
			  {ok, [C]} -> {string:to_upper(C), ""};
			  {ok, [C, D]} -> {string:to_upper(C), D}
		      end,
    handle_command(Command, Data, State).

handle_command("QUIT", _ClientDomain, State) ->
    {stop, normal, reply(221, "Goodbye",
			 reset_buffers(State))};

handle_command("EHLO", _ClientDomain, State) ->
    ServerDomain = State#session.domain, 
    {noreply, reply(250, ServerDomain ++ " You have reached an SMTP service",
		    reset_buffers(State))};

handle_command("HELO", _ClientDomain, State) ->
    ServerDomain = State#session.domain, 
    {noreply, reply(250, ServerDomain ++ " You have reached an SMTP service",
		    reset_buffers(State))};

handle_command("MAIL", FromReversePathAndMailParameters, State) ->
    case smtp_util:parse_path_and_parameters("[fF][rR][oO][mM]:", FromReversePathAndMailParameters) of
	unintelligible ->
	    {noreply, reply(553, "Unintelligible reverse-path", State)};
	{ok, Path, _Params} ->
	    {noreply, reply(250, "OK",
			    State#session{reverse_path = Path})}
    end;
	

handle_command("RCPT", ToForwardPathAndMailParameters,
	       State = #session{reverse_path = ReversePath,	forward_paths = ForwardPaths}) ->
    if
		ReversePath == undefined ->
	    	{noreply, reply(503, "MAIL first", State)};
		true ->
	    	case smtp_util:parse_path_and_parameters("[tT][oO]:", ToForwardPathAndMailParameters) of
				unintelligible ->
		    		{noreply, reply(553, "Unintelligible forward-path", State)};
				{ok, Path, _Params} ->
					%% Quirk in gen_event..handlers returned in reverse order added
					Handlers = lists:reverse(gen_event:which_handlers(?MODULE)),
					Verified = verify_with_handlers(ReversePath, Path, Handlers),
					case Verified of
						true ->
							NextState = State#session{forward_paths = [Path | ForwardPaths]},
			    			{noreply, reply(250, "OK", NextState)};
						false ->
							{User, Domain} = Path,
			    			ErrMsg = io_lib:format("Unacceptable recipient ~s", [User++"@"++Domain]),
							{noreply, reply(550, ErrMsg, State)};
						Other -> exit("Unknown verify_with_handlers return: ~p", [Other])
					end;
				OuterOther -> exit("Unknown parse_path_and_parameters return: ~p", [OuterOther])
	    	end
    end;

handle_command("DATA", _Junk, State = #session{forward_paths = ForwardPaths}) ->
    if
	ForwardPaths == [] ->
	    {noreply, reply(503, "RCPT first", State)};
	true ->
	    {noreply, reply(354, "Go ahead", State#session{mode = data})}
    end;

handle_command("RSET", _Junk, State) ->
    {noreply, reply(250, "OK",
		    reset_buffers(State))};

handle_command("VRFY", _UserOrMailboxPossibly, State) ->
    {noreply, reply(252, "Will not VRFY", State)};

handle_command("EXPN", _MailingListPossibly, State) ->
    {noreply, reply(252, "Will not EXPN", State)};

handle_command("HELP", _MaybeCommand, State) ->
    {noreply, reply(502, "Unimplemented", State)};

handle_command("NOOP", _Junk, State) ->
    {noreply, reply(250, "OK", State)};

handle_command(Command, _Data, State) ->
    {noreply, reply(500, "Unsupported command " ++ Command, State)}.

handle_data_line(".\r\n", State = #session{reverse_path = ReversePath,
					   forward_paths = ForwardPaths,
					   data_buffer = DataBuffer}) ->
	Handlers = lists:reverse(gen_event:which_handlers(?MODULE)),
	{Code, Text} = case deliver_to_handlers(ReversePath, ForwardPaths, lists:reverse(DataBuffer), Handlers) of
						{ok, _Result} -> {250, "OK"};
						{error, Reason} -> {554, "Transaction Failed -" ++ Reason}
					end,
	{noreply, reply(Code, Text, reset_buffers(State#session{mode = command}))};
handle_data_line("." ++ Line, State) ->
    accumulate_line(Line, State);
handle_data_line(Line, State) ->
    accumulate_line(Line, State).

accumulate_line(Line, State = #session{data_buffer = Buffer}) ->
    {noreply, State#session{data_buffer = [Line | Buffer]}}.

deliver({M,F,A}, ReversePath, Mailboxes, DataLinesRev) ->
    case catch apply(M, F, [ReversePath, Mailboxes, lists:reverse(DataLinesRev) | A]) of
	{'EXIT', Reason} ->
	    error_logger:error_msg("SMTP delivery callback failed: ~p", [Reason]),
	    callback_failure;
	Result -> Result
    end.

add_handler(Module) ->
  gen_event:add_handler(?MODULE, Module, []).

notify(Event) ->
  gen_event:notify(?MODULE, Event).

%---------------------------------------------------------------------------

init([Sock, Handlers, Domain]) ->
	gen_event:start_link({local, ?MODULE}),
	lists:map(fun add_handler/1, Handlers),
	Session = new_session(Sock, Domain),
    {ok, reset_buffers(Session)}.
				
new_session(Sock, Domain) ->
	#session{socket = Sock,
				mode = initializing,
				domain = Domain}.

terminate(_Reason, #session{socket = Sock}) ->
    gen_tcp:close(Sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_call(Request, _From, State) ->
    {stop, {bad_call, Request}, State}.

handle_cast({socket_control_transferred, _Sock}, State = #session{socket = Sock}) ->
    inet:setopts(Sock, [{active, true}]),
    {noreply, reply(220, "Hi there", State#session{mode = command})};

handle_cast(Request, State) ->
    {stop, {bad_cast, Request}, State}.

handle_info({tcp, _Sock, FullLine}, State = #session{mode = command}) ->
    handle_command_line(smtp_util:strip_crlf(FullLine), State);

handle_info({tcp, _Sock, FullLine}, State = #session{mode = data}) ->
    handle_data_line(FullLine, State);

handle_info({tcp_closed, _Sock}, State) ->
    %%error_logger:warning_msg("SMTP session closed without warning"),
    {stop, normal, State};

handle_info({tcp_error, _Sock, Reason}, State) ->
    error_logger:warning_msg("SMTP session closed with socket error ~p", [Reason]),
    {stop, normal, State};

handle_info(Message, State) ->
    {stop, {bad_info, Message}, State}.
