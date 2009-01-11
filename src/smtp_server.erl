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

-module(smtp_server).

-export([start/0, stop/0, start/2, stop/1]).

%% Callbacks.
-export([log_delivery/3, log_new_rcpt/2]).

-include("couch_db.hrl").

-define(DBNAME, "maildb").
-define(DATADIR, "./data").
-define(PORT, 6000).

start() -> 
	application:start(?MODULE).
	
stop() -> 
	application:stop(?MODULE),
	application:stop(couch).

start(normal, []) ->
	application:load(couch),
	application:set_env(couch, {"Couch","DbRootDir"}, ?DATADIR),
	application:set_env(couch, {"Couch","Port"}, ?PORT),
	couch_server:start(),
    {ok, Host} = application:get_env(listen_host),
    {ok, Port} = application:get_env(listen_port),
    {ok, Domain} = application:get_env(listen_domain),
    generic_tcp_server:start_link(smtp_server_session, Host, Port,
				  [list,
				   {active, false},
				   {packet, line},
				   {reuseaddr, true}],
				  [{?MODULE, log_delivery, []},
				   {?MODULE, log_new_rcpt, []},
				   Domain
				  ]).

stop(_State) ->
    ok.

log_new_rcpt(_ReversePath, [Rcpt | _Rest]) ->
    io:format("New recipient: ~p~n", [Rcpt]),
    ok.

log_delivery(ReversePath, ForwardPaths, DataLines) ->
    {rfc2822, Headers, BodyLines} = Message = rfc2822:parse(DataLines),
    io:format("SMTP delivery:~n - reverse path ~p~n - forward paths ~p~n - ~p~n", [ReversePath, ForwardPaths, Message]),
	{ok, Db} = couchdb_embedded:dbopen(?DBNAME, initial_docs()),
	MailDocBody = {obj, Headers ++ [{body, list_to_tuple(BodyLines)}]},
	MailDoc = #doc{id = couch_util:new_uuid(), revs = ["0"], body = MailDocBody},
	Options = [],
	couch_db:save_docs(Db, [MailDoc], Options),
    ok.
	
initial_docs() ->
	[
		create_user_doc("Bill Barnhill", {"bill.barnhill@communitivity.com"}, {"admin","user"})
		
	].
	
create_user_doc(Name, EmailAddresses, Roles) ->
	#doc{id = couch_util:new_uuid(), revs = ["0"], body = {obj, [
		{"username", Name},
		{"addresses", EmailAddresses},
		{"added", erlang:universaltime() },
		{"roles", Roles}
	]}}.
	
