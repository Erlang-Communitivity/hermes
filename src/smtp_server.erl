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
-export([add_user/3, populate_db/0, doc_for_address/1]).

-include("couch_db.hrl").

-define(DBNAME, "maildb").
-define(DATADIR, "./data").
-define(PORT, 6000).

start() -> 
	application:start(?MODULE).
	
stop() -> 
	application:stop(?MODULE).
	%%application:stop(couch).

start(normal, []) ->
	%%application:load(couch),
	%%application:set_env(couch, {"Couch","DbRootDir"}, ?DATADIR),
	%%application:set_env(couch, {"Couch","Port"}, ?PORT),
	%%couch_server:start(),
	couch_embedded:start(),
    {ok, Host} = application:get_env(listen_host),
    {ok, Port} = application:get_env(listen_port),
    {ok, Domain} = application:get_env(listen_domain),
    generic_tcp_server:start_link(smtp_server_session, Host, Port,
				  [list,
				   {active, false},
				   {packet, line},
				   {reuseaddr, true}],
				  [ [logging_smtp_handler, storing_smtp_handler], Domain ]
				).

stop(_State) ->
    ok.

add_user(Name, Address, Roles) ->
	RolesJson = list_to_tuple(Roles),
	AddressesJson = {Address},
	%% FIXME Should check for duplicates here!
	Doc = #doc{id = couch_util:new_uuid(), revs = ["0"], body = {obj, [
		{"type", "user"},
		{"username", Name},
		{"addresses", AddressesJson},
		{"added", erlang:universaltime() },
		{"roles", RolesJson}
	]}},
	{ok, Db} = couch_embedded:dbopen(?DBNAME, []),
	couch_db:save_docs(Db, [Doc], []).
	
populate_db() ->
	add_user("Bill Barnhill", {"bill.barnhill","communitivity.com"}, ["admin", "user"]).
	

doc_for_address({Userid, Domain} = Address) ->
	{ok, Db} = couch_embedded:dbopen(?DBNAME, []),
	Map_Fn = fun (#full_doc_info{id=Id}=Info, _Offset, Acc) ->
		case Acc of
			none -> 
				{ok, #doc{body={obj, Fields}}} = {ok, Doc} = couch_db:open_doc(Db, Id),
				case proplists:get_value("type", Fields, none) of
					"user" ->
						Addresses = tuple_to_list(proplists:get_value("addresses", Fields, {})),
						case lists:member(Address, Addresses) of
							true -> {ok, Doc};
							false -> {ok, none}
						end;
					_Other -> {ok, none}
				end;
			Doc -> {ok, Doc}
		end
	end,
	couch_db:enum_docs(Db, 0, Map_Fn, none).

	
%%log_delivery(ReversePath, ForwardPaths, DataLines) ->
%%    {rfc2822, Headers, BodyLines} = Message = rfc2822:parse(DataLines),
%%    io:format("SMTP delivery:~n - reverse path ~p~n - forward paths ~p~n - ~p~n", [ReversePath, ForwardPaths, Message]),
%%	{ok, Db} = couchdb_embedded:dbopen(?DBNAME, initial_docs()),
%%	MailDocBody = {obj, Headers ++ [{body, list_to_tuple(BodyLines)}]},
%%	MailDoc = #doc{id = couch_util:new_uuid(), revs = ["0"], body = MailDocBody},
%%	Options = [],
%%	couch_db:save_docs(Db, [MailDoc], Options),
%%  ok.
	
	
