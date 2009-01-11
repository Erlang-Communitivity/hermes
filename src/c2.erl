-module(c2).
-author("=Bill.Barnhill").

-export([start/0, start/1]).
-export([test/0,initial_docs/0, create_user_doc/3]).

-include("couch_db.hrl").

-define(DBNAME, "maildb").

start() ->
	start("./data").

start([A | _Rest] = DataDir) when is_integer(A) ->
	application:load(couch),
	application:set_env(couch, {"Couch","DbRootDir"}, DataDir),
	application:set_env(couch, {"Couch","Port"}, 6000).

test() ->
	couch_server:start(),
	couch_server:delete(?DBNAME),
	{ok, Db} = couchdb_embedded:dbopen(?DBNAME, initial_docs()),
	couchdb_embedded:list(Db).

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
	


