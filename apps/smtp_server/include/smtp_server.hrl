-module(smtp_server).

-define(SAY(Msg), io:format("~s:~p > ~p~n",[?FILE,?LINE,Msg])).

-define(SAY(Format, Args), io:format("~s:~p > "++Format++"~n",[?FILE,?LINE]++Args)).

-define(CRLF, "\r\n").

-record(message, {from, recipients=[], data=[]}).

-record(conn_spi, {reply_fn, closed_fn, delivery_fn}).
