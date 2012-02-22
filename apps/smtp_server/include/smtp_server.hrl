
-define(SAY(Msg), io:format("~n~s:~p > ~p\r\n",[?FILE,?LINE,Msg])).

-define(SAY(Format, Args), io:format("~n~s:~p > "++Format++"\r\n",[?FILE,?LINE]++Args)).

-define(CRLF, "\r\n").

-record(message, {from, recipients=[], data=[]}).

-record(conn_spi, {reply_fn, close_fn, closed_fn, delivery_fn}).
