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

-module(smtp_util).

-export([
	 strip_crlf/1,
	 address_to_path/1,
	 split_path_from_params/1,
	 parse_path_and_parameters/2
	]).

address_to_path(Address) ->
    case regexp:match(Address, "[^@]+@") of
	{match, 1, Length} ->
	    {string:substr(Address, 1, Length - 1), string:substr(Address, Length + 1)};
	_ ->
	    Address
    end.

split_path_from_params(Str) ->
    case regexp:match(Str, "<[^>]*>") of
	{match, Start, Length} ->
	    Address = string:substr(Str, Start + 1, Length - 2),
	    Params = string:strip(string:substr(Str, Start + Length), left),
	    {address_to_path(Address), Params};
	_ ->
	    case httpd_util:split(Str, " ", 2) of
		{ok, [Address]} ->
		    {address_to_path(Address), ""};
		{ok, [Address, Params]} ->
		    {address_to_path(Address), Params}
	    end
    end.

parse_path_and_parameters(PrefixRegexp, Data) ->
    case regexp:first_match(Data, PrefixRegexp) of
	nomatch ->
	    unintelligible;
	{match, 1, Length} ->
	    PathAndParams = string:strip(string:substr(Data, Length + 1), left),
	    {Path, Params} = split_path_from_params(PathAndParams),
	    {ok, Path, Params}
    end.

strip_crlf(S) ->
    lists:reverse(strip_crlf1(lists:reverse(S))).

strip_crlf1([$\n, $\r | S]) -> S;
strip_crlf1([$\n | _S]) -> throw(improper_smtp_line_ending);
strip_crlf1(_S) -> throw(missing_smtp_line_ending).
