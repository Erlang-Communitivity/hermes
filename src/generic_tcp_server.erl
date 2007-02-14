-module(generic_tcp_server).

-behaviour(gen_server).

-export([start_link/5]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Module, Host, Port, ListenOpts, ModuleOpts) ->
    gen_server:start_link(?MODULE, [Module, Host, Port, ListenOpts, ModuleOpts], []).

%---------------------------------------------------------------------------

accept_and_start(Module, ModuleOpts, LSock) ->
    spawn_link(fun () ->
		       case gen_tcp:accept(LSock) of
			   {ok, Sock} ->
			       accept_and_start(Module, ModuleOpts, LSock),
			       {ok, Pid} = gen_server:start(Module, [Sock | ModuleOpts], []),
			       gen_tcp:controlling_process(Sock, Pid),
			       gen_server:cast(Pid, {socket_control_transferred, Sock});
			   {error, Reason} ->
			       exit({error, Reason})
		       end
	       end).

ip_listen_opt(any) ->
    [];
ip_listen_opt(Host) ->
    {ok, IP} = inet:getaddr(Host, inet),
    [{ip, IP}].

%---------------------------------------------------------------------------

init([Module, Host, Port, ListenOpts, ModuleOpts]) ->
    {ok, LSock} = gen_tcp:listen(Port, ip_listen_opt(Host) ++ ListenOpts),
    accept_and_start(Module, ModuleOpts, LSock),
    {ok, LSock}.

terminate(_Reason, State) ->
    LSock = State,
    gen_tcp:close(LSock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.
