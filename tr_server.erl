%%% @doc RPC over TPC server, from _Erlang and OTP in Action_
%%%   n.b. designed to be a singleton (p 107)
%%% @end

-module(tr_server).
-behavior(gen_server).

% API ... so what's this specific for? start_link/1 and /0 just conventions or what
-export([
         start_link/1,
         start_link/0,
         get_count/0,
         stop/0
        ]).

% behavior-specific callbacks
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-define(SERVER, ?MODULE). % may want to change; shouldn't assume server name will always remain same as module name
-define(DEFAULT_PORT, 1055).

-record(state, {port, lsock, request_count = 0}).


%%% API

%% @doc starts the server
%% @spec start_link(Port::integer()) -> {ok, Pid}
%% where
%%   Pid = pid()
%% @end
start_link(Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).
  % 1st param is where to start the new server's process
  % 2nd param is... how the link is established back to here?
  % 3rd param is args passed to the gen_server's init/1
  % 4th is extra options

%% @doc Calls `start_link(Port)` using default port.
%% @spec start_link() -> {ok, Pid}
start_link() ->
  start_link(?DEFAULT_PORT).

%% @doc Fetches number of requests made to server
%% @spec get_count() -> {ok, Count}
%% where
%%   Count = integer()
%% @end
get_count() ->
  gen_server:call(?SERVER, get_count). % n.b. gen_server:call/2 blocks, waiting for (default) 5 sec

%% @doc Stops the server.
%% @spec stop() -> ok
stop() ->
  get_server:cast(?SERVER, stop).


%%% gen_server callbacks

init([Port]) -> % convention to always pass a list to init/1
  {ok, LSock} = gen_tcp:listen(Port, [{active, true}]),
  {ok, #state{port = Port, lsock = LSock}, 0}. % immediate timeout returns to caller of start_link; handle_info(timeout) for any additional async processing

handle_call(get_count, _From, State) ->
  {reply, {ok, State#state.request_count}, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State) ->
  do_rpc(Socket, RawData),
  RequestCount = State#state.request_count,
  {noreply, State#state{request_count = RequestCount + 1}};
handle_info(timeout, #state{lsock = LSock} = State) ->
  {ok, _Sock} = gen_tcp:accept(LSock), % blocks this server listening for connection on socket
  {noreply, State}. % once a connection made, return unchanged (socket handle is still in State)

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%% internal functions

do_rpc(Socket, RawData) ->
  try
    {M, F, A} = split_out_mfa(RawData),
    Result = apply(M, F, A), % like js
    gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
  catch
    _Class:Err ->
      gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Err]))
  end.

split_out_mfa(RawData) ->
  MFA = re:replace(RawData, "\r\n$", "", [{return, list}]), % trim
  {match, [M, F, A]} = % capture Module, Function, Argument(s)
    re:run(MFA,
           "(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
           [{capture, [1,2,3], list}, ungreedy]),
  {list_to_atom(M), list_to_atom(F), args_to_terms(A)}.

args_to_terms(RawArgs) ->
  {ok, Toks, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
  {ok, Args} = erl_parse:parse_term(Toks),
  Args.
