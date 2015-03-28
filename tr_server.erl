%%%-------------
%%% @doc RPC over TPC server, from _Erlang and OTP in Action_
%%%-------------

-module(tr_server).
-behavior(gen_server).

% API
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
