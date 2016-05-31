-module(erlsrcds_qc_low).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").


-define(SERVER, "127.0.0.1").
-define(PORT, 27015).
-define(PASSWORD, "passwrd").

-record(state, {
}).

initial_state() ->
    closed.

closed(State) ->
    [{listen, {}}].

listen(State) ->
    [{auth_sent, {}}].

auth_sent(State) ->
    [{auth_resp_recv, {}}].

auth_resp_recv(State) ->
    [
     {established, {}},
     {closed, {}}
    ].

established(State) ->
    [
     {command_sent, {}},
     {closed, {}}
    ].

command_sent(State) ->
    [{command_resp_recv, {}}].

command_resp_recv(State) ->
    [{closed, {}}].

