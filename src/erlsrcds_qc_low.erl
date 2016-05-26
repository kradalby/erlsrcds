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
    #state{}.
