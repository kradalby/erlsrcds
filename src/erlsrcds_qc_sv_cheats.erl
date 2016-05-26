-module(erlsrcds_qc_sv_cheats).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").


-define(SERVER, "127.0.0.1").
-define(PORT, 27015).
-define(PASSWORD, "passwrd").

-record(state, {
          rules = #{},
          cheats = 1
}).


rule_map() ->
    #{
       "sv_pushaway_max_hostage_force" => {1000, choose(100, 2000)},
       "sv_noclipduringpause" => {0, choose(0, 1)},
       "sv_showlagcompensation" => {0, choose(0, 1)},
       "sv_pushaway_hostage_force" => {20000, choose(100, 100000)},
       "cs_ShowStateTransitions" => {-2, choose(-2, 1)}
     }.

rule_default_value(Rule) ->
    element(1, maps:get(Rule)).

rule_generator() ->
    Commands = rule_map(),
    ?LET(
       Command,
       oneof(maps:keys(Commands)),
       [
          Command,
          ?LET(
             Value,
             maps:get(Command, Commands),
             integer_to_list(element(2, Value))
            )]
      ).


initial_state() ->
    #state{}.

