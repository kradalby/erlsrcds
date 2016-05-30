-module(erlsrcds_qc_sv_cheats).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").


-define(SERVER, "127.0.0.1").
-define(PORT, 27015).
-define(PASSWORD, "passwrd").

-record(state, {
          rules = #{},
          cheats = 0
}).

rule_map(default) ->
    rule_map(1);

rule_map(generator) ->
    rule_map(2);

rule_map(Element) ->
    #{
       "sv_pushaway_max_hostage_force" => element(Element, {"1000", choose(100, 2000)}),
       "sv_noclipduringpause" => element(Element, {"0", choose(0, 1)}),
       "sv_showlagcompensation" => element(Element, {"0", choose(0, 1)}),
       "sv_pushaway_hostage_force" => element(Element, {"20000", choose(100, 100000)}),
       "cs_ShowStateTransitions" => element(Element, {"-2", choose(-2, 1)})
     }.
    

rule_default_value(Rule) ->
    element(1, maps:get(Rule)).

rule_generator() ->
    Commands = rule_map(generator),
    ?LET(
       Command,
       oneof(maps:keys(Commands)),
       [
          Command,
          ?LET(
             Value,
             maps:get(Command, Commands),
             integer_to_list(Value)
            )]
      ).


weight(_, toggle_cheats) ->
    1;
weight(_, compare_rule) ->
    8;
weight(_, change_rule) ->
    4.


initial_state() ->
    erlsrcds:rcon("sv_cheats 0", ?PASSWORD, ?SERVER, ?PORT),
    #state{}.


%% Change sv_cheats
%% If cheats is turned up, change the whole state to default
toggle_cheats_args(#state{cheats = Cheats}) ->
    [Cheats].

toggle_cheats(Cheats) ->
    case Cheats of
        1 ->
            erlsrcds:rcon("sv_cheats 0", ?PASSWORD, ?SERVER, ?PORT);
        0  ->
            erlsrcds:rcon("sv_cheats 1", ?PASSWORD, ?SERVER, ?PORT)
    end.

toggle_cheats_next(State, _Var, [Cheats]) ->
    case Cheats of
        1 ->
            #state{rules = rule_map(default), cheats = 0};
        0 ->
            State#state{cheats = 1}
    end.

%% Check rule
compare_rule_pre(#state{rules = Rules}) ->
    length(maps:keys(Rules)) > 0.

compare_rule_args(#state{rules = Rules}) ->
    [oneof(maps:keys(Rules))].

compare_rule(Command) ->
    erlsrcds:parse_rcon(erlsrcds:rcon(Command, ?PASSWORD, ?SERVER, ?PORT)).

compare_rule_post(State = #state{rules = Rules}, [Command], Return) ->
    io:format("~nState: ~p ~n", [State]),
    io:format("~nCompare value: ~p and ~p~n", [maps:get(Command, Rules), Return]),
    maps:get(Command, Rules) =:= Return.


%% Change a rule
change_rule_args(_State) ->
    rule_generator().

change_rule(Rule, Value) ->
    Command = string:concat(Rule, string:concat(" ", Value)),
    erlsrcds:rcon(Command, ?PASSWORD, ?SERVER, ?PORT).

change_rule_next(State = #state{rules=Rules, cheats = Cheats}, _Return, [Rule, Value]) ->
    case Cheats of
        1 ->
            State#state{rules = maps:put(Rule, Value, Rules)};
        0 ->
            State
    end. 


%% Generation
sample() ->
    eqc_gen:sample(eqc_statem:commands(?MODULE)).


prop_p1() ->
    ?FORALL(Cmds, parallel_commands(?MODULE),
          begin
            {H, S, Res} = run_parallel_commands(?MODULE,Cmds),
            pretty_commands(?MODULE, Cmds, {H, S, Res},
              aggregate(command_names(Cmds),
                        Res == ok))
          end).

eqcp() ->
    eqc:quickcheck(?MODULE:prop_p1()).

eqcp_n(N) ->
    eqc:quickcheck(eqc:numtests(N, erlsrcds_qc_sv_cheats:prop_p1())).

prop_s1() ->
    ?FORALL(Cmds, commands(?MODULE),
          begin
            {H, S, Res} = run_commands(?MODULE,Cmds),
            pretty_commands(?MODULE, Cmds, {H, S, Res},
              aggregate(command_names(Cmds),
                        Res == ok))
          end).

eqc() ->
    eqc:quickcheck(?MODULE:prop_s1()).

eqc_n(N) ->
    eqc:quickcheck(eqc:numtests(N, erlsrcds_qc_sv_cheats:prop_s1())).
