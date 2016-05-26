-module(erlsrcds_qc_state).

-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").


-define(SERVER, "127.0.0.1").
-define(PORT, 27015).
-define(PASSWORD, "passwrd").

-record(state, {
          rules = #{},
          map = ""
}).

rules_generator() ->
    Commands = #{
      "sv_gravity" => choose(100, 800),
      "sv_cheats" => choose(0, 1),
      "mp_timelimit" => choose(5, 100),
      "mp_startmoney" => choose(800, 16000),
      "mp_c4timer" => choose(10, 90),
      "mp_freezetime" => choose(0, 13),
      "mp_flashlight" => choose(0, 1),
      "sv_waterfriction" => choose(0, 1),
      "sv_airaccelerate" => choose(100, 1000),
      "sv_enablebunnyhopping" => choose(0, 1),
      "sv_maxspeed" => choose(100, 1000)
     },
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
            
maps_generator() ->
      oneof(["de_dust2", "cs_office", "cs_italy", "de_nuke", "de_inferno"]).


initial_state() ->
    #state{}.


change_rules_args(_State) ->
    rules_generator().

change_rules(Rule, Value) ->
    Command = string:concat(Rule, string:concat(" ", Value)),
    erlsrcds:rcon(Command, ?PASSWORD, ?SERVER, ?PORT).

change_rules_next(State = #state{rules=Rules}, _Return, [Rule, Value]) ->
    State#state{rules = maps:put(Rule, Value, Rules)}.


check_rules_pre(#state{rules = Rules}) ->
    length(maps:keys(Rules)) > 6.

check_rules_args(_State) ->
    [].

check_rules() ->
    erlsrcds:rules(?SERVER, ?PORT).

check_rules_post(#state{rules = Rules}, [], Return) ->
    case Return of
        {error, timeout} ->
            % io:format("UDP timeout rules ~n"),
            true;
        _ ->
            % io:format("rules succ~n"),
            lists:any(fun (Rule) ->
                              maps:get(Rule, Rules) == maps:get(Rule, Return)
                      end, maps:keys(Rules))
    end.


changelevel_pre(#state{rules = Rules}) ->
    length(maps:keys(Rules)) > 8.

changelevel_args(_State) ->
    [maps_generator()].

changelevel(Map) ->
    Command = string:concat("changelevel ", Map),
    erlsrcds:rcon(Command, ?PASSWORD, ?SERVER, ?PORT).

changelevel_next(_State, _Args, [Map]) -> 
    #state{rules=#{}, map=Map}.


checklevel_pre(#state{map = Map}) ->
    length(Map) > 0.
    
checklevel_args(_State) ->
    [].

checklevel() ->
    erlsrcds:info(?SERVER, ?PORT).    

checklevel_post(#state{map = Map}, [], Return) ->
    case Return of
        {error, timeout} ->
            % io:format("UDP timeout changelevel~n"),
            true;
        _ ->
            % io:format("changelevel succ~n"),    
            Map == maps:get("map", Return)
    end.


check_one_rule_pre(#state{rules = Rules}) ->
    length(maps:keys(Rules)) > 0.

check_one_rule_args(#state{rules = Rules}) ->
    [oneof(maps:keys(Rules))].

check_one_rule(Command) ->
    erlsrcds:parse_rcon(erlsrcds:rcon(Command, ?PASSWORD, ?SERVER, ?PORT)).

check_one_rule_post(#state{rules = Rules}, [Command], Return) ->
    maps:get(Command, Rules) == Return.


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
    eqc:quickcheck(eqc:numtests(N, erlsrcds_qc_state:prop_p1())).

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
    eqc:quickcheck(eqc:numtests(N, erlsrcds_qc_state:prop_s1())).
