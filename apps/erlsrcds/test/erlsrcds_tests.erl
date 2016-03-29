-module(erlsrcds_tests).
-include_lib("eunit/include/eunit.hrl").

info_timeout_test() ->
    Address = "8.8.8.8",
    Port = 27015,
    Result = erlsrcds:info(Address, Port),
    ?_assert({error, timeout} =:= Result).

player_timeout_test() ->
    Address = "8.8.8.8",
    Port = 27015,
    Result = erlsrcds:player(Address, Port),
    ?_assert({error, timeout} =:= Result).

rules_timeout_test() ->
    Address = "8.8.8.8",
    Port = 27015,
    Result = erlsrcds:rules(Address, Port),
    ?_assert({error, timeout} =:= Result).
