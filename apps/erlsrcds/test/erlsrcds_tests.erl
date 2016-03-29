-module(erlsrcds_tests).
-include_lib("eunit/include/eunit.hrl").

test_info_timeout() ->
    Address = "8.8.8.8",
    Port = 27015,
    Result = erlsrcds:info(Address, Port),
    ?_assert({error, timeout} =:= Result).

test_player_timeout() ->
    Address = "8.8.8.8",
    Port = 27015,
    Result = erlsrcds:player(Address, Port),
    ?_assert({error, timeout} =:= Result).

test_rules_timeout() ->
    Address = "8.8.8.8",
    Port = 27015,
    Result = erlsrcds:rules(Address, Port),
    ?_assert({error, timeout} =:= Result).
