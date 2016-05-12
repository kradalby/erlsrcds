-module(erlsrcds_tests).
-include_lib("eunit/include/eunit.hrl").

info_timeout_test() ->
    Address = "8.8.8.8",
    Port = 27115,
    Result = erlsrcds:info(Address, Port),
    ?assertEqual({error, timeout}, Result).

player_timeout_test() ->
    Address = "8.8.8.8",
    Port = 27115,
    Result = erlsrcds:player(Address, Port),
    ?assertEqual({error, timeout}, Result).

rules_timeout_test() ->
    Address = "8.8.8.8",
    Port = 27115,
    Result = erlsrcds:rules(Address, Port),
    ?assertEqual({error, timeout}, Result).

info_query_test() ->
    Address = "193.202.115.82",
    Port = 27115,
    Result = erlsrcds:info(Address, Port),
    Hostname = "Recess.no #12 Surf [ Tier 1-2 | Timer | 100 Tick ]",
    FetchedHostname = maps:get("hostname", Result),
    ?assertEqual(Hostname, FetchedHostname).

rules_query_test() ->
    Address = "193.202.115.82",
    Port = 27115,
    Result = erlsrcds:rules(Address, Port),
    Key = "sv_pausable",
    Value = maps:get(Key, Result),
    ?assertEqual(Value, "0").
