-module(cli).

-export([
    info/0,
    player/0,
    rules/0
]).

info() ->
    erlsrcds:info("193.202.115.74", 27127).
player() ->
    erlsrcds:player("193.202.115.74", 27125).
rules() ->
    erlsrcds:rules("193.202.115.74", 27015).
