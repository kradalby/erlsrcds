-module(cli).

-export([
    info/0,
    player/0
]).

info() ->
    erlsrcds:info("193.202.115.74", 27115).
player() ->
    erlsrcds:player("193.202.115.74", 27115).
