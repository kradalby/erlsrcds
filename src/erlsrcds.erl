-module(erlsrcds).

-export([
    info/2,
    player/2,
    rules/2
]).


-spec info(byte(), number()) -> #{}|{'error','timout'}.
info(Address, Port) ->
    {ok, AddressTuple} = inet_parse:address(Address),
    query:info_internal(AddressTuple, Port).

-spec player(byte(), number()) -> #{}|{'error','timout'}.
player(Address, Port) ->
    {ok, AddressTuple} = inet_parse:address(Address),
    query:player_internal(AddressTuple, Port).

-spec rules(byte(), number()) -> #{}|{'error','timout'}.
rules(Address, Port) ->
    {ok, AddressTuple} = inet_parse:address(Address),
    query:rules_internal(AddressTuple, Port).

