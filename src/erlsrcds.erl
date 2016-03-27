-module(erlsrcds).

-export([
    test_info/0,
    test_player/0
]).

-define(PACKETSIZE, 1400).
-define(WHOLE, -1:32/signed).
-define(SPLIT, -2:32/signed).

-define(A2S_INFO, "T").
-define(A2S_INFO_STRING, "Source Engine Query").
-define(A2S_INFO_REPLY, "I").
-define(A2S_PLAYER, "U").
-define(A2S_PLAYER_REPLY, "D").
-define(A2S_RULES, "V").
-define(A2S_RULES_REPLY, "E").
-define(CHALLENGE, -1:32/signed).
-define(S2C_CHALLENGE, "A").

-define(STRING_TERMINATION, 16#00).

-define(UDP_OPTS, [
    binary,
    {active, false}
]).

%% Read the and return the next string from
%% the payload.
read_string(Payload) ->
    [String, NewPayload] = binary:split(Payload, [<<?STRING_TERMINATION>>], []),
    io:format("Reading string: ~s~n", [String]),
    {binary:bin_to_list(String), NewPayload}.

%% parse the packet header and
%% forward the payload to the
%% correct parse function.
parse_packet(Packet) when is_binary(Packet) ->

    case Packet of
        <<
            ?WHOLE,
            ?A2S_INFO_REPLY/utf8,
            _Protocol:8,
            Payload/binary
        >> ->
            io:format("Correct: ~s~n", [Payload]),
            parse_info_payload(Payload);

        <<
            ?WHOLE,
            ?S2C_CHALLENGE,
            Challenge:32
        >> ->
            io:format("Got PLAYER challenge~n"),
            Challenge;

        <<
            ?WHOLE,
            ?A2S_PLAYER_REPLY,
            Payload/binary
        >> ->
            io:format("Got players: ~s~n", [Payload]);

        X->
            io:format("Wildcard got this: ~s~n", [X])
    end.

parse_info_payload(Payload) when is_binary(Payload) ->
    {Name, Payload1} = read_string(Payload),
    {Map, Payload2} = read_string(Payload1),
    {Folder, Payload3} = read_string(Payload2),
    {Game, Payload4} = read_string(Payload3),
    Result = maps:put("hostname", Name,
             maps:put("map", Map,
             maps:put("gamedir", Folder,
             maps:put("gamedesc", Game,
             maps:new())))),
    _Result1 = case Payload4 of
        <<
            ID:16,
            Players:8,
            MaxPlayers:8,
            Bots:8,
            ServerType:8,
            Environment:8,
            Visibility:8,
            VAC:8,
            EDF:8,
            _Payload5/binary %% Discard for now
        >> ->
            maps:put("appid", ID,
            maps:put("numplayers", Players,
            maps:put("maxplayers", MaxPlayers,
            maps:put("numbots", Bots,
            maps:put("dedicated", binary:bin_to_list(<<ServerType>>),
            maps:put("os", binary:bin_to_list(<<Environment>>),
            maps:put("passworded", Visibility,
            maps:put("secure", VAC,
            maps:put("edf", EDF, Result)))))))));
        _ -> Result
    end.

create_request_package(info) ->
    <<
        ?CHALLENGE,
        ?A2S_INFO,
        ?A2S_INFO_STRING,
        ?STRING_TERMINATION
    >>;
create_request_package(player) ->
    <<
        ?CHALLENGE,
        ?A2S_PLAYER,
        ?CHALLENGE,
        ?STRING_TERMINATION
    >>;
create_request_package(rules) ->
    <<
        ?CHALLENGE,
        ?A2S_INFO,
        ?A2S_INFO_STRING,
        ?STRING_TERMINATION
    >>.
create_request_package(player, Challenge) ->
    <<
        ?CHALLENGE,
        ?A2S_PLAYER,
        Challenge,
        ?STRING_TERMINATION
    >>.



test_info() ->
    Payload = create_request_package(info),
    io:format("~s~n", [Payload]),
    {ok, Socket} = gen_udp:open(0, ?UDP_OPTS),
    ok = gen_udp:send(Socket, {193, 202, 115, 74}, 27115, Payload),
    {ok, {_Address, _Port, Packet}} = gen_udp:recv(Socket, ?PACKETSIZE),
    %io:format("~s ~s~n", [Address, Port]),
    parse_packet(Packet).

test_player() ->
    Payload = create_request_package(player),
    io:format("~s~n", [Payload]),
    {ok, Socket} = gen_udp:open(0, ?UDP_OPTS),
    ok = gen_udp:send(Socket, {193, 202, 115, 74}, 27115, Payload),
    {ok, {_Address, _Port, Packet}} = gen_udp:recv(Socket, ?PACKETSIZE),
    %io:format("~s ~s~n", [Address, Port]),
    Challenge = parse_packet(Packet),
    Payload2 = create_request_package(player, Challenge),
    io:format("~s~n", [Payload2]),
    ok = gen_udp:send(Socket, {193, 202, 115, 74}, 27115, Payload),
    {ok, {_Address, _Port, Packet}} = gen_udp:recv(Socket, ?PACKETSIZE),
    parse_packet(Packet).
