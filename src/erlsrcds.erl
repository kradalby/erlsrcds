-module(erlsrcds).

-export([
    test/0
]).

-define(PACKETSIZE, 1400).
-define(WHOLE, -1).
-define(SPLIT, -2).

-define(A2S_INFO, "T").
-define(A2S_INFO_STRING, "Source Engine Query").
-define(A2S_INFO_REPLY, "I").
-define(A2S_PLAYER, "U").
-define(A2S_PLAYER_REPLY, "D").
-define(A2S_RULES, "V").
-define(A2S_RULES_REPLY, "E").
-define(CHALLENGE, -1).
-define(S2C_CHALLENGE, "A").

-define(STRING_TERMINATION, 16#00).

-define(UDP_OPTS, [
    binary,
    {active, false}
]).

read_string(Payload) ->
    [String, NewPayload] = binary:split(Payload, [<<?STRING_TERMINATION>>], []),
    io:format("Reading string: ~s~n", [String]),
    {binary:bin_to_list(String), NewPayload}.

parse_packet(Packet) when is_binary(Packet) ->

    case Packet of
        <<
            ?WHOLE:32/binary,
            ?A2S_INFO_REPLY/utf8,
            Protocol:8,
            Payload/binary
        >> ->
            io:format("Correct: ~s~n", [Payload]);

        <<
            255,255,255,255,
            73,
            Protocol:8,
            Payload/binary
        >> ->
            io:format("Aids: ~s~n", [Payload]),
            parse_info_payload(Payload)
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
    case Payload4 of
        <<
            ID:16,
            Players:8,
            MaxPlayers:8,
            Bots:8,
            ServerType:8,
            Environment:8,
            Visibility:8,
            VAC:8,
            Payload5/binary
        >> ->
            Result1 = maps:put("appid", ID,
                      maps:put("numplayers", Players,
                      maps:put("maxplayers", MaxPlayers,
                      maps:put("numbots", Bots,
                      maps:put("dedicated", binary:bin_to_list(<<ServerType>>),
                      maps:put("os", binary:bin_to_list(<<Environment>>),
                      maps:put("passworded", Visibility,
                      maps:put("secure", VAC,
                      maps:put("rest", Payload5, Result)))))))));
        _ -> Result
    end.


test() ->
    Payload = <<
        ?CHALLENGE:32,
        ?A2S_INFO:8,
        ?A2S_INFO_STRING,
        ?STRING_TERMINATION
    >>,
    io:format("~s~n", [Payload]),
    {ok, Socket} = gen_udp:open(0, ?UDP_OPTS),
    ok = gen_udp:send(Socket, {193, 202, 115, 74}, 27115, Payload),
    {ok, {Address, Port, Packet}} = gen_udp:recv(Socket, ?PACKETSIZE),
    %io:format("~s ~s~n", [Address, Port]),
    parse_packet(Packet).
