-module(erlsrcds).


-define(PACKETSIZE, 1400).
-define(WHOLE, -1).
-define(SPLIT, -2).

-define(A2S_INFO, <<"T">>).
-define(A2S_INFO_STRING, "Source Engine Query").
-define(A2S_INFO_REPLY, <<"I">>).
-define(A2S_PLAYER, <<"U">>).
-define(A2S_PLAYER_REPLY, <<"D">>).
-define(A2S_RULES, <<"V">>).
-define(A2S_RULES_REPLY, <<"E">>).
-define(CHALLENGE, -1).
-define(S2C_CHALLENGE, <<"A">>).



parse_packet(Packet) when is_binary(Packet) ->

    case Packet of
        <<
            Header:32,
            ID:32,
            Total:8,
            Number:8,
            Size:16,
            Payload/binary
        >> ->
            io:format("Received a SRCDS package");
        <<
            Header:32,
            ID:32,
            PacketNumber:8,
            Payload/binary
        >> ->
            io:format("Received a GoldSrc package")
    end.
