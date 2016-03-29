-module(erlsrcds).

-export([
    info/2,
    player/2,
    rules/2
]).

-define(PACKETSIZE, 1400).

-define(WHOLE, -1:32/signed).
-define(SPLIT, -2:4/little-signed-integer-unit:8).

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
-spec read_string(Payload::binary()) -> {[byte()], binary()}.
read_string(Payload) ->
    Result = binary:split(Payload, [<<?STRING_TERMINATION>>], []),
    case Result of
        [String, NewPayload] -> {binary:bin_to_list(String), NewPayload};
        % Handle the edge case with a string
        % being the last thing in the payload.
        [String] -> {binary:bin_to_list(String), <<>>}
    end.

%% parse the packet header and
%% forward the payload to the
%% correct parse function.
-spec parse_packet(Payload::binary()) -> #{} | [].
parse_packet(Packet) when is_binary(Packet) ->
    case Packet of
        <<
            ?WHOLE,
            ?A2S_INFO_REPLY/utf8,
            _Protocol:8,
            Payload/binary
        >> ->
            parse_info_payload(Payload);

        <<
            ?WHOLE,
            ?S2C_CHALLENGE,
            Challenge:32/signed,
            _Payload/binary
        >> ->
            #{"Challenge" => Challenge};

        <<
            ?WHOLE,
            ?A2S_PLAYER_REPLY,
            Players:8,
            Payload/binary
        >> ->
            parse_player_payload(Payload, Players, []);


        % Match different Rules scenarios
        %% Matches: Rules in one packet
        <<
            ?WHOLE,
            ?A2S_RULES_REPLY,
            Rules:2/little-signed-integer-unit:8,
            Payload/binary
        >> ->
            parse_rules_payload(Payload, Rules, #{});

        %% Matches: Rules in one packet w/o challenge?
        <<
            ?SPLIT,
            ?A2S_RULES_REPLY,
            Rules:2/little-signed-integer-unit:8,
            Payload/binary
        >> ->
            parse_rules_payload(Payload, Rules, #{});

        %% Matches: Rules in multiple packets
        <<
            ?SPLIT,
            ID:4/little-signed-integer-unit:8,
            Total:8,
            Number:8,
            Size:2/little-signed-integer-unit:8,
            ?WHOLE,
            ?A2S_RULES_REPLY,
            Rules:2/little-signed-integer-unit:8,
            Payload/binary
        >> ->
            io:format("ID: ~p~n Total: ~p~n Number: ~p~n Size: ~p~n", [ID, Total, Number, Size]),
            parse_rules_payload(Payload, Rules, #{});

        X ->
            io:format("Wildcard got this: ~p~n", [X])
    end.

-spec parse_info_payload(Payload::binary()) -> #{}.
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
            ID:2/little-signed-integer-unit:8,
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

-spec parse_player_payload(binary(), number(), [#{}]) -> [].
parse_player_payload(_Payload, 0, State) ->
    State;
parse_player_payload(Payload, Number, State) when Number > 0 ->
    <<Index:8, Payload1/binary>> = Payload,
    {Name, Payload2} = read_string(Payload1),
    <<Score:4/little-signed-integer-unit:8, Payload3/binary>> = Payload2,
    <<Duration:4/little-signed-float-unit:8, Payload4/binary>> = Payload3,
    Player = #{
        "index" => Index,
        "name" => Name,
        "kill" => Score,
        "time" => Duration
    },
    parse_player_payload(Payload4, Number - 1, [Player|State]).

-spec parse_rules_payload(binary(), number(), #{}) -> #{}.
parse_rules_payload(_Payload, 0, State) ->
    State;
parse_rules_payload(<<>>, _Number, State) ->
    State;
parse_rules_payload(Payload, Number, State) when Number > 0 ->
    {Name, Payload1} = read_string(Payload),
    {Value, Payload2} = read_string(Payload1),
    NewState = maps:put(Name, Value, State),
    parse_rules_payload(Payload2, Number - 1, NewState).

-spec create_request_package('info' | 'player' | 'rules') -> binary().
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
        ?A2S_RULES,
        ?A2S_INFO_STRING,
        ?STRING_TERMINATION
    >>.

-spec create_request_package('player' | 'rules', integer()) -> binary().
create_request_package(player, Challenge) ->
    <<
        ?CHALLENGE,
        ?A2S_PLAYER,
        Challenge:32,
        ?STRING_TERMINATION
    >>;
create_request_package(rules, Challenge) ->
    <<
        ?CHALLENGE,
        ?A2S_RULES,
        Challenge:32,
        ?STRING_TERMINATION
    >>.

-spec info(byte(), number()) -> #{}.
info(Address, Port) ->
    {ok, AddressTuple} = inet_parse:address(Address),
    info_internal(AddressTuple, Port).

-spec info_internal({number(),number(),number(),number()}, number()) -> #{}.
info_internal(Address = {_,_,_,_}, Port) ->
    Payload = create_request_package(info),
    {ok, Socket} = gen_udp:open(0, ?UDP_OPTS),
    ok = gen_udp:send(Socket, Address, Port, Payload),
    {ok, {_Address, _Port, Packet}} = gen_udp:recv(Socket, ?PACKETSIZE),
    parse_packet(Packet).

-spec player(byte(), number()) -> #{}.
player(Address, Port) ->
    {ok, AddressTuple} = inet_parse:address(Address),
    player_internal(AddressTuple, Port).

-spec player_internal({number(),number(),number(),number()}, number()) -> #{}.
player_internal(Address = {_,_,_,_}, Port) ->
    Payload = create_request_package(player),
    {ok, Socket} = gen_udp:open(0, ?UDP_OPTS),
    ok = gen_udp:send(Socket, Address, Port, Payload),
    {ok, {_Address, _Port, ChallengePacket}} = gen_udp:recv(Socket, ?PACKETSIZE),
    Challenge = maps:get("Challenge", parse_packet(ChallengePacket)),
    ChallengePayload = create_request_package(player, Challenge),
    ok = gen_udp:send(Socket, Address, Port, ChallengePayload),
    {ok, {_Address, _Port, Packet}} = gen_udp:recv(Socket, ?PACKETSIZE),
    parse_packet(Packet).

-spec rules(byte(), number()) -> #{}.
rules(Address, Port) ->
    {ok, AddressTuple} = inet_parse:address(Address),
    rules_internal(AddressTuple, Port).

-spec rules_internal({number(),number(),number(),number()}, number()) -> #{}.
rules_internal(Address = {_,_,_,_}, Port) ->
    Payload = create_request_package(rules),
    {ok, Socket} = gen_udp:open(0, ?UDP_OPTS),
    ok = gen_udp:send(Socket, Address, Port, Payload),
    {ok, {_Address, _Port, ChallengePacket}} = gen_udp:recv(Socket, ?PACKETSIZE),
    Challenge = maps:get("Challenge", parse_packet(ChallengePacket)),
    ChallengePayload = create_request_package(rules, Challenge),
    ok = gen_udp:send(Socket, Address, Port, ChallengePayload),
    {ok, {_Address, _Port, Packet}} = gen_udp:recv(Socket, ?PACKETSIZE),
    parse_packet(Packet).
