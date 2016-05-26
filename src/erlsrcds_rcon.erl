-module(erlsrcds_rcon).

-export([
         rcon/4,
         get_value_from_command_result/1
]).

-define(TIMEOUT, 1000).

-define(STRING_TERMINATION, 16#00).

-define(SERVERDATA_AUTH, 3).
-define(SERVERDATA_AUTH_RESPONSE, 2).

-define(SERVERDATA_EXECCOMMAND, 2).
-define(SERVERDATA_RESPONSE_VALUE, 0).

-define(MAX_COMMAND_LENGTH, 510).

-define(MIN_MESSAGE_LENGTH, 4 + 4 + 1 + 1).
-define(MAX_MESSAGE_LENGTH, 4 + 4 + 4096 + 1).

-define(TCP_OPTS, [
                   binary,
                   {packet, 0},
                   {active, false}
]).

create_request_packet(Type, ID, Body) ->
    BodyBinary = list_to_binary(Body),
    Data = <<
      ID:4/little-signed-integer-unit:8,
      Type:4/little-signed-integer-unit:8,
      BodyBinary/binary,
      ?STRING_TERMINATION,
      ?STRING_TERMINATION
    >>,

    Size = byte_size(Data),
    <<
      Size:4/little-signed-integer-unit:8,
      Data/binary
    >>.


parse_response_packet(Payload) ->
    case Payload of
        <<
          ID:4/little-signed-integer-unit:8,
          Type:4/little-signed-integer-unit:8,
          Rest/binary
        >> ->
            {Body, _Rest1} = erlsrcds_common:read_string(Rest),
            #{
               "id" => ID,
               "type" => Type,
               "body" => Body
             };
        _ ->
            {error, "Parse error"}
    end.


authorize(Socket, Password, ID) ->
    Packet = create_request_packet(?SERVERDATA_AUTH, ID, Password),
    gen_tcp:send(Socket, Packet),

    {ok, EmptyPayload} = read_packet(Socket),
    _EmptyPayloadMap = parse_response_packet(EmptyPayload),
    {ok, AuthResponse} = read_packet(Socket),
    AuthResponseMap = parse_response_packet(AuthResponse),

    case maps:get("id", AuthResponseMap) of
        -1 ->
            {denied};
        ID ->
            {approved}
    end.


read_packet(Socket) ->
    % Read the length of the package
    case gen_tcp:recv(Socket, 4) of
        {ok, <<Size:4/little-signed-integer-unit:8>>} ->

            % Read the rest of the package
            case gen_tcp:recv(Socket, Size) of
                {ok, Packet} ->
                    {ok, Packet};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


rcon(Command, Password, Address, Port) ->
    {ok, Socket} = gen_tcp:connect(Address, Port, ?TCP_OPTS),
    authorize(Socket, Password, 0),
    CommandPacket = create_request_packet(?SERVERDATA_EXECCOMMAND, 1, Command),
    gen_tcp:send(Socket, CommandPacket),
    Result = case read_packet(Socket) of
        {ok, Packet} ->
            parse_response_packet(Packet);
        {error, Reason} ->
            {error, Reason}
    end,
    gen_tcp:close(Socket),
    maps:get("body", Result).

get_value_from_command_result(Result) ->
    case re:run(Result, "^.*= \\\"(?<value>\\w+)\\\"", [{capture, ['value'], list}]) of
        {match, [Return]} ->
            Return;
        nomatch ->
            nomatch
    end.
          
