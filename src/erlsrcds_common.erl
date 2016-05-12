-module(erlsrcds_common).

-export([
         read_string/1
]).

-define(STRING_TERMINATION, 16#00).

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
