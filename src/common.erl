-module(common).

-include("realworld.hrl").

-export([is_authorized/2]).

-export([
    reply/2,
    reply/3,
    to_iso8601/1
]).

reply(Req, StatusCode) ->
    reply(Req, StatusCode, #{}).

reply(Req, StatusCode, Reply) ->
    cowboy_req:reply(
        StatusCode,
        ?no_server(#{<<"content-type">> => ?RESP_CONTENT_TYPE}),
        jsx:encode(Reply),
        Req
    ).

is_authorized(Req, JWTKey) ->
    try
        Req0 = normalize_auth_header(Req),
        case cowboy_req:parse_header(<<"authorization">>, Req0) of
            {bearer, Token} ->
                case repo:auth_user(Token, JWTKey) of
                    {true, User} ->
                        {true, User};
                    {false, _} ->
                        {false, <<"invalid token">>}
                end;
            _ ->
                {false, <<"missing authorization credentials">>}
        end
    catch
        C:E:S ->
            logger:error("authentication error ~p - ~p : ~p ~n", [C, E, S]),
            {false, <<"Bearer Secure Area">>}
    end.

to_iso8601({{Year, Month, Day}, {Hour, Min, Sec}}) when is_float(Sec) ->
    list_to_binary(
        io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~6.3.0fZ", [Year, Month, Day, Hour, Min, Sec])
    );
to_iso8601({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    list_to_binary(
        io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ", [Year, Month, Day, Hour, Min, Sec])
    ).

%% internal functions

normalize_auth_header(#{headers := Header} = Req) ->
    case maps:find(<<"authorization">>, Header) of
        error ->
            Req;
        {ok, Auth} ->
            Req#{headers => Header#{<<"authorization">> => parse_authorization(Auth)}}
    end.

parse_authorization(<<T, O, K, E, N, " ", R/bits>>) when
    (R =/= <<>>),
    ((T =:= $T) or (T =:= $t)),
    ((O =:= $O) or (O =:= $o)),
    ((K =:= $K) or (K =:= $k)),
    ((E =:= $E) or (E =:= $e)),
    ((N =:= $N) or (N =:= $n))
->
    <<<<"Bearer">>/binary, " ", R/bits>>;
parse_authorization(Other) ->
    Other.
