-module(user_h).

-include("realworld.hrl").

%% cowboy_rest callbacks
-export([
    init/2,
    allowed_methods/2,
    malformed_request/2,
    is_authorized/2,
    content_types_provided/2,
    content_types_accepted/2
]).

-export([
    handle_create/2,
    handle_login/2,
    handle_update/2,
    handle_get/2
]).

-export([user_map_from_row/1]).

init(Req, State) ->
    JWTKey = application:get_env(realworld_api_app, jwt_key, ?DEFAULT_JWT_KEY),
    {cowboy_rest, Req, State#{jwt_key => JWTKey}}.

allowed_methods(Req, #{methods := MethodList} = State) ->
    {MethodList, Req, State}.

malformed_request(#{method := <<"GET">>} = Req, State) ->
    {false, Req, State};
malformed_request(Req, State) ->
    case cowboy_req:has_body(Req) of
        false ->
            Req1 = common:reply(Req, ?HTTP_INVALID, ?error_msg([<<"Can't be empty">>])),
            {stop, Req1, State};
        true ->
            {false, Req, State}
    end.

is_authorized(#{method := <<"GET">>} = Req, State) ->
    is_authorized_1(Req, State);
is_authorized(#{method := <<"PUT">>} = Req, State) ->
    is_authorized_1(Req, State);
is_authorized(Req, State) ->
    {true, Req, State}.

is_authorized_1(Req, #{jwt_key := JWTKey} = State) ->
    case common:is_authorized(Req, JWTKey) of
        {true, User} ->
            {true, Req, State#{<<"user">> => User}};
        {false, Reason} ->
            {{false, Reason}, Req, State}
    end.

%% GET callback
content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_get}], Req, State}.

%% POST callback
content_types_accepted(Req, State) ->
    Path = cowboy_req:path(Req),
    {[{<<"application/json">>, to_callback(Path)}], Req, State}.

handle_login(Req, #{jwt_key := JWTKey} = State) ->
    {ok, Body, Req0} = cowboy_req:read_body(Req),
    Req1 =
        case get_user(jsx:decode(Body), <<"login">>) of
            {error, undefined} ->
                common:reply(Req0, ?HTTP_INVALID, ?error_msg([<<"Can't be empty">>]));
            {error, Description} ->
                common:reply(Req0, ?HTTP_INVALID, ?error_msg(Description));
            {ok, []} ->
                common:reply(Req0, ?HTTP_NOT_FOUND);
            {ok, UserMap} ->
                common:reply(Req0, ?HTTP_OK, build_response(UserMap, JWTKey))
        end,
    {stop, Req1, State}.

handle_create(Req, #{jwt_key := JWTKey} = State) ->
    {ok, Data, Req0} = cowboy_req:read_body(Req),
    Req1 =
        case get_user(jsx:decode(Data), <<"create">>) of
            {error, undefined} ->
                common:reply(Req0, ?HTTP_INVALID, ?error_msg([<<"Can't be empty">>]));
            {error, Description} ->
                common:reply(Req0, ?HTTP_INVALID, ?error_msg(Description));
            {ok, []} ->
                common:reply(Req0, ?HTTP_NOT_FOUND);
            {ok, User} ->
                Reply = build_response(User, JWTKey),
                common:reply(Req0, ?HTTP_OK, Reply)
        end,
    {stop, Req1, State}.

handle_update(Req, #{<<"user">> := User0, jwt_key := JWTKey} = State) ->
    {ok, Body, Req0} = cowboy_req:read_body(Req),

    #{<<"user">> := User} = jsx:decode(Body),

    {ok, User1} = repo:update_user(User, User0),

    Req1 = common:reply(Req0, ?HTTP_OK, build_response(User1, JWTKey)),

    {stop, Req1, State}.

handle_get(Req, #{<<"user">> := User, jwt_key := JWTKey} = State) ->
    UserId = maps:get(<<"id">>, User),
    Response =
        case repo:get_user_by_id(UserId) of
            {ok, UserMap} ->
                build_response(UserMap, JWTKey);
            {error, _} ->
                #{}
        end,
    {jsx:encode(Response), Req, State}.

%% Internal functions

get_user(#{<<"user">> := User}, Action) ->
    case {missing_keys(User, Action), invalid_keys(User)} of
        {[], []} ->
            login_or_register_user(User, Action);
        {[], IKs} ->
            {error, [<<"Invalid keys:">> | IKs]};
        {MKs, _} ->
            {error, [<<"Missing keys:">> | MKs]}
    end;
get_user(Body, _Path) ->
    {error, undefined}.

login_or_register_user(User, <<"login">>) ->
    repo:get_user(User);
login_or_register_user(User, <<"create">>) ->
    case repo:get_user(User) of
        {error, _} ->
            repo:create_user(User);
        {ok, _} ->
            logger:warning("User exists~n"),
            Msg = #{
                <<"email">> => [<<"has already been taken">>],
                <<"username">> => [<<"has already been taken">>]
            },
            {error, Msg}
    end.

missing_keys(User, Action) ->
    lists:filter(fun(K) -> not maps:is_key(K, User) end, required_keys(Action)).

invalid_keys(User) ->
    lists:filter(
        fun(K) ->
            V = maps:get(K, User),
            not (is_binary(V) andalso size(V) > 0)
        end,
        maps:keys(User)
    ).

required_keys(<<"login">>) ->
    [<<"email">>, <<"password">>];
required_keys(<<"create">>) ->
    [<<"username">>, <<"email">>, <<"password">>].

to_callback(<<"/api/users">>) ->
    handle_create;
to_callback(<<"/api/users/login">>) ->
    handle_login;
to_callback(<<"/api/user">>) ->
    handle_update.

build_response(User, JWTKey) ->
    Claim = maps:with([<<"id">>], User),
    Token = gen_token(Claim, JWTKey),
    #{
        <<"user">> =>
            maps:without(
                ?USER_EXCLUDE_KEYS,
                User#{<<"token">> => Token}
            )
    }.

%% to do: maybe deal with token expiration
gen_token(Claim, JWTKey) ->
    {ok, Token} = jwt:encode(<<"HS256">>, Claim, _ExpirationSeconds = 86400, JWTKey),
    Token.

user_map_from_row(UserRow) when is_tuple(UserRow) ->
    user_map_from_row(tuple_to_list(UserRow));
user_map_from_row(UserRow) ->
    #{Key => Value || {Key, Value} <- lists:zip(?USER_KEYS, UserRow)}.
