-module(profile_h).

-include("realworld.hrl").

-export([
    init/2,
    allowed_methods/2,
    is_authorized/2,
    content_types_provided/2,
    content_types_accepted/2,
    delete_resource/2
]).

-export([
    handle_get/2,
    handle_post/2
]).

-export([build_profile/1]).

init(Req, State) ->
    JWTKey = application:get_env(realworld_api_app, jwt_key, ?DEFAULT_JWT_KEY),
    {cowboy_rest, Req, State#{jwt_key => JWTKey}}.

allowed_methods(Req, #{methods := MethodList} = State) ->
    {MethodList, Req, State}.

is_authorized(#{method := <<"POST">>} = Req, State) ->
    is_authorized_1(Req, State);
is_authorized(#{method := <<"DELETE">>} = Req, State) ->
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

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_get}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_post}], Req, State}.

handle_get(Req, State) ->
    Username = cowboy_req:binding(username, Req),
    Response =
        case repo:get_user_by_username(Username) of
            {ok, User} ->
                build_profile(User);
            _ ->
                #{}
        end,
    {jsx:encode(Response), Req, State}.

handle_post(Req, #{<<"user">> := User} = State) ->
    Username = cowboy_req:binding(username, Req),
    ClaimId = maps:get(<<"id">>, User),
    Req1 =
        case repo:follow_user(ClaimId, Username) of
            {ok, User0} ->
                common:reply(Req, ?HTTP_OK, build_profile(User0));
            {error, Reason} ->
                common:reply(Req, ?HTTP_INVALID, ?error_msg(Reason))
        end,
    {stop, Req1, State}.

delete_resource(Req, #{<<"user">> := User} = State) ->
    Username = cowboy_req:binding(username, Req),
    ClaimId = maps:get(<<"id">>, User),
    Req1 =
        case repo:unfollow_user(ClaimId, Username) of
            {ok, User0} ->
                common:reply(Req, ?HTTP_OK, build_profile(User0));
            {error, Reason} ->
                common:reply(Req, ?HTTP_INVALID, ?error_msg(Reason))
        end,
    {stop, Req1, State}.

build_profile(User) ->
    Following = length(maps:get(<<"followed_by">>, User)) > 0,
    Profile = maps:with(?PROFILE_KEYS, User#{<<"following">> => Following}),
    #{<<"profile">> => Profile}.
