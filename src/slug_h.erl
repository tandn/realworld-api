-module(slug_h).

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
    handle_post/2,
    handle_update/2
]).

init(Req, State) ->
    JWTKey = application:get_env(realworld_api_app, jwt_key, ?DEFAULT_JWT_KEY),
    {cowboy_rest, Req, State#{jwt_key => JWTKey}}.

allowed_methods(Req, #{methods := MethodList} = State) ->
    {MethodList, Req, State}.

is_authorized(#{method := Method} = Req, #{jwt_key := JWTKey} = State) when Method =/= <<"GET">> ->
    case common:is_authorized(Req, JWTKey) of
        {true, User} ->
            {true, Req, State#{<<"user">> => User}};
        {false, Reason} ->
            {{false, Reason}, Req, State}
    end;
is_authorized(Req, State) ->
    {true, Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_get}], Req, State}.

content_types_accepted(#{method := <<"POST">>} = Req, State) ->
    {[{<<"application/json">>, handle_post}], Req, State};
content_types_accepted(#{method := <<"PUT">>} = Req, State) ->
    {[{<<"application/json">>, handle_update}], Req, State}.

handle_get(Req, State) ->
    Slug = cowboy_req:binding(slug, Req),
    Response =
        case repo:get_article_by_slug(Slug) of
            {ok, Article} ->
		UserId = maps:get(<<"author_id">>, Article),
                {ok, User} = repo:get_user_by_id(UserId),
                build_response(article_h:format_article(Article, User));
            {error, Reason} ->
                ?error_msg(Reason)
        end,
    {jsx:encode(Response), Req, State}.

handle_post(Req, #{<<"user">> := User} = State) ->
    Slug = cowboy_req:binding(slug, Req),
    Req1 =
        case repo:favorite_article(Slug, User) of
            {ok, Article} ->
		Username = maps:get(<<"username">>, User),
                {ok, User1} = repo:get_user_by_username(Username),
                common:reply(Req, ?HTTP_OK, build_response(article_h:format_article(Article, User1)));
            {error, Reason} ->
                common:reply(Req, ?HTTP_INVALID, ?error_msg(Reason))
        end,
    {stop, Req1, State}.

handle_update(Req, #{<<"user">> := User} = State) ->
    Slug = cowboy_req:binding(slug, Req),
    {ok, Body, Req0} = cowboy_req:read_body(Req),
    Req1 =
        case repo:update_article(jsx:decode(Body), Slug, User) of
            {ok, Article} ->
		Username = maps:get(<<"username">>, User),
                {ok, User1} = repo:get_user_by_username(Username),
                common:reply(Req0, ?HTTP_OK, build_response(article_h:format_article(Article, User1)));
            {error, Reason} ->
                common:reply(Req0, ?HTTP_INVALID, ?error_msg(Reason))
        end,
    {stop, Req1, State}.

delete_resource(Req, State) ->
    <<"/api/articles", Rest/binary>> = cowboy_req:path(Req),
    PathComponents = binary:split(Rest, <<"/">>, [global, trim_all]),
    handle_delete(PathComponents, Req, State).

handle_delete([_Slug, <<"favorite">>], Req, #{<<"user">> := User} = State) ->
    Slug = cowboy_req:binding(slug, Req),
    Req1 =
        case repo:unfavorite_article(Slug, User) of
            {ok, Article} ->
		Username = maps:get(<<"username">>, User),
                {ok, User1} = repo:get_user_by_username(Username),
                common:reply(Req, ?HTTP_OK, build_response(article_h:format_article(Article, User1)));
            {error, Reason} ->
                common:reply(Req, ?HTTP_INVALID, ?error_msg(Reason))
        end,
    {stop, Req1, State};
handle_delete([_Slug], Req, #{<<"user">> := User} = State) ->
    Slug = cowboy_req:binding(slug, Req),
    _ = repo:delete_article(Slug, User),

    Req1 = common:reply(Req, ?HTTP_OK),

    {stop, Req1, State}.

build_response(Article) ->
    #{<<"article">> => Article}.
