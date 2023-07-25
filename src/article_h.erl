-module(article_h).

-include("realworld.hrl").

%% cowboy_rest callbacks
-export([
    init/2,
    allowed_methods/2,
    is_authorized/2,
    content_types_provided/2,
    content_types_accepted/2
]).

-export([
    handle_get/2,
    handle_create/2
]).

-export([
    article_map_from_row/1,
    format_article/2
]).

init(Req, State) ->
    JWTKey = application:get_env(realworld_api_app, jwt_key, ?DEFAULT_JWT_KEY),
    {cowboy_rest, Req, State#{jwt_key => JWTKey}}.

allowed_methods(Req, #{methods := MethodList} = State) ->
    {MethodList, Req, State}.

is_authorized(#{method := <<"POST">>} = Req, #{jwt_key := JWTKey} = State) ->
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
    {[{<<"application/json">>, handle_create}], Req, State}.

handle_get(Req, State) ->
    Req1 =
	maybe
	    {ok, Opts} ?= parse_qs(Req),
	    {ok, ArticleList} = repo:get_articles_with_authors(Opts),
	    common:reply(Req, ?HTTP_OK, format_articles(ArticleList))
	else
	    {error, Reason} ->
		common:reply(Req, ?HTTP_INVALID, ?error_msg(Reason))
	end,
    {stop, Req1, State}.

handle_create(Req, #{<<"user">> := User} = State) ->
    {ok, Data, Req0} = cowboy_req:read_body(Req),
    case repo:create_article(jsx:decode(Data), User) of
        {ok, Article} ->
	    Username = maps:get(<<"username">>, User),
            {ok, User1} = repo:get_user_by_username(Username),
            common:reply(
                Req,
                ?HTTP_OK,
                #{<<"article">> => format_article(Article, User1)}
            );
        {error, Reason} ->
            common:reply(Req, ?HTTP_INVALID, ?error_msg(Reason))
    end,
    {stop, Req0, State}.

format_article(Article0, User) ->
    FavoritesCount = length(maps:get(<<"favorited_by">>, Article0)),
    CreatedAt = maps:get(<<"createdAt">>, Article0),
    UpdatedAt = maps:get(<<"updatedAt">>, Article0),

    Article = maps:without(
        ?ARTICLE_EXCLUDE_KEYS,
        Article0#{
            <<"favorited">> => (FavoritesCount > 0),
            <<"favoritesCount">> => FavoritesCount,
            <<"createdAt">> => common:to_iso8601(CreatedAt),
            <<"updatedAt">> => common:to_iso8601(UpdatedAt)
        }
    ),

    AuthorInfo = profile_h:build_profile(User),
    maps:merge(Article, #{<<"author">> => AuthorInfo}).

format_articles(Rows) ->
    ArticlesList =
        lists:map(
            fun(Row0) ->
                {ArticleRow, UserRow} =
                    lists:split(
                        length(?ARTICLE_KEYS),
                        tuple_to_list(Row0)
                    ),

                format_article(
                    article_h:article_map_from_row(ArticleRow),
                    user_h:user_map_from_row(UserRow)
                )
            end,
            Rows
        ),

    #{
        <<"articles">> => ArticlesList,
        <<"articlesCount">> => length(ArticlesList)
    }.

article_map_from_row(ArticleRow) when is_tuple(ArticleRow) ->
    article_map_from_row(tuple_to_list(ArticleRow));
article_map_from_row(ArticleRow) ->
    #{Key => Value || {Key, Value} <- lists:zip(?ARTICLE_KEYS, ArticleRow)}.

parse_qs(Req) ->
    case catch cowboy_req:match_qs(?DEFAULT_QS, Req) of
        Result when is_map(Result) ->
            {ok, Result};
        {'EXIT', Reason} ->
            logger:debug("~p~n", [Reason]),
            {error, <<"Query string validation constraints failed">>}
    end.
