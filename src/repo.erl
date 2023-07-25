-module(repo).

-include("realworld.hrl").
-include_lib("epgsql/include/epgsql.hrl").

-export([init/0]).

-export([
    %% user
    login_user/1,
    update_user/2,
    auth_user/2,
    create_user/1,
    create_article/2,
    %% profile
    get_user_by_username/1,
    get_user_by_id/1,
    follow_user/2,
    unfollow_user/2,
    %% article
    get_articles_with_authors/1,
    get_article_by_slug/1,
    update_article/3,
    favorite_article/2,
    unfavorite_article/2,
    delete_article/2,
    %% comment
    add_comment/3,
    delete_comment/3,
    get_comments_with_authors/1,
    %% tag
    get_tags/0
]).

-export([build_query/1]).

-define(DB_NAME, pgsql).

init() ->
    ok = pooler:start(),

    {ok, DBConfig} = application:get_env(realworld_api, db_config),

    PoolConfig = #{
        name => ?DB_NAME,
        max_count => maps:get(pool_size, DBConfig, 10),
        init_count => 2,
        start_mfa => {epgsql, connect, [maps:without([pool_size], DBConfig)]}
    },

    {ok, _Pid} = pooler:new_pool(PoolConfig).

-spec login_user(map()) -> {ok, map()} | {error, term()}.
login_user(User) ->
    Email = maps:get(<<"email">>, User),
    Password = maps:get(<<"password">>, User),
    case
        query(
            "select * from users where email = $1 and password = $2",
            [Email, encrypt_password(Password)]
        )
    of
        {ok, []} ->
            {error, not_found};
        {ok, [Row]} ->
            {ok, user_h:user_map_from_row(Row)};
        Result ->
            Result
    end.

get_user_by_id(UserId) ->
    case
        query(
            "select * from users where id = $1",
            [UserId]
        )
    of
        {ok, [Row]} ->
            {ok, user_h:user_map_from_row(Row)};
        _ ->
	    {error, not_found}
    end.

get_user_by_username(Username) ->
    case
        query(
            "select * from users where username = $1",
            [Username]
        )
    of
        {ok, [Row]} ->
            {ok, user_h:user_map_from_row(Row)};
        _ ->
	    {error, not_found}
    end.

follow_user(#{<<"id">> := FollowerId} = _Follower, Followee) ->
    case
        query(
            "update users set followed_by = array_append(followed_by, $1) where username = $2 and $1 = any(followed_by) is not true returning *",
            [FollowerId, Followee]
        )
    of
        {ok, 1, [Row]} ->
            {ok, user_h:user_map_from_row(Row)};
        _ ->
            {error, <<"fail to follow">>}
    end.

unfollow_user(#{<<"id">> := FollowerId} = _Follower, Followee) ->
    case
        query("update users set followed_by = array_remove(followed_by, $1) where username = $2 returning *", [
            FollowerId, Followee
        ])
    of
        {ok, 1, [Row]} ->
            {ok, user_h:user_map_from_row(Row)};
        _Result ->
            {error, <<"fail to unfollow">>}
    end.

-spec auth_user(binary(), binary()) -> {boolean(), map()}.
auth_user(Token, JWTKey) ->
    {ok, #{<<"id">> := UserId} = User} = jwt:decode(Token, JWTKey),
    case
        query(
            "select id from users where id = $1", [UserId]
        )
    of
        {ok, [{UserId}]} ->
            {true, User};
        _Other ->
            {false, #{}}
    end.

-spec create_user(map()) -> {ok, map()} | {error, term()}.
create_user(User) ->
    Bio = maps:get(<<"bio">>, User, <<>>),
    Image = maps:get(<<"image">>, User, <<>>),
    Password = maps:get(<<"password">>, User),
    do_insert(User#{
        <<"bio">> => Bio,
        <<"image">> => Image,
        <<"password">> => encrypt_password(Password)
    }).
%% @private
do_insert(User) ->
    #{
        <<"username">> := UserName,
        <<"email">> := Email,
        <<"password">> := Password,
        <<"bio">> := Bio,
        <<"image">> := Image
    } = User,
    case
        query(
            "insert into users (username, email, password, bio, image) values ($1, $2, $3, $4, $5)",
            [UserName, Email, Password, Bio, Image]
        )
    of
        {ok, 1} ->
            {ok, User};
        Result ->
            {error, Result}
    end.

-spec update_user(map(), map()) -> {ok, map()} | {error, term()}.
update_user(User, #{<<"username">> := Username, <<"id">> := UserId} = _ClaimUser) ->
    {ok, User0} = get_user_by_username(Username),
    User1 = maps:merge(User0, User),
    Password = maps:get(<<"password">>, User1),
    do_update_user(User1#{<<"password">> => encrypt_password(Password)}, UserId).

%% @private
do_update_user(User, UserId) ->
    #{
        <<"username">> := Username,
        <<"email">> := Email,
        <<"password">> := Password,
        <<"bio">> := Bio,
        <<"image">> := Image
    } = User,
    case
        query(
            "update users set username = $1, email = $2, password = $3, bio = $4, image = $5 where id = $6",
            [Username, Email, Password, Bio, Image, UserId]
        )
    of
        {ok, 1} ->
            {ok, User};
        _Other ->
            {error, <<"update failed">>}
    end.

-spec create_article(map(), binary()) -> {ok, map()} | {error, term()}.
create_article(#{<<"article">> := Article}, #{<<"id">> := UserId} = _ClaimUser) ->
    Body = maps:get(<<"body">>, Article),
    Desc = maps:get(<<"description">>, Article),
    Tags = maps:get(<<"tagList">>, Article),
    Title = maps:get(<<"title">>, Article),
    case
        query(
            "insert into articles (title, slug, body, description, tag_list, author_id) values ($1, $2, $3, $4, $5, $6) returning *",
            [Title, slug:make(binary_to_list(Title)), Body, Desc, Tags, UserId]
        )
    of
        {ok, 1, [Row]} ->
            Article0 = article_h:article_map_from_row(Row),
            {ok, Article0};
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_articles_with_authors(map()) -> {ok, list()} | {error, term()}.
get_articles_with_authors(Opts) ->
    Limit = maps:get(?LIMIT, Opts),
    Offset = maps:get(?OFFSET, Opts),
    FavoritedAuthor = maps:get(?FAVORITED, Opts),

    {ok, UserId} = query("select id from users where username = $1", [FavoritedAuthor]),

    {WhereClause, Index, Params} = build_query(
				     maps:to_list(
				       maps:with(
					 [?AUTHOR, ?FAVORITED, ?TAG],
					 Opts#{?FAVORITED => UserId}
					)
				      )
				    ),
    LimitClause = " limit $" ++ integer_to_list(Index) ++ " offset $" ++ integer_to_list(Index + 1),

    Query = "select distinct articles.*, users.* from articles, users where articles.author_id = users.id" ++
	WhereClause ++
	" order by created_at desc" ++
	LimitClause,
    io:format("Query ~p - ~p ~n",[Query, Params]),
    case
        query(
            "select distinct articles.*, users.* from articles, users where articles.author_id = users.id" ++
                WhereClause ++
                " order by created_at desc" ++
                LimitClause,
            Params ++ [Limit, Offset]
        )
    of
        {ok, []} ->
            {ok, []};
        {ok, Rows} ->
            {ok, Rows};
        Result ->
            {error, Result}
    end.

-spec get_article_by_slug(binary()) -> {ok, map()} | {error, term()}.
get_article_by_slug(Slug) ->
    case query("select * from articles where slug = $1", [Slug]) of
        {ok, []} ->
            {error, <<<<"Article do not exists">>/binary, Slug/binary>>};
        {ok, [Row]} ->
            Article0 = article_h:article_map_from_row(Row),
            {ok, Article0};
        Result ->
            Result
    end.

update_article(Article, Slug, ClaimUser) ->
    case repo:get_article_by_slug(Slug) of
        {ok, Article0} ->
            do_update_article(Article0, Article, Slug, ClaimUser);
        {error, Reason} ->
            {error, Reason}
    end.

do_update_article(Article0, Article, Slug, #{<<"id">> := UserId} = _ClaimUser) ->
    Article1 = maps:merge(Article0, Article),
    Title = maps:get(<<"title">>, Article1),
    Slug1 = slug:make(binary_to_list(Title)),
    Body = maps:get(<<"body">>, Article1),
    Desc = maps:get(<<"description">>, Article1),
    Tags = maps:get(<<"tagList">>, Article1),

    Now = erlang:timestamp(),

    Result =
        case Slug =/= Slug1 of
            true ->
                query(
                    "update articles set (title, slug, body, description, updated_at, tag_list) = ($1, $2, $3, $4, $5, $6) where slug = $7 and author_id = $8",
                    [Title, Slug1, Body, Desc, Now, Tags, Slug, UserId]
                );
            false ->
                query(
                    "update articles set (body, description, updated_at, tag_list) = ($1, $2, $3, $4) where slug = $5",
                    [Body, Desc, Tags, Now, Slug]
                )
        end,
    case Result of
        {ok, 1} ->
            {ok, Article1};
        _ ->
            {error, <<"failed when updating article">>}
    end.

favorite_article(Slug, #{<<"id">> := UserId} = _ClaimUser) ->
    Now = erlang:timestamp(),

    case
        query(
            "update articles set (favorited_by, updated_at) = (array_append(favorited_by, $1), $2) where slug = $3 returning *",
            [UserId, Now, Slug]
        )
    of
        {ok, 1, [Row]} ->
            Article = article_h:article_map_from_row(Row),
            {ok, Article};
        {ok, 0, []} ->
            {error, <<<<"Article does not exists">>/binary, Slug/binary>>};
        Result ->
            logger:warning("~p results in ~p", [?FUNCTION_NAME, Result]),
            {error, <<"fail when favoriting article">>}
    end.

unfavorite_article(Slug, #{<<"id">> := UserId} = _ClaimUser) ->
    Now = erlang:timestamp(),

    case
        query(
            "update articles set (favorited_by, updated_at) = (array_remove(favorited_by, $1), $2) where slug = $3 returning *",
            [UserId, Now, Slug]
        )
    of
        {ok, 1, [Row]} ->
            Article = article_h:article_map_from_row(Row),
            {ok, Article};
        {ok, 0, []} ->
            {error, <<<<"Article does not exists">>/binary, Slug/binary>>};
        Result ->
            logger:info("~p results in ~p", [?FUNCTION_NAME, Result]),
            Result
    end.

delete_article(Slug, #{<<"id">> := UserId} = _ClaimUser) ->
    _ = query("delete from articles where slug = $1 and author_id = $2", [Slug, UserId]).

add_comment(#{<<"comment">> := Comment}, Slug, #{<<"id">> := UserId} = _ClaimUser) ->
    Body = maps:get(<<"body">>, Comment),

    case query("insert into comments (body, author_id, slug) values ($1, $2, $3) returning *", [Body, UserId, Slug]) of
        {ok, 1, [Comment1]} ->
            {ok, comment_h:comment_map_from_row(Comment1)};
        {error, Reason} ->
            logger:error("Cannot add comment ~p for reason ~p~n", [Comment, Reason]),
            {error, Reason}
    end.

delete_comment(CommentId, Slug, #{<<"id">> := UserId} = _ClaimUser) ->
    try
        Id = binary_to_integer(CommentId),
        query("delete from comments where author_id = $1 and id = $2 and slug = $3", [UserId, Id, Slug])
    of
        {ok, _Count} ->
            true;
        {error, Reason} ->
            {error, Reason}
    catch
        _C:E:_S ->
            {error, E}
    end.

get_comments_with_authors(Slug) ->
    case
        query(
            "select distinct comments.*, users.* from comments, users where comments.author_id = users.id and slug = $1",
            [Slug]
        )
    of
        {ok, Rows} when length(Rows) > 0 ->
            {ok, Rows};
        _ ->
            {ok, []}
    end.

get_tags() ->
    case query("select tag_list from articles") of
        {ok, Rows} ->
            {ok, lists:usort(lists:append([Row || {Row} <- Rows]))};
        Result ->
            Result
    end.

%% db query

query(Command) ->
    query(Command, []).

query(Command, ArgList) ->
    Conn = pooler:take_member(?DB_NAME),
    try epgsql:equery(Conn, Command, ArgList) of
        {ok, Count} ->
            {ok, Count};
        {ok, _Columns, Rows} ->
            {ok, Rows};
        {ok, Count, _Columns, Rows} ->
            {ok, Count, Rows};
        {error, #error{message = Reason}} ->
            logger:error("query: ~p, arg: ~p failed for reason: ~p~n", [Command, ArgList, Reason]),
            {error, Reason}
    catch
        Class:Exception:StackTrace ->
            logger:error("epgsql query error ~p for reason ~p: details ~p~n", [Class, Exception, StackTrace]),
            {error, Exception}
    after
        pooler:return_member(?DB_NAME, Conn)
    end.
%%
%% encrypt password
%%

encrypt_password(Password) ->
    list_to_binary(lists:flatten([[io_lib:format("~2.16.0B", [X]) || <<X:8>> <= crypto:hash(md5, Password)]])).

build_query(List) ->
    build_query(List, 1, [], []).

build_query([], Index, Acc, Params) when length(Params) > 0 ->
    {" and " ++ lists:flatten(lists:join(" and ", lists:reverse(Acc))), Index, lists:reverse(Params)};
build_query([], Index, Acc, Params) ->
    {Acc, Index, Params};
build_query([{_Key, []} | T], Index, Acc, Params) ->
    build_query(T, Index, Acc, Params);
build_query([{Key, Value} | T], Index, Acc, Params) ->
    Str =
        case Key of
            ?TAG ->
                "$" ++ integer_to_list(Index) ++ " = any(tag_list)";
            ?AUTHOR ->
                "username = $" ++ integer_to_list(Index);
	    ?FAVORITED ->
		"$" ++ integer_to_list(Index) ++ " = any(favorited_by)"
        end,
    build_query(T, Index + 1, [Str | Acc], [Value | Params]).
