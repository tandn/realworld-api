-module(comment_h).

-include("realworld.hrl").

-export([init/2]).

-export([
    allowed_methods/2,
    is_authorized/2,
    content_types_provided/2,
    content_types_accepted/2,
    delete_resource/2
]).

-export([
    handle_post/2,
    handle_get/2
]).

-export([comment_map_from_row/1]).

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

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_post}], Req, State}.

delete_resource(Req, #{<<"user">> := User} = State) ->
    Slug = cowboy_req:binding(slug, Req),
    CommentId = cowboy_req:binding(id, Req),

    Req1 =
        case repo:delete_comment(CommentId, Slug, User) of
            {error, Reason} ->
                common:reply(Req, ?HTTP_INVALID, ?error_msg(Reason));
            true ->
                common:reply(Req, ?HTTP_OK)
        end,
    {stop, Req1, State}.

handle_get(Req, State) ->
    Slug = cowboy_req:binding(slug, Req),
    {ok, Rows} = repo:get_comments_with_authors(Slug),
    CommentsList = format_comments(Rows),
    {jsx:encode(CommentsList), Req, State}.

handle_post(Req, #{<<"user">> := User} = State) ->
    Slug = cowboy_req:binding(slug, Req),
    {ok, Body, Req0} = cowboy_req:read_body(Req),
    Req1 =
        case repo:add_comment(jsx:decode(Body), Slug, User) of
            {ok, Comment} ->
		Username = maps:get(<<"username">>, User),
                {ok, User1} = repo:get_user_by_username(Username),
                common:reply(Req0, ?HTTP_OK, #{<<"comment">> => format_comment(Comment, User1)});
            {error, Reason} ->
                common:reply(Req0, ?HTTP_INVALID, ?error_msg(Reason))
        end,
    {stop, Req1, State}.

comment_map_from_row(CommentRow) when is_tuple(CommentRow) ->
    comment_map_from_row(tuple_to_list(CommentRow));
comment_map_from_row(CommentRow) ->
    #{Key => Value || {Key, Value} <- lists:zip(?COMMENT_KEYS, CommentRow)}.

format_comment(Comment, User) ->
    AuthorInfo = #{<<"author">> => profile_h:build_profile(User)},
    Comment1 = maps:without(?COMMENT_EXCLUDE_KEYS, Comment),
    maps:merge(Comment1, AuthorInfo).

format_comments(Rows) ->
    CommentList =
        lists:map(
            fun(Row) ->
                {CommentRow, UserRow} = lists:split(length(?COMMENT_KEYS), tuple_to_list(Row)),
                format_comment(
                    comment_map_from_row(CommentRow),
                    user_h:user_map_from_row(UserRow)
                )
            end,
            Rows
        ),
    #{<<"comments">> => CommentList}.
