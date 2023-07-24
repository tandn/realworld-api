%% https://realworld-docs.netlify.app/docs/specs/backend-specs/error-handling

-define(HTTP_OK, 200).
-define(HTTP_UN_AUTHORIZED, 401).
-define(HTTP_FORBIDDEN, 403).
-define(HTTP_NOT_FOUND, 404).
-define(HTTP_INVALID, 422).

%% https://realworld-docs.netlify.app/docs/specs/backend-specs/api-response-format

-define(TOKEN, <<"jwt.token.here">>).
-define(error_msg(Description), #{<<"error">> => #{<<"body">> => Description}}).
-define(RESP_CONTENT_TYPE, <<"application/json; charset=utf-8">>).

%% @private
-define(no_server(Header), Header#{<<"server">> => <<>>}).

-define(DEFAULT_JWT_KEY, <<"default_key">>).

-define(ARTICLE_KEYS, [
    <<"id">>,
    <<"slug">>,
    <<"title">>,
    <<"body">>,
    <<"description">>,
    <<"createdAt">>,
    <<"updatedAt">>,
    <<"favoritesCount">>,
    <<"author_id">>,
    <<"tagList">>
]).

-define(ARTICLE_EXCLUDE_KEYS, [<<"id">>, <<"author_id">>]).

-define(USER_KEYS, [
    <<"id">>,
    <<"username">>,
    <<"email">>,
    <<"password">>,
    <<"bio">>,
    <<"image">>,
    <<"followed_by">>
]).

-define(USER_EXCLUDE_KEYS, [
    <<"id">>,
    <<"password">>,
    <<"followed_by">>
]).

-define(COMMENT_KEYS, [
    <<"id">>,
    <<"body">>,
    <<"author_id">>,
    <<"slug">>,
    <<"createdAt">>,
    <<"updatedAt">>
]).

-define(COMMENT_EXCLUDE_KEYS, [
    <<"author_id">>,
    <<"slug">>
]).

-define(PROFILE_KEYS, [<<"username">>, <<"bio">>, <<"image">>, <<"following">>]).
