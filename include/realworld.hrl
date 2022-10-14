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
