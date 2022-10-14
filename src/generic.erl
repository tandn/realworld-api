-module(generic).

-export([init/2]).

-export([service_available/2,
         known_methods/2,
         uri_too_long/2,
         allowed_methods/2,
         malformed_request/2,
         is_authorized/2,
         forbidden/2,
         valid_content_headers/2,
         rate_limited/2]).

%% Content negotiation diagram

-export([content_types_provided/2,
         languages_provided/2,
         charset_provided/2,
         variances/2]).

-export([%% resource_exists/2,
         %% previously_existed/2,
         allow_missing_post/2,
         content_types_accepted/2]).

-export([hello_to_html/2,
	 hello_to_json/2,
	 hello_to_text/2,
	 hello_post/2]).

%% Switch to cowboy_rest handler
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

service_available(Req, State) ->
    io:format("~p~n",[?FUNCTION_NAME]),
    {true, Req, State}.

known_methods(Req, State) ->
    io:format("~p~n",[?FUNCTION_NAME]),
    Method = [<<"GET">>, <<"HEAD">>, <<"POST">>, <<"PUT">>, <<"PATCH">>, <<"DELETE">>, <<"OPTIONS">>],
    {Method, Req, State}.

uri_too_long(Req, State) ->
    io:format("~p~n",[?FUNCTION_NAME]),
    {false, Req, State}.

allowed_methods(Req, State) ->
    io:format("~p~n",[?FUNCTION_NAME]),
    Method = [<<"GET">>, <<"POST">>, <<"DELETE">>],
    {Method, Req, State}.

malformed_request(Req, State) ->
    io:format("~p~n",[?FUNCTION_NAME]),
    {false, Req, State}.

is_authorized(Req, State) ->
    io:format("~p~n",[?FUNCTION_NAME]),
    {true, Req, State}.

forbidden(Req, State) ->
    io:format("~p~n",[?FUNCTION_NAME]),
    {false, Req, State}.

valid_content_headers(Req, State) ->
    io:format("~p~n",[?FUNCTION_NAME]),
    {true, Req, State}.

rate_limited(Req, State) ->
    io:format("~p~n",[?FUNCTION_NAME]),
    {false, Req, State}.

content_types_provided(Req, State) ->
    io:format("~p~n",[?FUNCTION_NAME]),
    Map = [
           {<<"text/html">>, hello_to_html},
           {<<"application/json">>, hello_to_json},
           {<<"text/plain">>, hello_to_text}
          ],
    {Map, Req, State}.

languages_provided(Req, State) ->
    io:format("~p~n",[?FUNCTION_NAME]),
    L = [<<"en">>, <<"it">>],
    {L, Req, State}.

charset_provided(Req, State) ->
    io:format("~p~n",[?FUNCTION_NAME]),
    C = [<<"utf-8">>],
    {C, Req, State}.

variances(Req, State) ->
    io:format("~p~n",[?FUNCTION_NAME]),
    {[], Req, State}.

%% resource_exists(Req, State) ->
%%     io:format("~p~n",[?FUNCTION_NAME]),
%%     {true, Req, State}.

%% previously_existed(Req, State) ->
%%     io:format("~p~n",[?FUNCTION_NAME]),
%%     {false, Req, State}.

allow_missing_post(Req, State) ->
    io:format("~p~n",[?FUNCTION_NAME]),
    {true, Req, State}.

content_types_accepted(Req, State) ->
    io:format("~p~n",[?FUNCTION_NAME]),
    Map = [
           {<<"text/html">>, hello_post},
           {<<"application/json">>, hello_post},
           {<<"text/plain">>, hello_post}
          ],
    {Map, Req, State}.


%% Internal

hello_to_html(Req, State) ->
        Body = <<"<html>
<head>
        <meta charset=\"utf-8\">
        <title>REST Hello World!</title>
</head>
<body>
        <p>REST Hello World as HTML!</p>
</body>
</html>">>,
        {Body, Req, State}.

hello_to_json(Req, State) ->
        Body = <<"{\"rest\": \"Hello World!\"}">>,
        {Body, Req, State}.

hello_to_text(Req, State) ->
        {<<"REST Hello World as text!">>, Req, State}.


%% with post we getto provide reply and return
%% - true
%% - false if error
%%

hello_post(Req, State) ->
    io:fwrite("~p~n",[?FUNCTION_NAME]),
    Body = <<"{\"rest\": \"Hello World!\"}">>,
    Req1 = cowboy_req:set_resp_headers(
	     #{<<"content-type">> => <<"application/json; charset=utf-8">>,
	       <<"server">> => <<>>},
	     cowboy_req:set_resp_body(Body, Req)),
    {true, Req1, State}.
