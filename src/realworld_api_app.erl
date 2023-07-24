%%%-------------------------------------------------------------------
%% @doc realworld_api public API
%% @end
%%%-------------------------------------------------------------------

-module(realworld_api_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    PathList = [
        {"/api/users[/login]", user_h, #{methods => [<<"POST">>]}},
        {"/api/user", user_h, #{methods => [<<"PUT">>, <<"GET">>]}},
        {"/api/profiles/:username", profile_h, #{methods => [<<"GET">>]}},
        {"/api/profiles/:username/follow", profile_h, #{methods => [<<"POST">>, <<"DELETE">>]}},

        %% collection
        {"/api/articles/", article_h, #{methods => [<<"GET">>, <<"POST">>]}},
        {"/api/articles/feed", article_h, #{methods => [<<"GET">>]}},

        %% single resource handler
        {"/api/articles/:slug", article_slug_h, #{methods => [<<"GET">>, <<"DELETE">>, <<"PUT">>]}},
        {"/api/articles/:slug/favorite", article_slug_h, #{methods => [<<"POST">>, <<"DELETE">>]}},

        %% comments
        {"/api/articles/:slug/comments", comment_h, #{methods => [<<"POST">>, <<"GET">>]}},
        {"/api/articles/:slug/comments/:id", comment_h, #{methods => [<<"DELETE">>]}},

        {"/api/tags", tag_h, #{methods => [<<"GET">>]}}
    ],

    Route = [{_HostMatch = '_', PathList}],

    Dispatch = cowboy_router:compile(Route),

    HTTPPort = application:get_env(realworld_api_app, server_port, 8090),

    {ok, _} = cowboy:start_clear(
        my_http_listener,
        [{port, HTTPPort}],
        #{env => #{dispatch => Dispatch}}
    ),

    {ok, _} = repo:init(),

    realworld_api_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
