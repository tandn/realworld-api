%%%-------------------------------------------------------------------
%% @doc realworld_api public API
%% @end
%%%-------------------------------------------------------------------

-module(realworld_api_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    HostMatch = '_',
    PathList = [{"/api/users[/login]",  user_handler_h, #{methods => [<<"POST">>]}},
		{"/api/user", user_handler_h, #{methods => [<<"PUT">>, <<"GET">>]}}
	       ],
    Route = [{HostMatch, PathList}],    

    Dispatch = cowboy_router:compile(Route),
    {ok, _} = cowboy:start_clear(my_http_listener, 
				 [{port, 4001}],
				 #{env => #{dispatch => Dispatch}}),
		       
    realworld_api_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
