-module(article_h).

-include("realworld.hrl").

%% cowboy_rest callbacks
-export([init/2,
	 allowed_methods/2,
	 malformed_request/2,
	 is_authorized/2,
	 content_types_provided/2,
	 content_types_accepted/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, #{methods := MethodList} = State) ->
    {MethodList, Req, State}.

malformed_request(#{method := <<"GET">>} = Req, State) ->
    {false, Req, State};

malformed_request(Req, State) ->
    case cowboy_req:has_body(Req) of
	false ->
	    Req1 = reply(Req, ?HTTP_INVALID, ?error_msg([<<"Can't be empty">>])),
	    {stop, Req1, State};
	true ->
	    {false, Req, State}
    end.

is_authorized(#{method := <<"GET">>} = Req, State) ->
    is_authorized_1(Req, State);
is_authorized(#{method := <<"PUT">>} = Req, State) ->
    is_authorized_1(Req, State);
is_authorized(Req, State) ->
    {true, Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_read}, {<<"text/plain">>, handle_read}], Req, State}.

content_types_accepted(Req, State) ->
    Path = cowboy_req:path(Req),
    {[{<<"application/json">>, to_callback(Path)}], Req, State}.
