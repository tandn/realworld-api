-module(tag_h).

-include("realworld.hrl").

-export([init/2]).

-export([
    allowed_methods/2,
    content_types_provided/2
]).

-export([handle_get/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, #{methods := MethodList} = State) ->
    {MethodList, Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_get}], Req, State}.

handle_get(Req, State) ->
    {ok, TagList} = repo:get_tags(),
    Req1 = common:reply(Req, ?HTTP_OK, #{<<"tags">> => TagList}),
    {stop, Req1, State}.
