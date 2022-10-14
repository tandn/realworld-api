-module(user_h).

-include("realworld.hrl").
%% cowboy_rest callbacks

-export([init/2,
	 allowed_methods/2,
	 malformed_request/2,
	 is_authorized/2,
	 content_types_provided/2,
	 content_types_accepted/2]).

-export([handle_create/2,
	 handle_login/2,
	 handle_update/2,
	 handle_read/2]).

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

is_authorized_1(Req, State) ->
    try
	Req0 = normalize_auth_header(Req),
	case cowboy_req:parse_header(<<"authorization">>, Req0, <<>>) of
	    {bearer, Token} ->
		Cookies = cowboy_req:parse_cookies(Req),
		{_, Key} = lists:keyfind(<<"key">>, 1, Cookies),
		User = repo:get_user_by_token(Token, Key),
		{true, Req, State#{<<"user">> => User, <<"key">> => Key, <<"token">> => Token}};
	    _ ->
		{{false, <<"Bearer Secure Area">>}, Req, State}
	end
    catch
	_C:_E ->
	    {{false, <<"invalid token">>}, Req, State}
    end.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_read}, {<<"text/plain">>, handle_read}], Req, State}.

content_types_accepted(Req, State) ->
    Path = cowboy_req:path(Req),
    {[{<<"application/json">>, to_callback(Path)}], Req, State}.

handle_login(Req, State) ->
    {ok, Data, Req0} = cowboy_req:read_body(Req),
    Req1 =
	case get_user(jsx:decode(Data), <<"login">>) of
	    {error, undefined} ->
		{stop, reply(Req0, ?HTTP_INVALID, ?error_msg([<<"Can't be empty">>])), State};
	    {error, Description} ->
		{stop, reply(Req0, ?HTTP_INVALID, ?error_msg(Description)), State};
	    {ok, []} ->
		{stop, reply(Req0, ?HTTP_NOT_FOUND), State};
	    {ok, User} ->
		Req2 = cowboy_req:set_resp_cookie(<<"key">>,
						  maps:get(<<"password">>, User),
						  Req0,
						  #{path => <<"/api/user">>}),

		reply(Req2, ?HTTP_OK,  #{<<"user">> => maps:remove(<<"password">>, User)})
	end,
    {stop, Req1, State}.

handle_create(Req, State) ->
    {ok, Data, Req0} = cowboy_req:read_body(Req),
    Req1 =
	case get_user(jsx:decode(Data), <<"create">>) of
	    {error, undefined} ->
		reply(Req0, ?HTTP_INVALID, ?error_msg([<<"Can't be empty">>]));
	    {error, Description} ->
		reply(Req0, ?HTTP_INVALID, ?error_msg(Description));
	    {ok, []} ->
		reply(Req0, ?HTTP_NOT_FOUND);
	    {ok, User} ->
		reply(Req0, ?HTTP_OK, #{<<"user">> => maps:remove(<<"password">>, User)})
	end,
    {stop, Req1, State}.

handle_update(Req, #{<<"user">> := User0, <<"key">> := Key} = State) ->
    {ok, Data, Req0} = cowboy_req:read_body(Req),
    #{<<"user">> := User} = jsx:decode(Data),

    User1 = repo:update_user(maps:merge(User, User0), Key),

    Req1 = reply(Req0, ?HTTP_OK, #{<<"user">> => maps:remove(<<"password">>, User1)}),

    {stop, Req1, State}.

handle_read(Req, #{<<"user">> := User, <<"token">> := Token} = State) ->
    {jsx:encode(#{<<"user">> => User#{<<"token">> => Token}}), Req, State}.

%% internal functions

get_user(#{<<"user">> := User}, Action) ->
    case {missing_keys(User, Action), invalid_keys(User)} of
	{[], []} ->
	    {ok, login_or_register_user(User, Action)};
	{[], IKs} ->
	    {error, [<<"Invalid keys:">> | IKs]};
	{MKs, _} ->
	    {error, [<<"Missing keys:">> | MKs]}
    end;
get_user(Body, _Path) ->
    {error, undefined}.

login_or_register_user(User, <<"login">>) ->
    repo:get_user(User);
login_or_register_user(User, <<"create">>) ->
    repo:create_user(User).

missing_keys(User, Action) ->
    lists:filter(fun(K) -> not maps:is_key(K, User) end, required_keys(Action)).

invalid_keys(User) ->
    lists:filter(fun(K) ->
			 V = maps:get(K, User),
			 not (is_binary(V) andalso size(V) > 0)
		 end, maps:keys(User)).

required_keys(<<"login">>) ->
    [<<"email">>, <<"password">>];
required_keys(<<"create">>) ->
    [<<"username">>, <<"email">>, <<"password">>].

to_callback(<<"/api/users">>) ->
    handle_create;
to_callback(<<"/api/users/login">>) ->
    handle_login;
to_callback(<<"/api/user">>) ->
    handle_update.

reply(Req, StatusCode) ->
    reply(Req, StatusCode, #{}).
reply(Req, StatusCode, Reply) ->
    cowboy_req:reply(
      StatusCode,
      ?no_server(#{<<"content-type">> => ?RESP_CONTENT_TYPE}),
      jsx:encode(Reply),
      Req).

normalize_auth_header(#{headers := Header} = Req) ->
    Auth = parse_authorization(maps:get(<<"authorization">>, Header)),
    Req#{headers => Header#{<<"authorization">> => Auth}}.

parse_authorization(<<T, O, K, E, N, " ", R/bits >>)
  when (R =/= <<>>), ((T =:= $T) or (T =:= $t)),
       ((O =:= $O) or (O =:= $o)), ((K =:= $K) or (K =:= $k)),
       ((E =:= $E) or (E =:= $e)), ((N =:= $N) or (N =:= $n)) ->
    << <<"Bearer">>/binary, " ", R/bits >>;
parse_authorization(Other) ->
    Other.
