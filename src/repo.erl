-module(repo).

-export([get_user/1,
	 update_user/2,
	 get_user_by_token/2,
	 create_user/1]).

get_user(#{<<"email">> := Email}) ->
    Res = catch pgo:query("select * from users where email = $1",
			  [Email],
			  #{decode_opts => [return_rows_as_maps]}),
    case Res of
	#{rows := [User]} ->
	    {ok, Token} = jwt:encode(<<"HS256">>,
				     maps:remove(<<"password">>, User),
				     maps:get(<<"password">>, User)),

	    User#{<<"token">> => Token};
	_ ->
	    logger:error("select error", [Res]),
	    []
    end.

get_user_by_token(Token, Password) ->
    {ok, User} = jwt:decode(Token, Password),
    User.

%%
%% Need to return an JWT here
%%

create_user(#{<<"password">> := Password} = User) ->
    Bio = maps:get(<<"bio">>, User, <<>>),
    Image = maps:get(<<"image">>, User, <<>>),
    Key = list_to_binary(lists:flatten([[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= crypto:hash(md5, Password)]])),
    do_insert(User#{<<"bio">> => Bio, <<"image">> => Image}, Key).


%% user do update password, have to generate new token
update_user(#{<<"password">> := Password} = User, Key) ->
    Key0 = list_to_binary(lists:flatten([[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= crypto:hash(md5, Password)]])),
    do_update(maps:remove(<<"password">>, User), Key0, Key);
update_user(User, Key) ->
    do_update(User, Key, Key).

do_insert(User, Key) ->
    #{<<"username">> := UserName,
      <<"email">> := Email,
      <<"bio">> := Bio,
      <<"image">> := Image} = User,

    Res = catch pgo:query("insert into users (username, email, password, bio, image) values ($1, $2, $3, $4, $5)",
			  [UserName, Email, Key, Bio, Image]),
    case Res of
	#{rows := _Rows} ->
	    get_user(User);
	_ ->
	    logger:error("insert error", [Res]),
	    []
    end.

do_update(User, Password, Key) ->
    #{<<"username">> := UserName,
      <<"email">> := Email,
      <<"bio">> := Bio,
      <<"image">> := Image} = User,

    Res = catch pgo:query("update users set username = $1, email = $2, password = $3, bio = $4, image = $5 where password = $6",
			  [UserName, Email, Password, Bio, Image, Key]),
    case Res of
	#{rows := _Rows} ->
	    get_user(User);
	_ ->
	    logger:error("update error", [Res]),
	    []
    end.
