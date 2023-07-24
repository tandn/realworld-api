Rest API Example
=====

A few rest APIs [realworld_api](https://realworld-docs.netlify.app/docs/specs/backend-specs/endpoints) implemented in `Erlang`.

## Router

```erlang
    PathList = [
        {"/api/users[/login]", user_h, #{methods => [<<"POST">>]}},
        {"/api/user", user_h, #{methods => [<<"PUT">>, <<"GET">>]}},
        {"/api/profiles/:username", profile_h, #{methods => [<<"GET">>]}},
        {"/api/profiles/:username/follow", profile_h, #{methods => [<<"POST">>, <<"DELETE">>]}},

        {"/api/articles/", article_h, #{methods => [<<"GET">>, <<"POST">>]}},
        {"/api/articles/feed", article_h, #{methods => [<<"GET">>]}},

        {"/api/articles/:slug", article_slug_h, #{methods => [<<"GET">>, <<"DELETE">>, <<"PUT">>]}},
        {"/api/articles/:slug/favorite", article_slug_h, #{methods => [<<"POST">>, <<"DELETE">>]}},

        %% comments
        {"/api/articles/:slug/comments", comment_h, #{methods => [<<"POST">>, <<"GET">>]}},
        {"/api/articles/:slug/comments/:id", comment_h, #{methods => [<<"DELETE">>]}},

        {"/api/tags", tag_h, #{methods => [<<"GET">>]}}
    ],
```

# Config

- config/sys.config

```erlang
 [{server_port, 8090},
   {db_config,
    #{
      pool_size => 10,
      host => "192.168.43.47",
      port => 5432,
      username => "postgres",
      password => "postgres",
      database => "postgres"
     }
   },
   {jwt_key, <<"default-key">>}
  ]
```

## Run

```
$ rebar3 shell
```

## Test

```
$ cd run_api_test
$ ./run_api_test.sh
```

## TODO
- add tests
- optional endpoints
- cowboy_swaggger
