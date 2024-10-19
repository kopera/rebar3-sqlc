# rebar3_sqlc

rebar3 plugin for compiling .sql SQL modules to Erlang modules.

## Use

Add the plugin to your rebar3 config:

```erlang
    {plugins, [rebar3_sqlc]}.

    {provider_hooks, [
        {pre, [
            {compile, {sqlc, compile}},
            {clean, {sqlc, clean}}
        ]}
    ]}.
```

Then create your SQL module file inside your project's src/ directory, for example as `src/my_app_queries.sql`

```sql

query user_by_id(:id uuid) returns user as
  select user_id as id, name, avatar_url, registered_at
  from user
  where user_id = :id;


mutation update_user(:id uuid, :updates jsonb) returns user as
  update user
    set
      name = case when :updates ? 'name' then :updates ->> 'name' else user.name end,
      avatar_url = case when :updates ? 'avatarUrl' then :updates ->> 'avatarUrl' else user.avatar_url end
    where user_id = :id
    returning user_id AS id, name, avatar_url, registered_at;
```

The SQL module above would produce an erlang module with 2 functions:

```erlang
%% @private
-module(my_app_queries).
-export([
    user_by_id/1,
    update_user/1
]).

user_by_id(#{id := Id}) ->
    #{
        type => query,
        name => {my_app_queries, user_by_id},
        statement => [
            <<
            "select user_id as id, name, avatar_url, registered_at\n",
            "  from user\n",
            "  where user_id = "
            >>, {parameter, #{key => {my_app_queries, user_by_id, id}, value => Id, type => <<"uuid">>}}
        ]
    }.

update_user(#{id := Id, updates := Updates}) ->
    #{
        type => mutation,
        name => {my_app_queries, update_user},
        statement => [
            <<"update user\n  set\n    name = case "
             "when ">>,
           {parameter,
            #{key => {my_app_queries, update_user, updates}, value => Updates,
              type => <<"jsonb">>}},
           <<" ? 'name' then ">>,
           {parameter,
            #{key => {my_app_queries, update_user, updates}, value => Updates,
              type => <<"jsonb">>}},
           <<" ->> 'name' else user.name end,\n    avatar_url = case when ">>,
           {parameter,
            #{key => {my_app_queries, update_user, updates}, value => Updates,
              type => <<"jsonb">>}},
           <<" ? 'avatarUrl' then ">>,
           {parameter,
            #{key => {my_app_queries, update_user, updates}, value => Updates,
              type => <<"jsonb">>}},
           <<" ->> 'avatarUrl' else user.avatar_url ">>,
             "end\n  where user_id = ">>,
           {parameter,
            #{key => {my_app_queries, update_user, id}, value => Id,
              type => <<"uuid">>}},
           <<"\n  returning user_id AS id, name, avatar_url, registered_at">>
        ]
    }.
```