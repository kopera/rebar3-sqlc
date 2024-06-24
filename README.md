# rebar3_sqlc


rebar3 plugin for compiling .sql SQL modules to Erlang modules.

## Build

    $ rebar3 compile

## Use

Add the plugin to your rebar3 config:

```erlang
    {plugins, [
        {rebar3_sqlc, {git, "https://github.com/kopera/erlang-rebar3-sqlc.git", {branch, "main"}}}
    ]}.

    {provider_hooks, [
        {pre, [
            {compile, {sqlc, compile}},
            {clean, {sqlc, clean}}
        ]}
    ]}.
```