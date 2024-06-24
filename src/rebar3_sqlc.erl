-module(rebar3_sqlc).

-export([init/1]).

-spec init(State) -> {ok, State} when
    State :: rebar_state:t().
init(State) ->
    {ok, rebar3_sqlc_prv_compile:init(State)}.
