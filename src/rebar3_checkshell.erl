-module(rebar3_checkshell).

-export([init/1]).

-ignore_xref([init/1]).

init(State) ->
    {ok, State1} = rebar3_checkshell_prv:init(State),
    {ok, State1}.
