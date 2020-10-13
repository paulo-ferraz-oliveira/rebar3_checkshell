-module(rebar3_checkshell).

-export([init/1]).

-ignore_xref([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_checkshell_prv:init(State),
    {ok, State1}.

%FIXME
