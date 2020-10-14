-module(rebar3_checkshell).

-export([init/1]).

-ignore_xref([init/1]).

-spec init(State) -> Result
      when Result :: {ok, State}.
init(State) ->
    {ok, State1} = rebar3_checkshell_prv:init(State),
    {ok, State1}.
