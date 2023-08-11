% @private
-module(rebar3_checkshell).

-export([init/1]).
-ignore_xref([init/1]).

-export([main/1]).
-ignore_xref([main/1]).

-spec init(State) -> Result when
    Result :: {ok, State}.
init(State) ->
    {ok, _Started} = application:ensure_all_started(rebar3_checkshell),
    {ok, _State} = rebar3_checkshell_prv:init(State).

-spec main(Files) -> Result when
    Files :: [string()],
    Result :: ok.
main(Files) ->
    rebar3_checkshell_prv:do_for(Files, rebar_state:new()),
    ok.
