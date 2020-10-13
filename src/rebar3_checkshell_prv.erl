-module(rebar3_checkshell_prv).

-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-ignore_xref([init/1]).
-ignore_xref([do/1]).
-ignore_xref([format_error/1]).

-define(PROVIDER, rebar3_checkshell).
-define(DEPS, [app_discovery]).

init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, []},
                                 {desc, "A rebar3 plugin to ease shellcheck'ing"},
                                 {short_desc, "A rebar3 plugin to ease shellcheck'ing"},
                                 {example, "rebar3 checkshell"},
                                 {opts, []}]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    {ok, State}.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).
