-module(rebar3_checkshell_prv).

-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-ignore_xref([init/1]).
-ignore_xref([do/1]).
-ignore_xref([format_error/1]).

-define(PROVIDER, checkshell).

init(State) ->
    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, []},
                                 {desc, "A rebar3 plugin to ease shellcheck'ing, " ++ version()},
                                 {short_desc, "A rebar3 plugin to ease shellcheck'ing"},
                                 {example, "rebar3 checkshell"},
                                 {opts, opts()}]),
    {ok, rebar_state:add_provider(State, Provider)}.

do(State) ->
    Files = get_arg(files, State),
    case Files of
        undefined ->
            {error, "checkshell: missing --files"};
        _ ->
            rebar3_checkshell_arch:do(Files, State)
    end.

format_error(Reason) ->
    io_lib:format("~p", [Reason]).

opts() ->
    [{files, $f, "files", string, "Files to check"}].

version() ->
    {ok, Version} = file:read_file(rebar3_checkshell_utils:priv_dir() ++ "/VERSION"),
    binary_to_list(Version).

get_arg(Arg, State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    proplists:get_value(Arg, Args).
