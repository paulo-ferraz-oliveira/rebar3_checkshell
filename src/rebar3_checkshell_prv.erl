-module(rebar3_checkshell_prv).

-export([init/1]).
-export([do/1]).
-export([format_error/1]).

-ignore_xref([init/1]).
-ignore_xref([do/1]).
-ignore_xref([format_error/1]).

-define(PROVIDER, checkshell).

-spec init(State) -> Result when
    Result :: {ok, State}.
init(State) ->
    Provider =
        providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {bare, true},
            {deps, []},
            {desc, "A rebar3 plugin to ease shellcheck'ing, " ++ version()},
            {short_desc, "A rebar3 plugin to ease shellcheck'ing"},
            {example, "rebar3 checkshell"},
            {opts, [{files, $f, "files", string, "Files to check"}]}
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(State) -> Result when
    Result :: {error, rebar3_checkshell_utils:str()} | {ok, State}.
do(State) ->
    Files = get_arg(files, State),
    do_for(Files, State).

-spec do_for(Files, State) -> Result when
    Files :: undefined | string(),
    Result :: {ok, State} | {error, rebar3_checkshell_utils:str()}.
do_for(undefined = _Files, _State) ->
    {error, "checkshell: missing --files"};
do_for(Files, State) ->
    _ = rebar_log:log(
        info, "rebar3_checkshell analysis starting, this may take a while...", []
    ),
    rebar3_checkshell_arch:do(Files, State).

-spec format_error(Reason) -> Result when
    Reason :: term(),
    Result :: string().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec version() -> Result when
    Result :: rebar3_checkshell_utils:str().
version() ->
    VersionFile = filename:join(rebar3_checkshell_utils:priv_dir(), "VERSION"),
    {ok, Version} = file:read_file(VersionFile),
    binary_to_list(Version).

-spec get_arg(Arg, State) -> Result when
    Arg :: files,
    State :: term(),
    Result :: undefined | string().
get_arg(Arg, State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    proplists:get_value(Arg, Args).
