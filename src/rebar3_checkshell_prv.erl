% @private
-module(rebar3_checkshell_prv).

-include("rebar3_checkshell.hrl").

-export([init/1]).
-ignore_xref([init/1]).

-export([do/1]).
-ignore_xref([do/1]).

-export([do_for/2]).
-ignore_xref([do_for/2]).

-export([format_error/1]).
-ignore_xref([format_error/1]).

-export([opts/1]).
-ignore_xref([opts/1]).

-export([opt/2]).
-ignore_xref([opt/2]).

-export([opt/3]).
-ignore_xref([opt/3]).

-define(PROVIDER, checkshell).

-export_type([nonempty_ubytes/0]).

-type opt() :: atom() | list() | string() | integer() | true.
-export_type([opt/0]).

-spec init(State) -> Result when
    Result :: {ok, State}.
init(State) ->
    Provider =
        providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {bare, true},
            {deps, []},
            {desc, "A rebar3 plugin to ease shellcheck'ing"},
            {short_desc, "A rebar3 plugin to ease shellcheck'ing"},
            {example, "rebar3 checkshell"},
            {opts, [
                {files, $f, "files", string,
                    "Files to check (merged with option from rebar.config)"}
            ]}
        ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(State) -> Result when
    Result :: {ok, State} | {error, nonempty_ubytes()}.
do(State) ->
    FilesFromCLI = files_from_cli(State),
    FilesFromRebarConfig = files_from_rebar_config(State),
    AbsOptFiles = lists:keymerge(1, FilesFromCLI, FilesFromRebarConfig),
    Files = [Opt || {_Abs, Opt} <- AbsOptFiles],
    do_for(Files, State).

-spec do_for(Files, State) -> Result when
    Files :: [string()],
    Result :: {ok, State} | {error, nonempty_ubytes()}.
do_for(Files, State) ->
    _ = rebar3_checkshell_utils:log(
        info, "checkshell: analysis starting. This may take a while...", []
    ),
    rebar3_checkshell_arch:do(Files, State).

-spec format_error(Reason) -> Result when
    Reason :: term(),
    Result :: string().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec opts(State) -> Result when
    State :: rebar_state:t(),
    Result :: [opt()].
opts(State) ->
    rebar_state:get(State, checkshell, []).

-spec opt(State, Opt) -> Result when
    State :: rebar_state:t(),
    Opt :: atom(),
    Result :: undefined | opt().
opt(State, Opt) ->
    opt(State, Opt, undefined).

-spec opt(State, Opt, Default) -> Result when
    State :: rebar_state:t(),
    Opt :: atom(),
    Default :: term(),
    Result :: undefined | opt().
opt(State, Opt, Default) ->
    Opts = ?MODULE:opts(State),
    proplists:get_value(Opt, Opts, Default).

-spec files_from_cli(State) -> Result when
    State :: term(),
    Result :: [{Abs :: string(), Opt :: string()}].
files_from_cli(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    OptFiles0 = proplists:get_value(files, Args, []),
    OptFiles = string:split(OptFiles0, ",", all),
    [{filename:absname(OptFile), OptFile} || OptFile <- OptFiles].

-spec files_from_rebar_config(State) -> Result when
    State :: term(),
    Result :: [{Abs :: string(), Opt :: string()}].
files_from_rebar_config(State) ->
    OptFiles = opt(State, files, []),
    [{filename:absname(OptFile), OptFile} || OptFile <- OptFiles].
