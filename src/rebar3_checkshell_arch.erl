% @private
-module(rebar3_checkshell_arch).

-include("rebar3_checkshell.hrl").

-export([do/2]).
-export([t/0]).

-define(SUCCESS, 0).

-type t() :: darwin | linux | windows.
-export_type([t/0]).

-export_type([nonempty_ubytes/0]).

-elvis([{elvis_style, no_debug_call, disable}]).

-spec do(Files, State) -> Result when
    Files :: [string()],
    Result :: {ok, State} | {error, nonempty_ubytes()}.
do(Files, State) ->
    InstallRes = rebar3_checkshell_inst:put_executables(State),
    do(InstallRes, Files, State).

-spec do(InstallRes, Files, State) -> Result when
    InstallRes :: ok | {error, string()},
    Files :: [string()],
    Result :: {ok, State} | {error, string()}.
do({error, E} = _InstallRes, _File, _State) ->
    {error, "checkshell: installation returned " ++ E};
do(ok = _InstallRes, Files, State) ->
    Cmd = rebar3_checkshell_inst:shellcheck_path(State),
    Args = args(Files, State),
    result(rebar3_checkshell_utils:cmd(Cmd, Args), State).

-spec result({ExitCode, Analysis}, State) -> Result when
    ExitCode :: non_neg_integer(),
    Analysis :: string(),
    Result :: {ok, State} | {error, nonempty_ubytes()}.
result({?SUCCESS, _AnalysisRes}, State) ->
    {ok, State};
result({Failure, AnalysisRes}, _State) ->
    output_shellcheck_analysis(Failure, AnalysisRes),
    {error, "checkshell: ShellCheck exited with error"}.

-spec t() -> Result when
    Result :: t().
t() ->
    Subject = erlang:system_info(system_architecture),
    RE = ".*(?P<A>darwin|linux|windows).*",
    Options = [{capture, ['A'], list}],
    {match, [Arch]} = re:run(Subject, RE, Options),
    list_to_existing_atom(Arch).

-spec args(Files, State) -> Result when
    Files :: [string()],
    State :: term(),
    Result :: [string()].
args(Files, State) ->
    OptsFromRebarConfig = rebar3_checkshell_prv:opts(State),
    OptsForShellCheck =
        lists:foldl(
            fun
                ({files, _Files}, Acc) ->
                    % handled separately
                    Acc;
                (OptFromRebarConfig, Acc) ->
                    Acc ++ [opt(OptFromRebarConfig)]
            end,
            [""],
            OptsFromRebarConfig
        ),
    MaybeColor = rebar3_checkshell_prv:opt(State, color, undefined),
    OptsForShellCheck ++ [maybe_colorize(MaybeColor)] ++ opt({files, Files}).

-spec maybe_colorize(Color) -> Result when
    Color :: undefined | auto | always | never,
    Result :: string().
maybe_colorize(undefined = _Color) ->
    "--color=always";
maybe_colorize(_Color) ->
    "".

-spec opt(Option) -> Result when
    Option :: atom() | {atom(), atom() | list() | string() | integer()},
    Result :: string() | list().
opt(check_sourced) ->
    "--check-sourced";
opt({color, Color}) ->
    "--color=" ++ atom_to_list(Color);
opt({include, Includes}) ->
    "--include=" ++ string:join(Includes, ",");
opt({exclude, Excludes}) ->
    "--exclude=" ++ string:join(Excludes, ",");
opt({format, Format}) ->
    "--format=" ++ atom_to_list(Format);
opt(list_optional) ->
    "--list-optional";
opt(norc) ->
    "--norc";
opt({enable, all}) ->
    "--enable=all";
opt({enable, Checks}) ->
    "--enable=" ++ string:join(Checks, ",");
opt({source_path, SourcePath}) ->
    "--source-path=" ++ SourcePath;
opt({shell, Shell}) ->
    "--shell=" ++ atom_to_list(Shell);
opt({severity, Severity}) ->
    "--severity=" ++ atom_to_list(Severity);
opt({wiki_link_count, Num}) ->
    "--wiki-link-count=" ++ integer_to_list(Num);
opt(external_sources) ->
    "--external-sources";
opt({files, Files}) ->
    Files;
opt(UnknownOption) ->
    _ = rebar3_checkshell_utils:log(
        warn,
        "unknown rebar.config option ~p",
        [UnknownOption]
    ),
    "".

-spec output_shellcheck_analysis(Failure, AnalysisRes) -> Result when
    Failure :: pos_integer(),
    AnalysisRes :: string(),
    Result :: ok.
output_shellcheck_analysis(1 = _Failure, AnalysisRes) when length(AnalysisRes) > 1 ->
    io:format("~s", [string:sub_string(AnalysisRes, 2)]);
output_shellcheck_analysis(_Failure, AnalysisRes) ->
    rebar3_checkshell_utils:log(warn, "~p", [AnalysisRes], false).
