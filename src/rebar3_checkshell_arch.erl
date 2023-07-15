-module(rebar3_checkshell_arch).

-include("rebar3_checkshell.hrl").

-export([do/2]).
-export([t/0]).

-define(SUCCESS, 0).

-type t() :: darwin | linux | win32.
-export_type([t/0]).

-export_type([nonempty_ubytes/0]).

-spec do(Files, State) -> Result when
    Files :: string(),
    Result :: {ok, State} | {error, nonempty_ubytes()}.
do(Files, State) ->
    InstallRes = rebar3_checkshell_inst:put_executables(),
    do(InstallRes, Files, State).

-spec do(InstallRes, Files, State) -> Result when
    InstallRes :: ok | {error, string()},
    Files :: string(),
    Result :: {ok, State} | {error, string()}.
do({error, E} = _InstallRes, _File, _State) ->
    {error, "checkshell: installation returned " ++ E};
do(ok = _InstallRes, Files, State) ->
    Cmd = rebar3_checkshell_inst:shellcheck_path(),
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
    RE = ".*(?P<A>darwin|linux|win32).*",
    Options = [{capture, ['A'], list}],
    {match, [Arch]} = re:run(Subject, RE, Options),
    list_to_existing_atom(Arch).

-spec args(Files, State) -> Result when
    Files :: string(),
    State :: term(),
    Result :: [string()].
args(Files, State) ->
    OptsFromRebarConfig = rebar_state:get(State, checkshell, []),
    OptsForShellCheck =
        lists:foldl(
            fun(OptFromRebarConfig, Acc) -> Acc ++ [opt(OptFromRebarConfig)] end,
            [""],
            OptsFromRebarConfig
        ),
    MaybeColor = proplists:get_value(color, OptsFromRebarConfig, undefined),
    OptsForShellCheck ++ [maybe_colorize(MaybeColor)] ++ [opt({files, Files})].

-spec maybe_colorize(Color) -> Result when
    Color :: undefined | auto | always | never,
    Result :: string().
maybe_colorize(undefined = _Color) ->
    "--color=always";
maybe_colorize(_Color) ->
    "".

-spec opt(Option) -> Result when
    Option :: atom() | {atom(), atom() | list() | string() | integer()},
    Result :: string().
opt(check_sourced) ->
    "--check-sourced";
opt({color, Color}) when Color =:= auto orelse Color =:= always orelse Color =:= never ->
    "--color=" ++ atom_to_list(Color);
opt({include, Includes}) ->
    "--include=" ++
        lists:foldl(
            fun
                (Include, "" = _Acc) when is_list(Include) ->
                    Include;
                (Include, Acc) when is_list(Include) ->
                    Acc ++ "," ++ Include;
                (Include, Acc) ->
                    _ = rebar_log:log(
                        warn,
                        "checkshell: non-string value for option include: ~p",
                        [Include]
                    ),
                    Acc
            end,
            "",
            Includes
        );
opt({exclude, Excludes}) ->
    "--exclude=" ++
        lists:foldl(
            fun
                (Exclude, "" = _Acc) when is_list(Exclude) ->
                    Exclude;
                (Exclude, Acc) when is_list(Exclude) ->
                    Acc ++ "," ++ Exclude;
                (Exclude, Acc) ->
                    _ = rebar_log:log(
                        warn,
                        "checkshell: non-string value for option exclude: ~p",
                        [Exclude]
                    ),
                    Acc
            end,
            "",
            Excludes
        );
opt({format, Format}) when
    Format =:= checkstyle orelse
        Format =:= diff orelse
        Format =:= gcc orelse
        Format =:= json orelse
        Format =:= json1 orelse
        Format =:= quiet orelse
        Format =:= tty
->
    "--format=" ++ atom_to_list(Format);
opt(list_optional) ->
    "--list-optional";
opt(norc) ->
    "--norc";
opt({enable, Checks}) when is_list(Checks) ->
    "--enable=" ++
        lists:foldl(
            fun
                (Check, "" = _Acc) when is_list(Check) ->
                    Check;
                (Check, Acc) when is_list(Check) ->
                    Acc ++ "," ++ Check;
                (Check, Acc) ->
                    _ = rebar_log:log(
                        warn,
                        "checkshell: non-string value for option enable: ~p",
                        [Check]
                    ),
                    Acc
            end,
            "",
            Checks
        );
opt({enable, all}) ->
    "--enable=all";
opt({source_paths, SourcePaths}) when is_list(SourcePaths) ->
    "--source-path=" ++ SourcePaths;
opt({shell, Shell}) when
    Shell =:= sh orelse Shell =:= bash orelse Shell =:= dash orelse Shell =:= ksh
->
    "--shell=" ++ atom_to_list(Shell);
opt({severity, Severity}) when
    Severity =:= error orelse
        Severity =:= warning orelse
        Severity =:= info orelse
        Severity =:= style
->
    "--severity=" ++ atom_to_list(Severity);
opt({wiki_link_count, Num}) when is_integer(Num) andalso Num > 0 ->
    "--wiki-link-count=" ++ integer_to_list(Num);
opt(external_sources) ->
    "--external-sources";
opt({files, Files}) when is_list(Files) ->
    "" ++ Files;
opt(UnknownOption) ->
    _ = rebar_log:log(
        warn,
        "checkshell: unknown rebar.config option ~p",
        [UnknownOption]
    ),
    "".

-spec output_shellcheck_analysis(Failure, AnalysisRes) -> Result when
    Failure :: pos_integer(),
    AnalysisRes :: string(),
    Result :: ok.
output_shellcheck_analysis(1 = _Failure, AnalysisRes) when length(AnalysisRes) > 1 ->
    _ = rebar_log:log(warn, "~p", [string:sub_string(AnalysisRes, 2)]);
output_shellcheck_analysis(_Failure, AnalysisRes) ->
    _ = rebar_log:log(warn, "~p", [AnalysisRes]).
