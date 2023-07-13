-module(rebar3_checkshell_arch).

-export([do/2]).

-define(SUCCESS, 0).

-spec do(Files, State) -> Result when
    Files :: string(),
    Result :: {ok, State} | {error, rebar3_checkshell_utils:str()}.
do(Files, State) ->
    do(get_archs(), Files, State).

-spec do(Archs, Files, State) -> Result when
    Archs ::
        {IsMacOS :: true, IsLinux :: false, IsWindows :: false}
        | {IsMacOS :: false, IsLinux :: true, IsWindows :: false}
        | {IsMacOS :: false, IsLinux :: false, IsWindows :: true},
    Files :: string(),
    Result :: {ok, State} | {error, rebar3_checkshell_utils:str()}.
do({true = _IsMacOS, false = _IsLinux, false = _IsWindows}, Files, State) ->
    execute(darwin, Files, State);
do({false = _IsMacOS, true = _IsLinux, false = _IsWindows}, Files, State) ->
    execute(linux, Files, State);
do({false = _IsMacOS, false = _IsLinux, true = _IsWindows}, _Files, _State) ->
    {error, "checkshell: no support for Windows yet"}.

-spec execute(Arch, Files, State) -> Result when
    Arch :: darwin | linux,
    Files :: string(),
    Result :: {ok, State} | {error, rebar3_checkshell_utils:str()}.
execute(Arch, Files, State) ->
    ArchDir = filename:join(rebar3_checkshell_utils:priv_dir(), atom_to_list(Arch) ++ ".x86_64"),
    Exec = filename:join(ArchDir, "shellcheck"),
    OpenPortCmd = {spawn, Exec ++ args(Files, State)},
    OpenPortOpts = [exit_status],
    result(port_loop(erlang:open_port(OpenPortCmd, OpenPortOpts), ""), State).

-spec result({ExitCode, Analysis}, State) -> Result when
    ExitCode :: non_neg_integer(),
    Analysis :: string(),
    Result :: {ok, State} | {error, rebar3_checkshell_utils:str()}.
result({?SUCCESS, _AnalysisRes}, State) ->
    {ok, State};
result({Failure, AnalysisRes}, _State) ->
    output_shellcheck_analysis(Failure, AnalysisRes),
    {error, "checkshell: ShellCheck exited with error"}.

-spec get_archs() -> Result when
    Result :: {IsMacOS :: boolean(), IsLinux :: boolean(), IsWindows :: boolean()}.
get_archs() ->
    Arch = rebar_utils:get_arch(),
    IsMacOS = re:run(Arch, "darwin") =/= nomatch,
    IsLinux = re:run(Arch, "linux") =/= nomatch,
    IsWindows = re:run(Arch, "win32") =/= nomatch,
    {IsMacOS, IsLinux, IsWindows}.

-spec args(Files, State) -> Result when
    Files :: string(),
    State :: term(),
    Result :: rebar3_checkshell_utils:str().
args(Files, State) ->
    OptsFromRebarConfig = rebar_state:get(State, checkshell, []),
    OptsForShellCheck =
        lists:foldl(
            fun(OptFromRebarConfig, Acc) -> Acc ++ opt(OptFromRebarConfig) end,
            "",
            OptsFromRebarConfig
        ),
    MaybeColor = proplists:get_value(color, OptsFromRebarConfig, undefined),
    OptsForShellCheck ++ maybe_colorize(MaybeColor) ++ opt({files, Files}).

-spec maybe_colorize(Color) -> Result when
    Color :: undefined | auto | always | never,
    Result :: string().
maybe_colorize(undefined) ->
    " --color=always";
maybe_colorize(_) ->
    "".

-spec opt(Option) -> Result when
    Option :: atom() | {atom(), atom() | list() | string() | integer()},
    Result :: rebar3_checkshell_utils:str().
opt(check_sourced) ->
    " --check-sourced";
opt({color, Color}) when Color =:= auto orelse Color =:= always orelse Color =:= never ->
    " --color=" ++ atom_to_list(Color);
opt({include, Includes}) ->
    " --include=" ++
        lists:foldl(
            fun
                (Include, "" = _Acc) when is_list(Include) ->
                    Include;
                (Include, Acc) when is_list(Include) ->
                    Acc ++ "," ++ Include;
                (Include, Acc) ->
                    rebar_api:warn(
                        "checkshell: non-string value for option include: ~p",
                        [Include]
                    ),
                    Acc
            end,
            "",
            Includes
        );
opt({exclude, Excludes}) ->
    " --exclude=" ++
        lists:foldl(
            fun
                (Exclude, "" = _Acc) when is_list(Exclude) ->
                    Exclude;
                (Exclude, Acc) when is_list(Exclude) ->
                    Acc ++ "," ++ Exclude;
                (Exclude, Acc) ->
                    rebar_api:warn(
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
    " --format=" ++ atom_to_list(Format);
opt(list_optional) ->
    " --list-optional";
opt(norc) ->
    " --norc";
opt({enable, Checks}) when is_list(Checks) ->
    " --enable=" ++
        lists:foldl(
            fun
                (Check, "" = _Acc) when is_list(Check) ->
                    Check;
                (Check, Acc) when is_list(Check) ->
                    Acc ++ "," ++ Check;
                (Check, Acc) ->
                    rebar_api:warn(
                        "checkshell: non-string value for option enable: ~p",
                        [Check]
                    ),
                    Acc
            end,
            "",
            Checks
        );
opt({enable, all}) ->
    " --enable=all";
opt({source_paths, SourcePaths}) when is_list(SourcePaths) ->
    " --source-path=" ++ SourcePaths;
opt({shell, Shell}) when
    Shell =:= sh orelse Shell =:= bash orelse Shell =:= dash orelse Shell =:= ksh
->
    " --shell=" ++ atom_to_list(Shell);
opt({severity, Severity}) when
    Severity =:= error orelse
        Severity =:= warning orelse
        Severity =:= info orelse
        Severity =:= style
->
    " --severity=" ++ atom_to_list(Severity);
opt({wiki_link_count, Num}) when is_integer(Num) andalso Num > 0 ->
    " --wiki-link-count=" ++ integer_to_list(Num);
opt(external_sources) ->
    " --external-sources";
opt({files, Files}) when is_list(Files) ->
    " " ++ Files;
opt(UnknownOption) ->
    rebar_api:warn("checkshell: unknown rebar.config option ~p", [UnknownOption]),
    "".

-spec port_loop(Port, Data) -> Result when
    Port :: port(),
    Data :: string(),
    Result :: {ExitStatus :: non_neg_integer(), Data}.
port_loop(Port, Data) ->
    receive
        {Port, {data, MoreData}} ->
            port_loop(Port, Data ++ MoreData);
        {Port, {exit_status, ExitStatus}} ->
            {ExitStatus, Data}
    end.

-spec output_shellcheck_analysis(Failure, AnalysisRes) -> Result when
    Failure :: pos_integer(),
    AnalysisRes :: rebar3_checkshell_utils:str(),
    Result :: ok.
output_shellcheck_analysis(1 = _Failure, AnalysisRes) when length(AnalysisRes) > 1 ->
    rebar_api:warn("~s", [string:sub_string(AnalysisRes, 2)]);
output_shellcheck_analysis(_Failure, AnalysisRes) ->
    rebar_api:warn("~s", [AnalysisRes]).
