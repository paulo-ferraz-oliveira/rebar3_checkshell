-module(rebar3_checkshell_SUITE).

-compile([export_all, nowarn_export_all, nowarn_missing_spec_all]).

-hank([{unnecessary_function_arguments, [{end_per_testcase, 2}, {init_per_testcase, 2}]}]).

%
%% Configuration.

all() ->
    [
        Fun
     || {Fun, 1} <- ?MODULE:module_info(exports),
        not lists:member(Fun, [module_info, init_per_suite, end_per_suite])
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(rebar3_checkshell),
    Config.

end_per_suite(Config) ->
    ok = application:stop(rebar3_checkshell),
    Config.

init_per_testcase(_TestCase, Config) ->
    meck:new(rebar3_checkshell_prv, [passthrough]),
    Config.

end_per_testcase(_TestCase, _Config) ->
    meck:unload(rebar3_checkshell_prv).

%
%% Tests.

files_empty_ok(Config) ->
    mock_opts(Config, []),
    {ok, _} = exec().

files_fish_nok(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    FishSh = filename:join(DataDir, "fish.sh"),
    mock_opts(Config, [{files, [FishSh]}]),
    {error, _} = exec().

opt_color_auto(Config) ->
    mock_opts(Config, [{color, auto}]),
    {ok, _} = exec().

opt_check_sourced(Config) ->
    mock_opts(Config, [check_sourced]),
    {ok, _} = exec().

opt_include(Config) ->
    mock_opts(Config, [{include, ["SC1020", "SC2010"]}]),
    {ok, _} = exec().

opt_exclude(Config) ->
    mock_opts(Config, [{exclude, ["SC2011", "SC1120"]}]),
    {ok, _} = exec().

opt_format(Config) ->
    mock_opts(Config, [{format, diff}]),
    {ok, _} = exec().

opt_list_optional(Config) ->
    mock_opts(Config, [list_optional]),
    {ok, _} = exec().

opt_no_rc(Config) ->
    mock_opts(Config, [norc]),
    {ok, _} = exec().

opt_enable_all(Config) ->
    mock_opts(Config, [{enable, all}]),
    {ok, _} = exec().

opt_enable(Config) ->
    mock_opts(Config, [{enable, ["add-default-case", "deprecate-which"]}]),
    {ok, _} = exec().

opt_enable_checks(Config) ->
    mock_opts(Config, [no_rc]),
    {ok, _} = exec().

opt_source_path(Config) ->
    mock_opts(Config, [{source_path, "."}]),
    {ok, _} = exec().

opt_shell(Config) ->
    mock_opts(Config, [{shell, ksh}]),
    {ok, _} = exec().

opt_severity(Config) ->
    mock_opts(Config, [{severity, warning}]),
    {ok, _} = exec().

opt_wiki_link_count(Config) ->
    mock_opts(Config, [{wiki_link_count, 10}]),
    {ok, _} = exec().

opt_external_sources(Config) ->
    mock_opts(Config, [external_sources]),
    {ok, _} = exec().

opt_checksum(Config) ->
    mock_opts(Config, [{checksum, false}]),
    {ok, _} = exec().

opt_unknown(Config) ->
    mock_opts(Config, [{unknown_opt1, false}]),
    {ok, _} = exec().

%
%% Internal.

exec() ->
    rebar3_checkshell_prv:do(rebar_state:new()).

mock_opts(Config, Opts0) ->
    Opts =
        case proplists:get_value(files, Opts0) of
            undefined ->
                DataDir = proplists:get_value(data_dir, Config),
                EmptySh = filename:join(DataDir, "empty.sh"),
                Opts0 ++ [{files, [EmptySh]}];
            _ ->
                Opts0
        end,
    meck:expect(rebar3_checkshell_prv, opts, fun(_State) -> Opts end).
