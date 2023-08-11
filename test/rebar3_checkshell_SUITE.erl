-module(rebar3_checkshell_SUITE).

-define(REBAR3_NEW_STATE, rebar_state:new()).

-compile([export_all, nowarn_export_all]).

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

%
% Tests.

empty_ok(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Empty = filename:join(DataDir, "empty.sh"),
    {ok, _} = rebar3_checkshell_prv:do_for([Empty], ?REBAR3_NEW_STATE).

fish_nok(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    Fish = filename:join(DataDir, "fish.sh"),
    {error, _} = rebar3_checkshell_prv:do_for([Fish], ?REBAR3_NEW_STATE).
