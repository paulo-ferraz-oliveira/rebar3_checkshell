-module(rebar3_checkshell_utils).

-export([priv_dir/0]).

priv_dir() ->
    code:priv_dir(rebar3_checkshell).
