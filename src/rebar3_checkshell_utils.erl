-module(rebar3_checkshell_utils).

-export([priv_dir/0]).

-type str() :: [1..255, ...].
-export_type([str/0]).

-spec priv_dir() -> Result
      when Result :: file:filename().
priv_dir() ->
    code:priv_dir(rebar3_checkshell).
