% @private
-module(rebar3_checkshell_utils).

-export([cmd/2]).
-export([log/3]).
-export([log/4]).

-elvis([{elvis_style, no_debug_call, #{ignore => [{rebar3_checkshell_utils, ct_pal_log}]}}]).
-hank([{unnecessary_function_arguments, [{ct_pal_log, 3}]}]).

-spec cmd(Cmd, Args) -> Result when
    Cmd :: string(),
    Args :: [string()],
    Result :: {ExitStatus :: non_neg_integer(), Data :: string()}.
cmd(Cmd, Args0) ->
    Args = [" " ++ Arg || Arg <- Args0, Arg =/= ""],
    PortName = {spawn, Cmd ++ Args},
    PortSettings = [exit_status],
    Port = open_port(PortName, PortSettings),
    port_loop(Port, "").

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

-spec log(Level, Format, Args) -> Result when
    Level :: debug | info | warn,
    Format :: io:format(),
    Args :: [term()],
    Result :: ok.
log(Level, Format, Args) ->
    log(Level, Format, Args, _WithPrefix = true).

-spec log(Level, Format, Args, WithPrefix) -> Result when
    Level :: debug | info | warn,
    Format :: io:format(),
    Args :: [term()],
    WithPrefix :: boolean(),
    Result :: ok.
log(Level, Format0, Args, WithPrefix) ->
    Format = with_prefix(WithPrefix, Format0),
    rebar_log:log(Level, Format, Args),
    ct_pal_log0(Level, Format, Args),
    ok.

-spec with_prefix(WithPrefix, Format) -> Result when
    WithPrefix :: boolean(),
    Format :: io:format(),
    Result :: io:format().
with_prefix(false = _WithPrefix, Format) ->
    Format;
with_prefix(true = _WithPrefix, Format) ->
    "checkshell: " ++ Format.

-spec ct_pal_log0(Level, Format, Args) -> Result when
    Level :: debug | info | warn,
    Format :: io:format(),
    Args :: [term()],
    Result :: ok.
ct_pal_log0(debug = _Level, _Format, _Args) ->
    ok;
ct_pal_log0(info = _Level, _Format, _Args) ->
    ok;
ct_pal_log0(Level, Format, Args) ->
    ct_pal_log(Level, Format, Args).

-spec ct_pal_log(Level, Format, Args) -> Result when
    Level :: warn,
    Format :: io:format(),
    Args :: [term()],
    Result :: ok.
-ifdef(TEST).
ct_pal_log(Level, Format, Args) ->
    ct:pal(Level, 99, Format, Args, []).
-else.
ct_pal_log(_Level, _Format, _Args) ->
    ok.
% TEST
-endif.
