% @private
-module(rebar3_checkshell_utils).

-export([cmd/2]).
-export([log/3]).

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
    rebar_log:log(Level, Format, Args),
    ct_pal_log0(Level, Format, Args),
    ok.

-spec ct_pal_log(Level, Format, Args) -> Result when
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

-ifdef(TEST).
ct_pal_log(Level, Format, Args) ->
    ct:pal(Level, 99, Format, Args, []).
-else.
ct_pal_log(_Level, _Format, _Args) ->
    ok.
% TEST
-endif.
