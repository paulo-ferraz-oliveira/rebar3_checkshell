% @private
-module(rebar3_checkshell_utils).

-export([cmd/2]).

-spec cmd(Cmd, Args) -> Result when
    Cmd :: string(),
    Args :: [string()],
    Result :: {ExitStatus :: non_neg_integer(), Data :: string()}.
cmd(Cmd, Args0) ->
    Args = [" " ++ Arg || Arg <- Args0],
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
