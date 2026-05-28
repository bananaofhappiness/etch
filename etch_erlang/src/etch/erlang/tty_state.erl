-module(tty_state).

-export([init/0, set_raw/1, is_raw_mode/0, save_input_loop_pid/1, get_input_loop_pid/0]).

init() ->
    case ets:whereis(tty_state) of
        undefined ->
            ets:new(tty_state, [named_table, public, set]),
            ets:insert(tty_state, {raw_mode, false});
        _ ->
            ok
    end.

set_raw(IsRaw) ->
    case ets:whereis(tty_state) of
        undefined ->
            ets:new(tty_state, [named_table, public, set]);
        _ ->
            ok
    end,
    ets:insert(tty_state, {raw_mode, IsRaw}).

is_raw_mode() ->
    case ets:lookup(tty_state, raw_mode) of
        [{raw_mode, true}] ->
            true;
        _ ->
            false
    end.

save_input_loop_pid(Pid) ->
    case ets:lookup(tty_state, input_loop_pid) of
        [{input_loop_pid, _}] ->
            ets:insert(tty_state, {input_loop_pid, Pid});
        _ ->
            ets:insert(tty_state, {input_loop_pid, Pid})
    end.

get_input_loop_pid() ->
    case ets:lookup(tty_state, input_loop_pid) of
        [{input_loop_pid, Pid}] ->
            {ok, Pid};
        _ ->
            {error, nil}
    end.
