-module(terminal_ffi).

-export([enter_raw/0, exit_raw/0, window_size/0]).

enter_raw() ->
    try
        shell:start_interactive({noshell, raw}),
        {ok, nil}
    catch
        _:_ ->
            {error, failed_to_enter_raw_mode}
    end.

exit_raw() ->
    try
        shell:start_interactive(noshell),
        {ok, nil}
    catch
        _:_ ->
            {error, failed_to_exit_raw_mode}
    end.

window_size() ->
    case io:columns() of
        {ok, Cols} ->
            case io:rows() of
                {ok, Rows} ->
                    {ok, {Cols, Rows}};
                _ ->
                    {error, could_not_get_window_size}
            end;
        _ ->
            {error, could_not_get_window_size}
    end.
