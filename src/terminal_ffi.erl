-module(terminal_ffi).

-export([enter_raw/0, window_size/0]).

enter_raw() ->
    shell:start_interactive({noshell, raw}).

window_size() ->
    case io:columns() of
        {ok, Cols} ->
            case io:rows() of
                {ok, Rows} ->
                    {Cols, Rows};
                _ ->
                    error
            end;
        _ ->
            error
    end.
