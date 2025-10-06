-module(raw).
-export([enter_raw/0, reader_loop/1, start_reader/0, read_char/0]).

enter_raw() ->
    shell:start_interactive({noshell, raw}).

reader_loop(Pid) ->
    case io:get_chars("", 1) of
        eof -> Pid ! {self(), eof};
        Ch  -> Pid ! {self(), Ch}
    end,
    reader_loop(Pid).

start_reader() ->
    spawn(fun() -> reader_loop(self()) end).

read_char() ->
    case io:get_chars("", 1) of
        eof -> eof;
        Char -> Char
    end.
