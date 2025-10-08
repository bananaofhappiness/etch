-module(raw).
-export([enter_raw/0, loop/1]).

enter_raw() ->
    shell:start_interactive({noshell, raw}).
