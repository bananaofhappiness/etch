-module(raw).
-export([enter_raw/0]).

enter_raw() ->
    shell:start_interactive({noshell, raw}).
