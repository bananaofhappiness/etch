-module(terminal_ffi).

-export([enter_raw/0, window_size/0, enable_os_signals/1]).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_info/2, handle_call/2, terminate/2,
         code_change/3]).

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

enable_os_signals(Subject) ->
    ok = os:set_signal(sigwinch, handle),
    ok = os:set_signal(sigterm, handle),
    gen_event:add_handler(erl_signal_server, ?MODULE, Subject).

%% === gen_event callbacks ===
init(Subject) ->
    {ok, Subject}.

handle_event(sigwinch, Subject) ->
    {Cols, Rows} = window_size(),
    gleam@erlang@process:send(Subject, {resize, Cols, Rows}),
    {ok, Subject};
handle_event(sigterm, Subject) ->
    gleam@erlang@process:send(Subject, {exit}),
    {ok, Subject};
handle_event(_, S) ->
    {ok, S}.

handle_info(_, S) ->
    {ok, S}.

handle_call(_, S) ->
    {ok, ok, S}.

terminate(_, _) ->
    ok.

code_change(_, S, _) ->
    {ok, S}.
