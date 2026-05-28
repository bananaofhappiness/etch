-module(signal_handler).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_info/2, code_change/3, terminate/2,
         handle_call/2]).

init(Subject) ->
    ok = os:set_signal(sigwinch, handle),
    {ok, Subject}.

handle_event(sigwinch, State) ->
    case terminal_ffi:window_size() of
        {ok, {Cols, Rows}} ->
            event_ffi:push({ok, {resize, Cols, Rows}});
        _ ->
            event_ffi:push({error, could_not_get_window_size})
    end,
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
