-module(signal_handler).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_info/2, code_change/3, terminate/2,
         handle_call/2]).

init(Subject) ->
    ok = os:set_signal(sigwinch, handle),
    {ok, Subject}.

handle_event(sigwinch, State) ->
    {Cols, Rows} = terminal_ffi:window_size(),
    event_ffi:push({resize, Cols, Rows}),
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
