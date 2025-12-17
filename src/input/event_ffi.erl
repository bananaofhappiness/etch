-module(event_ffi).

-behaviour(gen_server).

-export([start_link/0, poll/1, push/1, read/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

poll(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs >= 0 ->
    case whereis(?MODULE) of
        undefined ->
            logger:error("Event server not started. Did you forget to call event.init_event_se"
                         "rver()?"),
            exit(event_loop_not_running);
        _Pid ->
            gen_server:call(?MODULE, {poll, TimeoutMs}, TimeoutMs + 50)
    end.

read() ->
    case whereis(?MODULE) of
        undefined ->
            logger:error("Event server not started. Did you forget to call event.init_event_se"
                         "rver()?"),
            exit(event_loop_not_running);
        _Pid ->
            gen_server:call(?MODULE, read, infinity)
    end.

push(Event) ->
    gen_server:cast(?MODULE, {push, Event}).

init([]) ->
    gen_event:add_handler(erl_signal_server, signal_handler, self()),
    {ok, {queue:new(), none}}.

handle_call({poll, _Timeout}, _From, {Q, Waiter}) ->
    case queue:out(Q) of
        {empty, _} ->
            {reply, none, {Q, Waiter}};
        {{value, Ev}, Q2} ->
            {reply, {some, Ev}, {Q2, Waiter}}
    end;
handle_call(read, From, {Q, none}) ->
    case queue:out(Q) of
        {empty, _} ->
            {noreply, {Q, From}};
        {{value, Ev}, Q2} ->
            {reply, {some, Ev}, {Q2, none}}
    end;
handle_call(read, From, {Q, Waiter}) ->
    gen_server:reply(From, none),
    {reply, ok, {Q, Waiter}};
handle_call(_Other, _From, State) ->
    {reply, ok, State}.

handle_cast({push, Ev}, {Q, none}) ->
    {noreply, {queue:in(Ev, Q), none}};
handle_cast({push, Ev}, {Q, Waiter}) ->
    gen_server:reply(Waiter, {some, Ev}),
    {noreply, {Q, none}};
handle_cast(_Other, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.
