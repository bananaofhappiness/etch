-module(input_ffi).

-behaviour(gen_server).

-export([start_link/0, poll/1, push/1, read/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {queue, waiter = none, input_loop_pid}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    gen_event:add_handler(erl_signal_server, signal_handler, self()),
    InputLoopPid = spawn_link(input, input_loop, []),
    {ok, #state{queue = queue:new(), input_loop_pid = InputLoopPid}}.

poll(TimeoutMs) when is_integer(TimeoutMs), TimeoutMs >= 0 ->
    case whereis(?MODULE) of
        undefined ->
            start_link(),
            gen_server:call(?MODULE, {poll, TimeoutMs}, TimeoutMs + 50);
        _Pid ->
            gen_server:call(?MODULE, {poll, TimeoutMs}, TimeoutMs + 50)
    end.

read() ->
    case whereis(?MODULE) of
        undefined ->
            start_link(),
            gen_server:call(?MODULE, read, infinity);
        _Pid ->
            gen_server:call(?MODULE, read, infinity)
    end.

restart_input_loop() ->
    gen_server:call(?MODULE, restart_input_loop, infinity).

push(Event) ->
    gen_server:cast(?MODULE, {push, Event}).

handle_call({poll, _Timeout}, _From, #state{queue = Q} = S) ->
    case queue:out(Q) of
        {empty, _} ->
            {reply, none, S};
        {{value, Ev}, Q2} ->
            {reply, {some, Ev}, S#state{queue = Q2}}
    end;
handle_call(read, From, #state{queue = Q} = S) ->
    case queue:out(Q) of
        {empty, _} ->
            {noreply, S#state{waiter = From}};
        {{value, Ev}, Q2} ->
            {reply, {some, Ev}, S#state{queue = Q2}}
    end;
handle_call(restart_input_loop, _From, #state{input_loop_pid = Pid} = S) ->
    unlink(Pid),
    exit(Pid, kill),
    {reply, ok, S#state{input_loop_pid = spawn_link(input, input_loop, [])}};
handle_call(_Other, _From, State) ->
    {reply, ok, State}.

handle_cast({push, Ev}, #state{queue = Q, waiter = none} = S) ->
    {noreply, S#state{queue = queue:in(Ev, Q)}};
handle_cast({push, Ev}, #state{waiter = Waiter} = S) ->
    gen_server:reply(Waiter, {some, Ev}),
    {noreply, S#state{waiter = none}};
handle_cast(_Other, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.
