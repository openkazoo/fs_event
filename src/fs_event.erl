-module(fs_event).
-behaviour(gen_server).
-include_lib("fs_event/include/logger.hrl").

%% API exports
-export([start_link/1, add_handler/3, delete_handler/3, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    event_manager :: pid(),
    port :: port(),
    handler :: module()
}).

-type state() :: #state{}.
-type handler_args() :: term().

%% API

-spec start_link(string()) -> gen_server:start_ret().
start_link(Path) -> 
    gen_server:start_link(?MODULE, [Path], []).

-spec add_handler(pid(), module(), handler_args()) -> ok.
add_handler(Pid, Handler, Args) -> 
    gen_server:call(Pid, {add_handler, Handler, Args}).

-spec delete_handler(pid(), module(), handler_args()) -> ok.
delete_handler(Pid, Handler, Args) -> 
    gen_server:call(Pid, {delete_handler, Handler, Args}).

-spec stop(pid() | state()) -> ok.
stop(Pid) when is_pid(Pid) -> 
    gen_server:call(Pid, {stop});
stop(#state{port=Port, event_manager=EventManager, handler=HandlerModule}) ->
    gen_event:stop(EventManager),
    HandlerModule:stop(Port).

%% gen_server callbacks

-spec init([string()]) -> {ok, state()}.
init([Path]) ->
    {ok, EventManager} = gen_event:start_link(),
    HandlerModule = choose_handler(fs_event_cfg:force_backend(), [inotify, naive]),
    ?INFO("Selected backend: ~p", [HandlerModule]),
    Handler = HandlerModule:start(Path),
    {ok, #state{event_manager=EventManager, port=Handler, handler=HandlerModule}}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> 
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({_Port, {data, {eol, Line}}}, State=#state{event_manager=EventManager, handler=HandlerModule}) ->
    {File, Events} = HandlerModule:parse(Line),
    _ = [ gen_event:notify(EventManager, {Event, File}) || Event <- Events ],
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec handle_call(term(), {pid(), term()}, state()) -> 
    {reply, ok, state()} | {stop, normal, ok, state()}.
handle_call({add_handler, Handler, Args}, _From, State=#state{event_manager=EventManager}) ->
    gen_event:add_handler(EventManager, Handler, Args),
    {reply, ok, State};
handle_call({delete_handler, Handler, Args}, _From, State=#state{event_manager=EventManager}) ->
    gen_event:delete_handler(EventManager, Handler, Args),
    {reply, ok, State};
handle_call({stop}, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) -> 
    {reply, ok, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    stop(State),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%% Internal functions

-spec choose_handler(module() | false, [module()]) -> module().
choose_handler(false, []) ->
    erlang:error(no_fs_module_available);
choose_handler(false, [Module|Modules]) ->
    case Module:check() of
        true -> Module;
        false -> choose_handler(false, Modules)
    end;
choose_handler(Handler, _) -> 
    Handler.
