-module(store).

-type store() :: pid().
-type key() :: term().
-type value() :: term().

-behaviour(gen_server).

-export([start/0, stop/1, start_link/1, init/1, store/3, handle_call/3, fetch/2, delete/2, all_keys/1, lookup/2, update/3]).

-spec start() -> {ok, _} | {error, term()}.
start() ->
  ok.

-spec stop() -> ok.
stop() ->
  application:stop(store).

-spec start_link(atom()) -> gen:start_ret().
start_link(Name) ->
   gen_server:start_link({local, Name}, ?MODULE, [], []).

init([]) -> {ok, []}.

terminate(normal) ->
   ok.

-spec store(store(), key(), value()) -> {ok}.
store(Pid, Key, Value) -> gen_server:call(Pid, {store, Key, Value}).	

-spec fetch(store(), key()) -> {ok, value()}.
fetch(Pid, Key) -> gen_server:call(Pid, {fetch, Key}).	

-spec delete(store(), key()) -> ok.
delete(Pid, Key) -> gen_server:call(Pid, {delete, Key}).

-spec all_keys(store()) -> [{key(), value()}].
all_keys(Pid) -> gen_server:call(Pid, {all_keys}).

-spec lookup(store(), value()) -> [key()].
lookup(Pid, Value) -> gen_server:call(Pid, {lookup, Value}).

-spec update(store(), key(), value()) -> {ok, value()} | {error, not_found}.
update(Pid, Key, Value) -> gen_server:call(Pid, {update, Key, Value}).

handle_call({store, Key, Value}, _From, State) ->
   put (Key , Value),
   {reply, ok, State };

handle_call({delete, Key}, _From, State) ->
   erase(Key),
   {reply, ok, State };

handle_call({fetch, Key}, _From, State) ->
   Element = get( Key ),
   case Element of
      undefined -> {reply, {error, not_found}, State };
      Value -> {reply, {ok, Value}, State }
   end;

handle_call({all_keys}, _From, State) ->
   List = get(),
   {reply, List, State };

handle_call({lookup, Value}, _From, State) ->
   List = get_keys(Value),
   {reply, List, State };

handle_call({update, Key, Value}, _From, State) ->
   case get(Key) of
      undefined -> {reply, {error, not_found}, State };
      OldValue ->
         put(Key, Value),
         {reply, {ok, OldValue}, State }
   end.



code_change(_OldVsn, State, _Extra) ->
{ok, State}.

%% @private
-spec stop([]) -> ok.
stop([]) -> ok.
