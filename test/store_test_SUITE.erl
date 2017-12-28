-module(store_test_SUITE).

-export([all/0]).
-export([ 
            store/1
        ]).

-type config() :: proplists:proplist().

-spec all() -> [atom()].
all() -> store_test_SUITE:all(?MODULE).

-spec store(config()) -> {comment, []}.
store(_Config) ->
  ct:comment("Store an element"),
  {ok, Pid} = store:start_link(a), 
  try 
    store:store(Pid, 1, 1)
  catch
    _:Error -> ct:fail("Unexpected result: ~p", [Error])
  end,
  {comment, ""}.
