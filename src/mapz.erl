-module(mapz).

% API
-export([deep_find/2]).
-export([deep_get/2]).
-export([deep_get/3]).
-export([deep_put/3]).
-export([deep_remove/2]).
-export([deep_merge/1]).
-export([deep_merge/2]).
-export([deep_merge/3]).

%--- API ----------------------------------------------------------------------

deep_find(Path, Map) ->
    search(Map, Path,
        fun(Value) -> {ok, Value} end,
        fun(_Key) -> error end
    ).

deep_get(Path, Map) ->
    search(Map, Path,
        fun(Value) -> Value end,
        fun(Key) -> error({badkey, Key}) end
    ).

deep_get(Path, Map, Default) ->
    search(Map, Path,
        fun(Value) -> Value end,
        fun(_Key) -> Default end
    ).

deep_put(Path, Value, Map) -> update(Map, Path, {set, Value}).

deep_remove(Path, Map) -> update(Map, Path, delete).

deep_merge([Map|Maps]) ->
    deep_merge(fun (_, V) -> V end, Map, Maps).

deep_merge(First, Second) when is_map(First), is_map(Second) ->
    deep_merge([First, Second]).

deep_merge(_Fun, Target, []) ->
    Target;
deep_merge(Fun, Target, [From|Maps]) ->
    deep_merge(Fun, deep_merge(Fun, Target, From), Maps);
deep_merge(Fun, Target, Map) when is_map(Map) ->
    maps:fold(
        fun(K, V, T) ->
            case maps:find(K, T) of
                {ok, Value} when is_map(Value), is_map(V) ->
                    maps:put(K, deep_merge(Fun, Value, [V]), T);
                {ok, Value} ->
                    maps:put(K, Fun(Value, V), T);
                error ->
                    maps:put(K, V, T)
            end
        end,
        Target,
        Map
    ).

%--- Internal Functions -------------------------------------------------------

search(Map, [], Wrap, _Default) ->
    Wrap(Map);
search(Map, [Key|Path], Wrap, Default) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> search(Value, Path, Wrap, Default);
        error       -> Default(Key)
    end;
search(_Map, [Key|_Path], _Wrap, Default) ->
    Default(Key).

update(Map, [Key], Act) when is_map(Map) ->
    case {maps:is_key(Key, Map), Act} of
        {true, delete}        -> maps:remove(Key, Map);
        {true, {set, Value}}  -> maps:update(Key, Value, Map);
        {false, delete}       -> error({badkey, Key});
        {false, {set, Value}} -> maps:put(Key, Value, Map)
    end;
update(Map, [Key|Path], Act) when is_map(Map) ->
    case {maps:find(Key, Map), Act} of
        {{ok, Value}, _} when is_map(Value) ->
            maps:update(Key, update(Value, Path, Act), Map);
        {{ok, Value}, _} ->
            error({badvalue, Value});
        {error, delete} ->
            error({badkey, Key});
        {error, {set, _Value}} ->
            maps:put(Key, update(#{}, Path, Act), Map)
    end;
update(_Map, [], {set, Value}) when is_map(_Map) ->
    Value.
