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
-export([inverse/1]).

-type path() :: [term()].
% A list of keys that are used to iterate deeper into a map of maps.

%--- API ----------------------------------------------------------------------

% @doc Returns a tuple `{ok,Value}', where Value is the value associated with
% `Path', or `error' if no value is associated with `Path' in `Map'.
%
% The call fails with a `{badmap,Map}' exception if `Map' is not a map, or with
% a `{badpath,Path}' exception if `Path' is not a path.
-spec deep_find(path(), map()) -> {ok, term()} | error.
deep_find(Path, Map) when is_list(Path), is_map(Map) ->
    search(Map, Path,
        fun(Value) -> {ok, Value} end,
        fun(_Key) -> error end
    );
deep_find(Path, Map) when is_map(Map) ->
    error({badpath, Path});
deep_find(Path, Map) when is_list(Path) ->
    error({badmap, Map}).

% @doc Returns value `Value' associated with `Path' if `Map' contains `Path'.
%
% The call fails with a `{badmap,Map}' exception if `Map' is not a map, or with
% a `{badpath,Path}' exception if `Path' is not a path.
-spec deep_get(path(), map()) -> term().
deep_get(Path, Map) when is_list(Path), is_map(Map) ->
    search(Map, Path,
        fun(Value) -> Value end,
        fun(Key) -> error({badkey, Key}) end
    );
deep_get(Path, Map) when is_map(Map) ->
    error({badpath, Path});
deep_get(Path, Map) when is_list(Path) ->
    error({badmap, Map}).

% @doc Returns value `Value' associated with `Path' if `Map' contains `Path'. If
% no value is associated with `Path', `Default' is returned.
%
% The call fails with a `{badmap,Map}' exception if `Map' is not a map, or with
% a `{badpath,Path}' exception if `Path' is not a path.
-spec deep_get(path(), map(), term()) -> term().
deep_get(Path, Map, Default) when is_list(Path), is_map(Map) ->
    search(Map, Path,
        fun(Value) -> Value end,
        fun(_Key) -> Default end
    );
deep_get(Path, Map, _Default) when is_map(Map) ->
    error({badpath, Path});
deep_get(Path, Map, _Default) when is_list(Path) ->
    error({badmap, Map}).

% @doc Associates `Path' with value `Value' and inserts the association into map
% `Map2'. If path `Path' already exists in map `Map1', the old associated value
% is replaced by value `Value'. The function returns a new map `Map2' containing
% the new association and the old associations in `Map1'.
%
% The call fails with a `{badmap,Map}' exception if `Map' is not a map, or with
% a `{badpath,Path}' exception if `Path' is not a path. `{badvalue,Term}' is
% raised if a term that is not a map exists as a intermediate key in the path.
-spec deep_put(path(), term(), map()) -> map().
deep_put(Path, Value, Map) when is_list(Path), is_map(Map) ->
    update(Map, Path, Value);
deep_put(Path, _Value, Map) when is_map(Map) ->
    error({badpath, Path});
deep_put(Path, _Value, Map) when is_list(Path) ->
    error({badmap, Map}).

% @doc Removes the last existing key of `Path', and its associated value from
% `Map1' and returns a new map `Map2' without that key. Any deeper non-existing
% keys are ignored.
%
% The call fails with a `{badmap,Map}' exception if `Map' is not a map, or with
% a `{badpath,Path}' exception if `Path' is not a path.
-spec deep_remove(path(), map()) -> map().
deep_remove([], Map) when is_map(Map) ->
    Map;
deep_remove(Path, Map) when is_list(Path), is_map(Map) ->
    remove(Map, Path);
deep_remove(Path, Map) when is_map(Map) ->
    error({badpath, Path});
deep_remove(Path, Map) when is_list(Path) ->
    error({badmap, Map}).

% @doc Merges a list of maps recursively into a single map. If a path exist in
% several maps, the value in the first nested map is superseded by the value in
% a following nested map.
%
% The call fails with a `{badmap,Map}' exception if `Map1' or `Map2' is not a
% map.
%
% @equiv deep_merge(fun (_, V) -> V end, #{}, Maps)
-spec deep_merge([map()]) -> map().
deep_merge([Map|Maps]) ->
    deep_merge(fun (_, V) -> V end, Map, Maps).

% @equiv deep_merge([Map1, Map2])
-spec deep_merge(map(), map()) -> map().
deep_merge(Map1, Map2) ->
    deep_merge([Map1, Map2]).

% @doc Merges a list of maps `Maps' recursively into a single map `Target'. If a
% path exist in several maps, the function `Fun' is called with the previous and
% the conflicting value to resolve the conflict. The return value from the
% function is put into the resulting map.
%
% The call fails with a `{badmap,Map}' exception if any of the maps is not a
% map.
-spec deep_merge(fun((Old::term(), New::term()) -> term()), map(), map() | [map()]) -> map().
deep_merge(_Fun, Target, []) when is_map(Target) ->
    Target;
deep_merge(Fun, Target, [From|Maps]) ->
    deep_merge(Fun, deep_merge(Fun, Target, From), Maps);
deep_merge(Fun, Target, Map) when is_map(Target), is_map(Map) ->
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
    );
deep_merge(_Fun, Target, Map) when is_map(Map) ->
    error({badmap, Target});
deep_merge(_Fun, Target, Map) when is_map(Target) ->
    error({badmap, Map}).

% @doc Inverts `Map' by inserting each value as the key with its corresponding
% key as the value. If two keys have the same value, one of the keys will be
% overwritten by the other in an undefined order.
%
% The call fails with a `{badmap,Map}' exception if `Map' is not a map.
-spec inverse(map()) -> map().
inverse(Map) ->
    maps:fold(fun(K, V, Acc) -> maps:put(V, K, Acc) end, #{}, Map).

%--- Internal Functions -------------------------------------------------------

search(Element, [], Wrap, _Default) ->
    Wrap(Element);
search(Map, [Key|Path], Wrap, Default) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> search(Value, Path, Wrap, Default);
        error       -> Default(Key)
    end;
search(_Map, [Key|_Path], _Wrap, Default) ->
    Default(Key).

update(Map, [Key], Value) when is_map(Map) ->
    maps:put(Key, Value, Map);
update(Map, [Key|Path], Value) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Existing} when is_map(Existing) ->
            maps:update(Key, update(Existing, Path, Value), Map);
        {ok, Existing} ->
            error({badvalue, Existing});
        error ->
            maps:put(Key, update(#{}, Path, Value), Map)
    end;
update(Map, [], Value) when is_map(Map) ->
    Value.

remove(Map, [First]) ->
    maps:remove(First, Map);
remove(Map, [First, Second|Path]) when is_map(Map) ->
    case maps:find(First, Map) of
        {ok, Sub} when is_map(Sub) ->
            case maps:find(Second, Sub) of
                {ok, _SubSub} ->
                    maps:update(First, remove(Sub, [Second|Path]), Map);
                error ->
                    maps:remove(First, Map)
            end;
        {ok, _Value} ->
            maps:remove(First, Map);
        error ->
            Map
    end.
