-module(mapz).

% API
-export([deep_find/2]).
-export([deep_get/2]).
-export([deep_get/3]).
-export([deep_put/3]).
-export([deep_update/3]).
-export([deep_update_with/3]).
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
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' if `Map' is not a map</li>
% <li>`{badpath,Path}' if `Path' is not a path</li>
% </ul>
-spec deep_find(path(), map()) -> {ok, term()} | error.
deep_find(Path, Map) ->
    check(Path, Map),
    search(Map, Path,
        fun(Value) -> {ok, Value} end,
        fun(_Existing, _Key) -> error end
    ).

% @doc Returns value `Value' associated with `Path' if `Map' contains `Path'.
%
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' if `Map' is not a map</li>
% <li>`{badpath,Path}' if `Path' is not a path</li>
% <li>`{badvalue,P}' if a term that is not a map exists as a intermediate key at
%     the path `P'</li>
% <li>`{badkey,Path}' if no value is associated with path `Path'</li>
% </ul>
-spec deep_get(path(), map()) -> term().
deep_get(Path, Map) ->
    check(Path, Map),
    search(Map, Path,
        fun(Value) -> Value end,
        fun
            ({ok, _Existing}, P) -> error({badvalue, P});
            (error, P)           -> error({badkey, P})
        end
    ).

% @doc Returns value `Value' associated with `Path' if `Map' contains `Path'. If
% no value is associated with `Path', `Default' is returned.
%
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' if `Map' is not a map</li>
% <li>`{badpath,Path}' if `Path' is not a path</li>
% <li>`{badvalue,P}' if a term that is not a map exists as a intermediate key at
%     the path `P'</li>
% </ul>
-spec deep_get(path(), map(), term()) -> term().
deep_get(Path, Map, Default) ->
    check(Path, Map),
    search(Map, Path,
        fun(Value) -> Value end,
        fun(_Existing, _P) -> Default end
    ).

% @doc Associates `Path' with value `Value' and inserts the association into map
% `Map2'. If path `Path' already exists in map `Map1', the old associated value
% is replaced by value `Value'. The function returns a new map `Map2' containing
% the new association and the old associations in `Map1'.
%
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' if `Map1' is not a map</li>
% <li>`{badpath,Path}' if `Path' is not a path</li>
% <li>`{badvalue,P}' if a term that is not a map exists as a intermediate key at
%     the path `P'</li>
% </ul>
-spec deep_put(path(), term(), map()) -> map().
deep_put(Path, Value, Map1)  ->
    check(Path, Map1),
    update(Map1, Path,
        fun(_Existing) -> Value end,
        fun
            (P, _Rest, {ok, _Existing}) ->
                error({badvalue, P});
            (_P, Rest, error) ->
                lists:foldr(fun(Key, Acc) -> #{Key => Acc} end, Value, Rest)
        end
    ).

% @doc If `Path' exists in `Map1', the old associated value is replaced by value
% `Value'. The function returns a new map `Map2' containing the new associated
% value.
%
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' if `Map1' is not a map</li>
% <li>`{badpath,Path}' if `Path' is not a path</li>
% <li>`{badvalue,P}' if a term that is not a map exists as a intermediate key at
%     the path `P'</li>
% <li>`{badkey,Path}' if no value is associated with path `Path'</li>
% </ul>
-spec deep_update(path(), term(), map()) -> map().
deep_update(Path, Value, Map1) ->
    check(Path, Map1),
    update(Map1, Path,
        fun(_Existing) -> Value end,
        fun
            (P, _Rest, {ok, _Existing}) -> error({badvalue, P});
            (P, _Rest, error)           -> error({badkey, P})
        end
    ).

% @doc Update a value in a `Map1' associated with `Path' by calling `Fun' on the
% old value to get a new value.
%
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' if `Map1' is not a map</li>
% <li>`{badpath,Path}' if `Path' is not a path</li>
% <li>`{badvalue,P}' if a term that is not a map exists as a intermediate key at
%     the path `P'</li>
% <li>`{badkey,Path}' if no value is associated with path `Path'</li>
% <li>`badarg' if `Fun' is not a function of arity 1</li>
% </ul>
-spec deep_update_with(path(), fun((term()) -> term()), map()) -> map().
deep_update_with(Path, Fun, Map1) ->
    check(Path, Map1),
    check_fun(Fun, 1),
    update(Map1, Path,
        fun(Value) -> Fun(Value) end,
        fun
            (P, _Rest, {ok, _Existing}) -> error({badvalue, P});
            (P, _Rest, error)           -> error({badkey, P})
        end
    ).

% @doc Removes the last existing key of `Path', and its associated value from
% `Map1' and returns a new map `Map2' without that key. Any deeper non-existing
% keys are ignored.
%
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' if `Map' is not a map</li>
% <li>`{badpath,Path}' if `Path' is not a path</li>
% </ul>
-spec deep_remove(path(), map()) -> map().
deep_remove(Path, Map) ->
    check(Path, Map),
    remove(Map, Path).

% @doc Merges a list of maps recursively into a single map. If a path exist in
% several maps, the value in the first nested map is superseded by the value in
% a following nested map.
%
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' exception if any of the maps is not a map</li>
% </ul>
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
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' exception if any of the maps is not a map</li>
% </ul>
% map.
-spec deep_merge(fun((Old::term(), New::term()) -> term()), map(), map() | [map()]) -> map().
deep_merge(_Fun, Target, []) when is_map(Target) ->
    Target;
deep_merge(Fun, Target, [From|Maps]) ->
    deep_merge(Fun, deep_merge(Fun, Target, From), Maps);
deep_merge(Fun, Target, Map) ->
    check_map(Target),
    check_map(Map),
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

% @doc Inverts `Map' by inserting each value as the key with its corresponding
% key as the value. If two keys have the same value, one of the keys will be
% overwritten by the other in an undefined order.
%
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' if `Map' is not a map</li>
% </ul>
-spec inverse(map()) -> map().
inverse(Map) ->
    maps:fold(fun(K, V, Acc) -> maps:put(V, K, Acc) end, #{}, Map).

%--- Internal Functions -------------------------------------------------------

check(Path, Map) ->
    check_path(Path),
    check_map(Map).

check_path(Path) when is_list(Path) -> ok;
check_path(Path)                    -> error({badpath, Path}).

check_map(Map) when is_map(Map) -> ok;
check_map(Map)                  -> error({badmap, Map}).

check_fun(Fun, Arity) when is_function(Fun, Arity) -> ok;
check_fun(_Fun, _Arity)                            -> exit(badarg).

search(Map, Path, Wrap, Default) -> search(Map, Path, Wrap, Default, []).

search(Element, [], Wrap, _Default, _Acc) ->
    Wrap(Element);
search(Map, [Key|Path], Wrap, Default, Acc) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> search(Value, Path, Wrap, Default, [Key|Acc]);
        error       -> Default(error, lists:reverse([Key|Acc]))
    end;
search(Value, [_Key|_Path], _Wrap, Default, Acc) ->
    Default({ok, Value}, lists:reverse(Acc)).

update(Map, Path, Wrap, Default) -> update(Map, Path, Wrap, Default, []).

update(Map, [Key|Path], Wrap, Default, Acc) when is_map(Map) ->
    Hist = [Key|Acc],
    Value = case maps:find(Key, Map) of
        {ok, Existing} when is_map(Existing) ->
            update(Existing, Path, Wrap, Default, Hist);
        {ok, Existing} ->
            case Path of
                [] -> Wrap(Existing);
                _  -> Default(lists:reverse(Hist), Path, {ok, Existing})
            end;
        error ->
            Default(lists:reverse(Hist), Path, error)
    end,
    maps:put(Key, Value, Map);
update(Map, [], Wrap, _Default, _Acc) when is_map(Map) ->
    Wrap(error).

remove(Map, []) ->
    Map;
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
