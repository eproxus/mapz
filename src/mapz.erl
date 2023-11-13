-module(mapz).

% API
-export([deep_find/2]).
-ignore_xref({deep_find, 2}).
-export([deep_search/2]).
-ignore_xref({deep_search, 2}).
-export([deep_get/2]).
-ignore_xref({deep_get, 2}).
-export([deep_get/3]).
-ignore_xref({deep_get, 3}).
-export([deep_put/3]).
-ignore_xref({deep_put, 3}).
-export([deep_update/3]).
-ignore_xref({deep_update, 3}).
-export([deep_update_with/3]).
-ignore_xref({deep_update_with, 3}).
-export([deep_update_with/4]).
-ignore_xref({deep_update_with, 4}).
-export([deep_remove/2]).
-ignore_xref({deep_remove, 2}).
-export([deep_merge/1]).
-ignore_xref({deep_merge, 1}).
-export([deep_merge/2]).
-ignore_xref({deep_merge, 2}).
-export([deep_merge/3]).
-ignore_xref({deep_merge, 3}).
-export([deep_merge_with/2]).
-ignore_xref({deep_merge_with, 2}).
-export([deep_merge_with/3]).
-ignore_xref({deep_merge_with, 3}).
-export([deep_iterator/1]).
-ignore_xref({deep_iterator, 1}).
-export([deep_next/1]).
-ignore_xref({deep_next, 1}).
-export([deep_intersect/2]).
-ignore_xref({deep_intersect, 2}).
-export([deep_intersect_with/3]).
-ignore_xref({deep_intersect_with, 3}).
-export([inverse/1]).
-ignore_xref({inverse, 1}).
-export([inverse/2]).
-ignore_xref({inverse, 2}).
-export([format_error/2]).
-ignore_xref({format_error, 2}).

% We must inline this so that the stack trace points to the correct function.
-compile({inline, [error_info/2]}).

%--- Types ---------------------------------------------------------------------

-export_type([path/0]).
-export_type([iterator/0]).
-export_type([combiner/0]).

-type path() :: [term()].
% A list of keys that are used to iterate deeper into a map of maps.

-opaque iterator() ::
    {?MODULE, none | maps:iterator(_, _) | {_, _, maps:iterator(_, _)}, path(),
        [maps:iterator(_, _)]}.
% An iterator representing the associations in a map with keys of type Key and
% values of type Value.
%
% Created using {@link deep_iterator/1}.
%
% Consumed by {@link deep_next/1}.

-type combiner() :: fun(
    (Path :: path(), Old :: term(), New :: term()) -> term()
).
% A combiner function that takes a path, and its two conflicting old values and
% returns a new value.

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
    search(
        Map,
        Path,
        fun(Value) -> {ok, Value} end,
        fun(_Existing, _Key) -> error end
    ).

% @doc Returns a tuple `{ok,Value}' where `Value' is the value associated
% with `Path', or `{error, PartialPath, Value}' if no value is associated with
% `Path' in `Map', where `PartialPath' represents the path to the last found
% element in `Map' and `Value' is the value found at that path.
%
% When no key in `Path' exists in `Map', `{error, [], Map}' is returned.
%
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' if `Map' is not a map</li>
% <li>`{badpath,Path}' if `Path' is not a path</li>
% </ul>
deep_search(Path, Map) ->
    check(Path, Map),
    search(
        Map,
        Path,
        fun(Value) -> {ok, Value} end,
        fun
            ({ok, Value}, LastPath) ->
                {error, LastPath, Value};
            (error, LastPath) ->
                {FoundPath, _} = lists:split(length(LastPath) - 1, LastPath),
                {error, FoundPath, deep_get(FoundPath, Map)}
        end
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
    search(
        Map,
        Path,
        fun(Value) -> Value end,
        fun
            ({ok, _Existing}, P) -> error({badvalue, P});
            (error, P) -> error({badkey, P})
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
    search(
        Map,
        Path,
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
deep_put(Path, Value, Map1) ->
    check(Path, Map1),
    update(Map1, Path, fun(_Existing) -> Value end, fun(P, Rest, V) ->
        badvalue_and_create(P, Rest, V, Value)
    end).

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
    update(Map1, Path, fun(_Existing) -> Value end, fun badvalue_and_badkey/3).

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
    deep_update_with_1(Path, Fun, Map1, fun badvalue_and_badkey/3).

% @doc Update a value in a `Map1' associated with `Path' by calling `Fun' on the
% old value to get a new value.  If `Path' is not present in `Map1' then `Init'
% will be associated with `Path'.
%
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' if `Map1' is not a map</li>
% <li>`{badpath,Path}' if `Path' is not a path</li>
% <li>`{badvalue,P}' if a term that is not a map exists as a intermediate key at
%     the path `P'</li>
% <li>`badarg' if `Fun' is not a function of arity 1</li>
% </ul>
-spec deep_update_with(path(), fun((term()) -> term()), any(), map()) -> map().
deep_update_with(Path, Fun, Init, Map1) ->
    deep_update_with_1(Path, Fun, Map1, fun(P, Rest, Value) ->
        badvalue_and_create(P, Rest, Value, Init)
    end).

deep_update_with_1(Path, Fun, Map1, Default) ->
    check(Path, Map1),
    check_fun(Fun, 1),
    update(
        Map1,
        Path,
        fun(Value) -> Fun(Value) end,
        Default
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
deep_merge(Maps) when is_list(Maps) ->
    deep_merge_with(fun(_Path, _V1, V2) -> V2 end, Maps).

% @equiv deep_merge([Map1, Map2])
-spec deep_merge(map(), map()) -> map().
deep_merge(Map1, Map2) when is_map(Map1), is_map(Map2) ->
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
%
% @deprecated Use {@link deep_merge_with/3} instead
-spec deep_merge(
    fun((Old :: term(), New :: term()) -> term()), map(), map() | [map()]
) -> map().
deep_merge(Fun, Target, Maps) ->
    deep_merge_with(fun(_P, V1, V2) -> Fun(V1, V2) end, Target, Maps).

% @doc Merges a list of maps `Maps' recursively into a single map. If a path
%  exist in several maps, the function `Fun' is called with the path, the
%  previous and the conflicting value to resolve the conflict. The return value
%  from the function is put into the resulting map.
%
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' exception if any of the maps is not a map</li>
% <li>`badarg' if `Fun' is not a function of arity 3</li>
% </ul>
% map.
-spec deep_merge_with(Fun :: combiner(), Maps :: [map()]) -> map().
deep_merge_with(Fun, [Target | Maps]) ->
    deep_merge_with1(Fun, Target, Maps, []).

% @doc Merges a list of maps `Maps' recursively into a single map. If a path
%  exist in several maps, the function `Fun' is called with the path, the
%  previous and the conflicting value to resolve the conflict. The return value
%  from the function is put into the resulting map.
%
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' exception if any of the maps is not a map</li>
% </ul>
% map.
-spec deep_merge_with(Fun :: combiner(), Map1 :: map(), Map2 :: map()) -> map().
deep_merge_with(Fun, Map1, Map2) when is_map(Map1), is_map(Map2) ->
    deep_merge_with(Fun, [Map1, Map2]).

deep_merge_with1(_Fun, Target, [], _Path) when is_map(Target) ->
    Target;
deep_merge_with1(Fun, Target, [From | Maps], Path) ->
    deep_merge_with1(
        Fun, deep_merge_with1(Fun, Target, From, Path), Maps, Path
    );
deep_merge_with1(Fun, Target, Map, Path) ->
    check_map(Target),
    check_map(Map),
    check_fun(Fun, 3),
    maps:fold(
        fun(K, V, T) ->
            case maps:find(K, T) of
                {ok, Value} when is_map(Value), is_map(V) ->
                    maps:put(
                        K, deep_merge_with1(Fun, Value, [V], Path ++ [K]), T
                    );
                {ok, Value} ->
                    maps:put(K, Fun(Path ++ [K], Value, V), T);
                error ->
                    maps:put(K, V, T)
            end
        end,
        Target,
        Map
    ).

% @doc Returns a map iterator Iterator that can be used by {@link deep_next/1}
%  to recursively traverse the path-value associations in a deep map structure.
%
% The call fails with a `{badmap,Map}' exception if Map is not a map.
-spec deep_iterator(map()) -> iterator().
deep_iterator(Map) when is_map(Map) ->
    {?MODULE, maps:next(maps:iterator(Map)), [], []};
deep_iterator(Map) ->
    error_info({badmap, Map}, [Map]).

% @doc Returns the next path-value association in Iterator and a new iterator
%  for the remaining associations in the iterator.
%
% If the value is another map the iterator will first return the map as a value
% with its path. Only on the next call the inner value with its path is
% returned. That is, first `{Path, map(), iterator()}' and then
% `{InnerPath, Value, iterator()}'.
%
% If there are no more associations in the iterator, `none' is returned.
-spec deep_next(iterator()) -> {path(), term(), iterator()} | none.
deep_next({?MODULE, I, Trail, Stack}) ->
    case {I, Stack} of
        {none, []} ->
            none;
        {none, [Prev | Rest]} ->
            deep_next({?MODULE, maps:next(Prev), lists:droplast(Trail), Rest});
        {{K, V, I2}, Stack} when is_map(V) ->
            Path = Trail ++ [K],
            Next = maps:next(maps:iterator(V)),
            {Path, V, {?MODULE, Next, Path, [I2 | Stack]}};
        {{K, V, I2}, Stack} ->
            Path = Trail ++ [K],
            {Path, V, {?MODULE, I2, Trail, Stack}}
    end;
deep_next(Iter) ->
    error_info(badarg, [Iter]).

% @doc Intersects two maps into a single map `Map3'. If a path exists in both
%  maps, the value in `Map1' is superseded by the value in `Map2'.
%
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' exception if any of the maps is not a map</li>
% </ul>
%
% @equiv deep_intersect_with(fun(_, _, V) -> V end, Map1, Map2)
-spec deep_intersect(Map1 :: map(), Map2 :: map()) -> Map3 :: map().
deep_intersect(A, B) ->
    deep_intersect_with(fun(_Path, _V1, V2) -> V2 end, A, B).

% @doc Intersects two maps into a single map `Map3'.
%
% If a path exists in both maps, the value in `Map1' is combined with the
% value in `Map2' by the `Combiner' fun. When `Combiner' is applied the path
% that exists in both maps is the first parameter, the value from `Map1' is
% the second parameter, and the value from `Map2' is the third parameter.
%
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' exception if any of the maps is not a map</li>
% <li>`badarg' if `Fun' is not a function of arity 3</li>
% </ul>
-spec deep_intersect_with(Fun :: combiner(), Map1 :: map(), Map2 :: map()) ->
    Map3 :: map().
deep_intersect_with(Combiner, A, B) ->
    check_map(B),
    check_fun(Combiner, 3),
    deep_intersect_with1({Combiner, flip_combiner(Combiner)}, A, B, []).

deep_intersect_with1({Combiner, Flipped}, A, B, Path) when
    map_size(A) > map_size(B)
->
    deep_intersect_with1({Flipped, Combiner}, B, A, Path);
deep_intersect_with1(Combiners, A, B, Path) ->
    deep_intersect1(maps:next(maps:iterator(A)), B, [], Combiners, Path).

deep_intersect1(none, _B, Keep, _Combiners, _Path) ->
    maps:from_list(Keep);
deep_intersect1({K, V1, Iter}, B, Keep, {Combiner, _} = Combiners, Path) ->
    NewKeep =
        case B of
            #{K := V2} when is_map(V1), is_map(V2) ->
                [{K, deep_intersect_with1(Combiners, V1, V2, Path)} | Keep];
            #{K := V2} ->
                [{K, Combiner(Path ++ [K], V1, V2)} | Keep];
            _ ->
                Keep
        end,
    deep_intersect1(maps:next(Iter), B, NewKeep, Combiners, Path).

% @doc Inverts a map by inserting each value as the key with its corresponding
%  key as the value. If two keys have the same value, the value for the first
%  key in map order will take precedence.
%
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' if `Map' is not a map</li>
% </ul>
%
% @equiv inverse(Map, fun(V, _) -> V end)
-spec inverse(map()) -> map().
inverse(Map) -> inverse(Map, fun(Old, _New) -> Old end).

% @doc Inverts a map by inserting each value as the key with its corresponding
%  key as the value. If two keys have the same value in `Map', `Fun' is called
%  with the old and new key to determine the resulting value.
%
% The call can raise the following exceptions:
% <ul>
% <li>`{badmap,Map}' if `Map' is not a map</li>
% <li>`badarg' if `Fun' is not a function of arity 2</li>
% </ul>
-spec inverse(map(), fun((Old :: term(), New :: term()) -> term())) -> map().
inverse(Map, Fun) when is_map(Map), is_function(Fun, 2) ->
    maps:fold(
        fun(K1, V, Acc) ->
            maps:update_with(V, fun(K0) -> Fun(K0, K1) end, K1, Acc)
        end,
        #{},
        Map
    );
inverse(Map, Fun) ->
    check_fun(Fun, 2),
    error_info({badmap, Map}, [Map, Fun]).

% @hidden
format_error(_Reason, [{_M, F, As, _Info} | _]) ->
    error_args(F, As).

%--- Internal Functions -------------------------------------------------------

check(Path, Map) ->
    check_path(Path),
    check_map(Map).

check_path(Path) when is_list(Path) -> ok;
check_path(Path) -> error_info({badpath, Path}, [Path]).

check_map(Map) when is_map(Map) -> ok;
check_map(Map) -> error_info({badmap, Map}, [Map]).

check_fun(Fun, Arity) when is_function(Fun, Arity) -> ok;
check_fun(_Fun, _Arity) -> exit(badarg).

search(Map, Path, Wrap, Default) -> search(Map, Path, Wrap, Default, []).

search(Element, [], Wrap, _Default, _Acc) ->
    Wrap(Element);
search(Map, [Key | Path], Wrap, Default, Acc) when is_map(Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> search(Value, Path, Wrap, Default, [Key | Acc]);
        error -> Default(error, lists:reverse([Key | Acc]))
    end;
search(Value, [_Key | _Path], _Wrap, Default, Acc) ->
    Default({ok, Value}, lists:reverse(Acc)).

update(Map, Path, Wrap, Default) -> update(Map, Path, Wrap, Default, []).

update(Map, [Key | Path], Wrap, Default, Acc) ->
    Hist = [Key | Acc],
    Value =
        case maps:find(Key, Map) of
            {ok, Existing} when is_map(Existing) ->
                update(Existing, Path, Wrap, Default, Hist);
            {ok, Existing} ->
                case Path of
                    [] -> Wrap(Existing);
                    _ -> Default(lists:reverse(Hist), Path, {ok, Existing})
                end;
            error ->
                Default(lists:reverse(Hist), Path, error)
        end,
    maps:put(Key, Value, Map);
update(Map, [], Wrap, _Default, _Acc) ->
    Wrap(Map).

remove(Map, []) ->
    Map;
remove(Map, [First]) ->
    maps:remove(First, Map);
remove(Map, [First, Second | Path]) when is_map(Map) ->
    case maps:find(First, Map) of
        {ok, Sub} when is_map(Sub) ->
            case maps:find(Second, Sub) of
                {ok, _SubSub} ->
                    maps:update(First, remove(Sub, [Second | Path]), Map);
                error ->
                    maps:remove(First, Map)
            end;
        {ok, _Value} ->
            maps:remove(First, Map);
        error ->
            Map
    end.

create(Path, Value) ->
    lists:foldr(fun(Key, Acc) -> #{Key => Acc} end, Value, Path).

badvalue_and_badkey(P, _Rest, {ok, _Existing}) -> error({badvalue, P});
badvalue_and_badkey(P, _Rest, error) -> error({badkey, P}).

badvalue_and_create(P, _Rest, {ok, _Existing}, _Init) -> error({badvalue, P});
badvalue_and_create(_P, Rest, error, Init) -> create(Rest, Init).

-ifdef(OTP_24_AND_LATER).
error_info(Reason, Args) ->
    erlang:error(Reason, Args, [{error_info, #{module => ?MODULE}}]).
-else.
error_info(Reason, Args) ->
    erlang:error(Reason, Args).
-endif.

error_args(iterator, [_Map]) ->
    #{1 => <<"not a map">>};
error_args(deep_next, [_Iter]) ->
    #{1 => <<"not a valid iterator">>}.

flip_combiner(Fun) -> fun(Path, V1, V2) -> Fun(Path, V2, V1) end.
