-module(mapz_tests).

-include_lib("eunit/include/eunit.hrl").

-define(LIST, [
    % not_a_map,
    #{a => 1, b => [1]},
    #{a => 2, b => [2]},
    #{a => 3, c => [3]}
]).
-define(STRUCT, #{
    a => #{
        a => #{
            a => 1
        },
        b => 2
    },
    b => #{
        a => 3,
        b => 4
    },
    c => ?LIST,
    d => []
}).

-import(mapz, [
    deep_find/2,
    deep_get/2,
    deep_get/3,
    deep_put/3,
    deep_update/3,
    deep_update_with/3,
    deep_remove/2,
    deep_merge/1,
    deep_merge/2,
    deep_merge/3,
    inverse/1
]).

%--- Tests --------------------------------------------------------------------

util_test_() ->
    {inparallel, [
        % Find
        ?_assertEqual({ok, 1},             deep_find([a, a, a], ?STRUCT)),
        ?_assertEqual(error,               deep_find([a, b, a], ?STRUCT)),
        % Get
        ?_assertEqual(1,                   deep_get([a, a, a], ?STRUCT)),
        ?_assertEqual(#{a => 1},           deep_get([a, a], ?STRUCT)),
        ?_assertEqual(d,                   deep_get([a, c], ?STRUCT, d)),
        ?_assertEqual(1,                   deep_get([a, a, a], ?STRUCT, d)),
        ?_assertEqual(d,                   deep_get([a, b, c], ?STRUCT, d)),
        ?_assertEqual(?STRUCT,             deep_get([], ?STRUCT)),
        % Put
        ?_assertEqual(v,                   deep_put([], v, #{})),
        ?_assertEqual(
            v,
            deep_get([a, a, a], deep_put([a, a, a], v, ?STRUCT))
        ),
        ?_assertEqual(
            #{a => 1, x => #{y => #{a => 3}}},
            deep_get([a, a], deep_put([a, a, x, y], #{a => 3}, ?STRUCT))
        ),
        ?_assertError({badvalue, [d]},      deep_put([d, x], y, ?STRUCT)),
        ?_assertError({badvalue, [a, b]},      deep_put([a, b, c], 1, ?STRUCT)),

        deep_update_(),
        deep_update_with_(),
        deep_remove_(),
        deep_merge_(),
        deep_merge_fun_(),
        error_(fun(Path, Map) -> deep_get(Path, Map) end)
    ]}.

deep_update_() ->
    {inparallel, [
        ?_assertEqual(#{a => 2}, deep_update([a], 2, #{a => 1})),
        ?_assertEqual(
            deep_put([a, a, a], 2, ?STRUCT),
            deep_update([a, a, a], 2, ?STRUCT)
        )
    ] ++ error_(fun(P, M) -> deep_update(P, 2, M) end)}.

deep_update_with_() ->
    Incr = fun(V) -> V + 1 end,
    {inparallel, [
        ?_assertEqual(#{a => 2}, deep_update_with([a], Incr, #{a => 1})),
        ?_assertEqual(
            deep_put([a, a, a], 2, ?STRUCT),
            deep_update_with([a, a, a], Incr, ?STRUCT)
        ),
        ?_assertExit(badarg, deep_update_with([a], x, ?STRUCT)),
        ?_assertExit(badarg, deep_update_with([a], fun() -> foo end, ?STRUCT))
    ] ++ error_(fun(P, M) -> deep_update_with(P, Incr, M) end)}.

error_(Function) ->
    {inparallel, [
        ?_assertError({badmap, foobar},  Function([a], foobar)),
        ?_assertError({badpath, foobar}, Function(foobar, #{a => 1})),
        ?_assertError({badkey, [b]},     Function([b], #{a => 1})),
        ?_assertError({badkey, [b, x]},  Function([b, x], ?STRUCT)),
        ?_assertError({badvalue, [d]},   Function([d, x], ?STRUCT))
    ]}.

deep_remove_() ->
    {inparallel, [
        ?_assertEqual(?STRUCT, deep_remove([], ?STRUCT)),
        ?_assertEqual(?STRUCT, deep_remove([y], ?STRUCT)),
        ?_assertEqual(?STRUCT, deep_remove([y, x], ?STRUCT)),
        ?_assertEqual(
            #{b => 2},
            deep_get([a], deep_remove([a, a], ?STRUCT))
        ),
        ?_assertEqual(
            #{b => 2},
            deep_get([a], deep_remove([a, a, 0], ?STRUCT))
        ),
        ?_assertEqual(#{}, deep_get([a, a], deep_remove([a, a, a], ?STRUCT))),
        ?_assertEqual(
            #{a => #{a => 1}},
            deep_get([a], deep_remove([a, b, a], ?STRUCT))
        )
    ]}.

deep_merge_() ->
    [First, Second|_] = Maps = [
        #{val => 1, a => 1},
        #{val => 2, b => 2, x => #{2 => true, y => #{more => stuff}}},
        #{val => 3, c => 3, x => #{3 => true}},
        #{val => 4, d => 4, x => #{4 => true, y => #{extra => data}}}
    ],
    Expected = #{
        val => 4,
        a => 1,
        b => 2,
        c => 3,
        d => 4,
        x => #{
            2 => true,
            3 => true,
            4 => true,
            y => #{more => stuff, extra => data}
        }
    },
    {inparallel, [
        ?_assertEqual(?STRUCT, deep_merge([?STRUCT, ?STRUCT])),
        ?_assertEqual(Expected, mapz:deep_merge(Maps)),
        ?_assertEqual(deep_merge([First, Second]), deep_merge(First, Second))
    ]}.

deep_merge_fun_() ->
    First = #{a => [1, 2], b => #{c => [a]}},
    Second = #{a => [3, 4], b => #{c => [b]}},
    Fun = fun(A, B) -> A ++ B end,
    Expected = #{
        a => [1, 2, 3, 4],
        b => #{c => [a, b]}
    },
    {inparallel, [
        ?_assertEqual(Expected, deep_merge(Fun, First, Second))
    ]}.

badmap_test_() ->
    {inparallel, [
        ?_assertError({badmap, 1}, deep_find([a], 1)),
        ?_assertError({badmap, 1}, deep_get([a], 1)),
        ?_assertError({badmap, 1}, deep_get([a], 1, d)),
        ?_assertError({badmap, 1}, deep_put([a], v, 1)),
        ?_assertError({badmap, 1}, deep_remove([a], 1)),
        ?_assertError({badmap, 1}, deep_merge(1, #{})),
        ?_assertError({badmap, 2}, deep_merge(#{}, 2)),
        ?_assertError({badmap, 1}, deep_merge([#{}, #{}, 1])),
        ?_assertError({badmap, 1}, deep_merge(fun(_, _) -> ok end, 1, #{})),
        ?_assertError({badmap, 2}, deep_merge(fun(_, _) -> ok end, #{}, 2))
    ]}.

badpath_test_() ->
    {inparallel, [
        ?_assertError({badpath, 1}, deep_find(1, #{})),
        ?_assertError({badpath, 1}, deep_get(1, #{})),
        ?_assertError({badpath, 1}, deep_get(1, #{}, d)),
        ?_assertError({badpath, 1}, deep_put(1, v, #{})),
        ?_assertError({badpath, 1}, deep_remove(1, #{}))
    ]}.

inverse_test_() ->
    {inparallel, [
        ?_assertEqual(#{1 => a, 2 => c}, inverse(#{a => 1, b => 2, c => 2}))
    ]}.
