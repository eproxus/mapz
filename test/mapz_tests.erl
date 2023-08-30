-module(mapz_tests).

-include_lib("eunit/include/eunit.hrl").

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
    d => [],
    e => #{}
}).
-define(TO_MERGE, [
    #{val => 1, a => 1},
    #{val => 2, b => 2, x => #{2 => true, y => #{more => stuff}}},
    #{val => 3, c => 3, x => #{3 => true}},
    #{val => 4, d => 4, x => #{4 => true, y => #{extra => data}}}
]).
-define(MERGED, #{
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
}).

-import(mapz, [
    deep_find/2,
    deep_search/2,
    deep_get/2,
    deep_get/3,
    deep_put/3,
    deep_update/3,
    deep_update_with/3,
    deep_update_with/4,
    deep_remove/2,
    deep_merge/1,
    deep_merge/2,
    deep_merge/3,
    deep_merge_with/2,
    deep_merge_with/3,
    deep_iterator/1,
    deep_next/1,
    inverse/1,
    inverse/2
]).

-define(_badarg(Function),
    ?_assertError({badmap, foobar}, (Function)([a], foobar)),
    ?_assertError({badpath, foobar}, (Function)(foobar, #{a => 1}))
).

-define(_badvalue(Function),
    ?_assertError({badvalue, [d]}, (Function)([d, x], ?STRUCT)),
    ?_assertError({badvalue, [a, b]}, (Function)([a, b, c], ?STRUCT))
).

-define(_badkey(Function),
    ?_assertError({badkey, [b]}, (Function)([b], #{a => 1})),
    ?_assertError({badkey, [b, x]}, (Function)([b, x], ?STRUCT))
).

%--- Tests --------------------------------------------------------------------

deep_find_test_() ->
    {inparallel, [
        ?_assertEqual({ok, 1}, deep_find([a, a, a], ?STRUCT)),
        ?_assertEqual(error, deep_find([a, b, a], ?STRUCT))
    ]}.

deep_search_test_() ->
    {inparallel, [
        ?_assertEqual({ok, 1}, deep_search([a, a, a], ?STRUCT)),
        ?_assertEqual({error, [a, b], 2}, deep_search([a, b, c], ?STRUCT)),
        ?_assertEqual(
            {error, [b], #{a => 3, b => 4}}, deep_search([b, c], ?STRUCT)
        ),
        ?_assertEqual({error, [], ?STRUCT}, deep_search([x], ?STRUCT)),
        ?_assertEqual({ok, ?STRUCT}, deep_search([], ?STRUCT)),
        ?_badarg(deep_search)
    ]}.

deep_get_test_() ->
    {inparallel, [
        ?_assertEqual(1, deep_get([a, a, a], ?STRUCT)),
        ?_assertEqual(#{a => 1}, deep_get([a, a], ?STRUCT)),
        ?_assertEqual(d, deep_get([a, c], ?STRUCT, d)),
        ?_assertEqual(1, deep_get([a, a, a], ?STRUCT, default)),
        ?_assertEqual(default, deep_get([a, b, c], ?STRUCT, default)),
        ?_assertEqual(?STRUCT, deep_get([], ?STRUCT)),
        ?_badarg(deep_get),
        ?_badvalue(deep_get),
        ?_badkey(deep_get)
    ]}.

deep_put_test_() ->
    {inparallel, [
        ?_assertEqual(v, deep_put([], v, #{})),
        ?_assertEqual(v, deep_get([a, a, a], deep_put([a, a, a], v, ?STRUCT))),
        ?_assertEqual(
            #{a => 1, x => #{y => #{a => 3}}},
            deep_get([a, a], deep_put([a, a, x, y], #{a => 3}, ?STRUCT))
        ),
        ?_badvalue(fun(P, M) -> deep_put(P, y, M) end)
    ]}.

deep_update_test_() ->
    {inparallel, [
        ?_assertEqual(#{a => 2}, deep_update([a], 2, #{a => 1})),
        ?_assertEqual(
            deep_put([a, a, a], 2, ?STRUCT),
            deep_update([a, a, a], 2, ?STRUCT)
        ),
        ?_badarg(fun(P, M) -> deep_update(P, 2, M) end),
        ?_badvalue(fun(P, M) -> deep_update(P, 2, M) end),
        ?_badkey(fun(P, M) -> deep_update(P, 2, M) end)
    ]}.

deep_update_with_test_() ->
    Incr = fun(V) -> V + 1 end,
    {inparallel, [
        ?_assertEqual(#{a => 2}, deep_update_with([a], Incr, #{a => 1})),
        ?_assertEqual(
            deep_put([a, a, a], 2, ?STRUCT),
            deep_update_with([a, a, a], Incr, ?STRUCT)
        ),
        ?_assertEqual(
            deep_put([e], #{v => 1}, ?STRUCT),
            deep_update_with([e], fun(M) -> M#{v => 1} end, ?STRUCT)
        ),
        ?_assertExit(badarg, deep_update_with([a], x, ?STRUCT)),
        ?_assertExit(
            badarg,
            deep_update_with([a], fun() -> foo end, ?STRUCT)
        ),
        ?_badarg(fun(P, M) -> deep_update_with(P, Incr, M) end),
        ?_badvalue(fun(P, M) -> deep_update_with(P, Incr, M) end),
        ?_badkey(fun(P, M) -> deep_update_with(P, Incr, M) end)
    ]}.

deep_update_with_init_test_() ->
    Incr = fun(V) -> V + 1 end,
    {inparallel, [
        ?_assertEqual(#{a => 2}, deep_update_with([a], Incr, 0, #{a => 1})),
        ?_assertEqual(
            deep_put([a, a, a], 2, ?STRUCT),
            deep_update_with([a, a, a], Incr, 0, ?STRUCT)
        ),
        ?_assertEqual(
            deep_put([a, a, x, y], 0, ?STRUCT),
            deep_update_with([a, a, x, y], Incr, 0, ?STRUCT)
        ),
        ?_assertExit(badarg, deep_update_with([a], x, 0, ?STRUCT)),
        ?_assertExit(
            badarg,
            deep_update_with([a], fun() -> foo end, 0, ?STRUCT)
        ),
        ?_badarg(fun(P, M) -> deep_update_with(P, Incr, 0, M) end),
        ?_badvalue(fun(P, M) -> deep_update_with(P, Incr, 0, M) end)
    ]}.

deep_remove_test_() ->
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

deep_merge_test_() ->
    [First, Second | _] = Maps = ?TO_MERGE,
    Expected = ?MERGED,
    {inparallel, [
        ?_assertEqual(?STRUCT, deep_merge([?STRUCT, ?STRUCT])),
        ?_assertEqual(Expected, mapz:deep_merge(Maps)),
        ?_assertEqual(deep_merge([First, Second]), deep_merge(First, Second))
    ]}.

deep_merge_fun_test_() ->
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

deep_merge_with_test_() ->
    Override = fun(_P, _A, B) -> B end,
    Append = fun
        (P, A, B) when is_list(A), is_list(B) ->
            io:format("~p ~p ~p~n", [P, A, B]),
            {P, A ++ B};
        (P, A, B) ->
            io:format("~p ~p ~p~n", [P, A, B]),
            {P, {A, B}}
    end,
    {inparallel, [
        ?_assertEqual(?STRUCT, deep_merge_with(Override, ?STRUCT, ?STRUCT)),
        ?_assertEqual(?MERGED, deep_merge_with(Override, ?TO_MERGE)),
        ?_assertEqual(
            #{
                a => {[a], [1, 2]},
                b => #{c => {[b, c], [3, 4]}},
                d => {[d], {5, 6}},
                e => 7,
                x => {[x], {[foo], #{y => [bar]}}}
            },
            deep_merge_with(
                Append,
                #{a => [1], b => #{c => [3]}, d => 5, x => [foo]},
                #{
                    a => [2],
                    b => #{c => [4]},
                    d => 6,
                    e => 7,
                    x => #{y => [bar]}
                }
            )
        )
    ]}.

deep_iterator_test_() ->
    {inparallel, [
        ?_assertError({badmap, foo}, deep_iterator(foo)),
        ?_assertError(badarg, deep_next(foo)),
        ?_assertEqual(none, deep_next(deep_iterator(#{}))),
        ?_assertEqual(
            [{[a], 1}],
            exhaust(deep_iterator(#{a => 1}))
        ),
        ?_assertEqual(
            lists:sort([
                {[a], #{b => 2}},
                {[a, b], 2},
                {[c], 3}
            ]),
            lists:sort(exhaust(deep_iterator(#{a => #{b => 2}, c => 3})))
        ),
        ?_assertEqual(
            lists:sort([
                {[a], deep_get([a], ?STRUCT)},
                {[a, a], deep_get([a, a], ?STRUCT)},
                {[a, a, a], 1},
                {[a, b], 2},
                {[b], deep_get([b], ?STRUCT)},
                {[b, a], 3},
                {[b, b], 4},
                {[d], []},
                {[e], #{}}
            ]),
            lists:sort(exhaust(deep_iterator(?STRUCT)))
        )
    ]}.

%--- Internal ------------------------------------------------------------------

exhaust(I) ->
    exhaust(deep_next(I), []).

exhaust(none, Acc) ->
    lists:reverse(Acc);
exhaust({K, V, I}, Acc) ->
    exhaust(deep_next(I), [{K, V} | Acc]).

inverse_test_() ->
    {inparallel, [
        ?_assertEqual(#{}, inverse(#{})),
        fun() ->
            Inverted = inverse(#{a => 1, b => 2, c => 2}),
            ?assertEqual([1, 2], lists:sort(maps:keys(Inverted))),
            #{1 := a, 2 := Second} = Inverted,
            ?assert(lists:member(Second, [b, c]))
        end,
        fun() ->
            Inverted = inverse(
                #{a => 1, b => 2, c => 2, d => 2},
                fun
                    (Old, New) when is_list(Old) -> Old ++ [New];
                    (Old, New) -> [Old, New]
                end
            ),
            ?assertEqual([1, 2], lists:sort(maps:keys(Inverted))),
            #{1 := a, 2 := Second} = Inverted,
            ?assertEqual([b, c, d], lists:sort(Second))
        end,
        ?_assertError({badmap, foo}, inverse(foo)),
        ?_assertError(
            {badmap, foo},
            inverse(foo, fun(_, _) -> error(should_not_occur) end)
        ),
        ?_assertExit(badarg, inverse(#{}, foo)),
        ?_assertExit(badarg, inverse(#{}, fun() -> ok end)),
        ?_assertExit(badarg, inverse(#{}, fun(_, _, _) -> ok end))
    ]}.
