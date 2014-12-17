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
    deep_remove/2,
    deep_merge/1
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
        ?_assertError(bad_key,             deep_get([a, b, a], ?STRUCT)),
        ?_assertEqual(d,                   deep_get([a, c], ?STRUCT, d)),
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
        % Remove
        ?_assertEqual(
            #{},
            deep_get([a, a], deep_remove([a, a, a], ?STRUCT))
        ),
        ?_assertError(bad_key,             deep_remove([y, x], ?STRUCT)),
        ?_assertError(bad_key,             deep_remove([a, a, x], ?STRUCT)),
        % Merge
        ?_assertEqual(?STRUCT, deep_merge([?STRUCT, ?STRUCT])),
        deep_merge_(),
        deep_merge_fun_()
    ]}.

deep_merge_() ->
    [First, Second|_] = Maps = [
        #{a => 1, x => removed},
        #{b => 2, x => #{2 => true, y => #{more => stuff}}},
        #{c => 3, x => #{3 => true, y => removed}},
        #{d => 4, x => #{4 => true, y => #{extra => data}}}
    ],
    Expected = #{
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
        ?_assertEqual(Expected, mapz:deep_merge(Maps)),
        ?_assertEqual(Expected, mapz:deep_merge(hd(Maps), tl(Maps))),
        ?_assertEqual(
            mapz:deep_merge([First, Second]),
            mapz:deep_merge(First, Second)
        )
    ]}.

deep_merge_fun_() ->
    Maps = [
        #{a => [1, 2], b => #{c => [a]}},
        #{a => [3, 4], b => #{c => [b]}}
    ],
    Fun = fun(A, B) -> A ++ B end,
    Expected = #{
        a => [1, 2, 3, 4],
        b => #{c => [a, b]}
    },
    {inparallel, [
        ?_assertEqual(Expected, mapz:deep_merge(Fun, Maps))
    ]}.
