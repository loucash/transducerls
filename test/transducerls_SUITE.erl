-module(transducerls_SUITE).

-export([all/0]).
-export([t_map/1,
         t_cat/1,
         t_mapcat/1,
         t_filter/1,
         t_mf_correct/1,
         t_remove/1,
         t_take/1,
         t_drop/1,
         t_take_while/1,
         t_drop_while/1,
         t_take_nth/1,
         t_replace/1,
         t_keep/1,
         t_keep_indexed/1,
         t_partition_by/1,
         t_partition_all/1,
         t_partition_all_map/1,
         t_dedupe/1,
         t_random_sample/1,
         t_big_comp/1,
         t_frontappend/1,
         t_generator_function_input/1,
         t_compatibiliy_with_proper_transducers/1,
         t_into_list/1,
         t_shortcircuit/1,
         t_completion_forward/1,
         t_completion_backward/1,
         t_two_completing_steps/1,
         t_partition_stack/1,
         t_nones/1,
         t_clj1557/1
        ]).

-define(td, transducerls).

all() ->
    [
     t_map,
     t_cat,
     t_mapcat,
     t_filter,
     t_mf_correct,
     t_remove,
     t_take,
     t_drop,
     t_take_while,
     t_drop_while,
     t_take_nth,
     t_replace,
     t_keep,
     t_keep_indexed,
     t_partition_by,
     t_partition_all,
     t_partition_all_map,
     t_dedupe,
     t_random_sample,
     t_big_comp,
     t_frontappend,
     t_generator_function_input,
     t_compatibiliy_with_proper_transducers,
     t_into_list,
     t_shortcircuit,
     t_completion_forward,
     t_completion_backward,
     t_two_completing_steps,
     t_partition_stack,
     t_nones,
     t_clj1557
    ].

t_map(_Config) ->
    Expected = [0, 1, 4, 9, 16],
    Result = ?td:transduce(?td:map(fun(X) -> X*X end),
                           fun ?td:append/3, [], lists:seq(0,4)),
    Expected = Result,
    ok.

t_cat(_Config) ->
    Expected = [1, 2, 3, 4],
    Result = ?td:transduce(?td:cat(), fun ?td:append/3, [], [[1,2],[3,4]]),
    Expected = Result,
    ok.

t_mapcat(_Config) ->
    Expected = [1, 2, 3, 4, 5],
    Result = ?td:transduce(?td:mapcat(fun lists:reverse/1),
                           fun ?td:append/3, [], [[3,2,1],[5,4]]),
    Expected = Result,
    ok.

t_filter(_Config) ->
    Expected = [0, 2, 4],
    Result = ?td:transduce(?td:filter(fun(X) -> X rem 2 == 0 end),
                           fun ?td:append/3, [], lists:seq(0,5)),
    Expected = Result,
    ok.

t_mf_correct(_Config) ->
    Expected = lists:map(fun(X) -> X*X end,
                         lists:filter(fun(X) -> X rem 2 == 0 end,
                                      lists:seq(1, 10000))),
    Result = ?td:transduce(?td:compose([
                                        ?td:filter(fun(X) -> X rem 2 == 0 end),
                                        ?td:map(fun(X) -> X*X end)
                                       ]),
                           fun ?td:append/3, [],
                           lists:seq(1, 10000)),
    Expected = Result,
    ok.

t_remove(_Config) ->
    Expected = [1, 3, 5, 7, 9],
    Result = ?td:transduce(?td:remove(fun(X) -> X rem 2 == 0 end),
                           fun ?td:append/3, [], lists:seq(1,10)),
    Expected = Result,
    ok.

t_take(_Config) ->
    Expected = [0, 1, 2],
    Result = ?td:transduce(?td:take(3), fun ?td:append/3, [], lists:seq(0, 5)),
    Expected = Result,
    ok.

t_drop(_Config) ->
    Expected = [4],
    Result = ?td:transduce(?td:drop(4), fun ?td:append/3, [], lists:seq(0, 4)),
    Expected = Result,
    ok.

t_take_while(_Config) ->
    Expected = [2, 4, 6],
    Result = ?td:transduce(?td:take_while(fun(X) -> X rem 2 == 0 end),
                           fun ?td:append/3, [], [2, 4, 6, 7, 8]),
    Expected = Result,
    ok.

t_drop_while(_Config) ->
    Expected = [7,8],
    Result = ?td:transduce(?td:drop_while(fun(X) -> X rem 2 == 0 end),
                           fun ?td:append/3, [], [2, 4, 6, 7, 8]),
    Expected = Result,
    ok.

t_take_nth(_Config) ->
    Expected = [0, 3, 6, 9, 12, 15, 18],
    Result = ?td:transduce(?td:take_nth(3), fun ?td:append/3, [], lists:seq(0,20)),
    Expected = Result,
    ok.

t_replace(_Config) ->
    Expected = ["ok", 3, "ok", 5, "ok", 7],
    Result = ?td:transduce(?td:replace([{1, "ok"}]),
                           fun ?td:append/3, [], [1, 3, 1, 5, 1, 7]),
    Expected = Result,
    ok.

t_keep(_Config) ->
    Expected = [0, 2, 4, 6, 8],
    Result = ?td:transduce(?td:keep(fun onlyeven/1),
                           fun ?td:append/3, [], lists:seq(0, 9)),
    Expected = Result,
    ok.

t_keep_indexed(_Config) ->
    Expected = [1, 5],
    Result = ?td:transduce(?td:keep_indexed(fun onlyeven_idx/2),
                           fun ?td:append/3, [], [1, 3, 5, 7]),
    Expected = Result,
    ok.

t_partition_by(_Config) ->
    Expected = [[1, 3, 1], [4, 2], [1], [6]],
    Result = ?td:transduce(?td:partition_by(fun(X) -> X rem 2 == 0 end),
                           fun ?td:append/3, [], [1, 3, 1, 4, 2, 1, 6]),
    Expected = Result,
    ok.

t_partition_all(_Config) ->
    Expected = [[0, 1, 2, 3], [4, 5, 6, 7], [8, 9, 10, 11], [12, 13, 14]],
    Result = ?td:transduce(?td:partition_all(4),
                           fun ?td:append/3, [], lists:seq(0,14)),
    Expected = Result,
    ok.

t_partition_all_map(_Config) ->
    Expected = [[0,1,2,3],[4,5,6,7],[8,9]],
    Result = ?td:transduce(?td:compose([?td:partition_all(4), ?td:map(fun identity/1)]),
                           fun ?td:append/3, [], lists:seq(0,9)),
    Expected = Result,
    ok.

t_dedupe(_Config) ->
    Expected = [1, 3, 1, 2, 1, 4],
    Result = ?td:transduce(?td:dedupe(),
                           fun ?td:append/3, [], [1, 3, 1, 1, 2, 2, 2, 1, 4]),
    Expected = Result,
    ok.

t_random_sample(_Config) ->
    random:seed(now()),
    N = 1000,
    Counts = [length(?td:transduce(?td:random_sample(0.4),
                                   fun ?td:append/3, [], lists:seq(1, N)))
              || _ <- lists:seq(1, 100)],
    Avg = lists:sum([Count/N || Count <- Counts]) / length(Counts),
    true = abs(Avg-0.4) < 0.1,
    ok.

t_big_comp(_Config) ->
    Result = ?td:transduce(?td:compose([
                                        ?td:mapcat(fun lists:reverse/1),
                                        ?td:map(fun(X) -> X*X end),
                                        ?td:filter(fun(X) -> X rem 2 == 0 end),
                                        ?td:random_sample(1),
                                        ?td:partition_all(4),
                                        ?td:take(6)
                                       ]),
                           fun ?td:append/3, [],
                           [lists:seq(1, 10000),
                            lists:seq(1, 10000),
                            lists:seq(1, 10000)]),
    6 = length(Result),
    ok.

t_frontappend(_Config) ->
    Expected = [16, 9, 4, 1, 0],
    Result = ?td:transduce(?td:compose([
                                        ?td:take(5),
                                        ?td:map(fun(X) -> X*X end)
                                       ]),
                           fun append_left/3, [],
                           lists:seq(0, 9)),
    Expected = Result,
    ok.

t_generator_function_input(_Config) ->
    Expected = 31,
    Result = ?td:transduce(?td:take(5), fun add/3,
                           0, geometric_series(1, 2)),
    Expected = Result,
    ok.

t_compatibiliy_with_proper_transducers(_Config) ->
    Expected = [1, -2, 4, -8, 16],
    Result = ?td:transduce(?td:take(5),
                           alternating_transducer(fun ?td:append/3),
                           [], geometric_series(1, 2)),
    Expected = Result,
    ok.

t_into_list(_Config) ->
    Expected = [0, 2, 4, 6, 8, 10, 12, 14, 16, 18],
    Result = ?td:into([], ?td:map(fun(X) -> X*2 end), lists:seq(0,9)),
    Expected = Result,
    ok.

t_shortcircuit(_Config) ->
    Result = ?td:transduce(?td:compose([
                                        ?td:map(fun(X) -> X*X end),
                                        ?td:filter(fun(X) -> X rem 2 == 0 end),
                                        ?td:take(10)
                                       ]),
                           fun ?td:append/3, [],
                           geometric_series(1,2)),
    10 = length(Result),
    ok.

t_completion_forward(_Config) ->
    Expected = [[0, 1, 2, 3], [4]],
    Result = ?td:transduce(?td:compose([
                                        ?td:take(5),
                                        ?td:partition_all(4)
                                       ]),
                           fun ?td:append/3, [],
                           lists:seq(0,9)),
    Expected = Result,
    ok.

t_completion_backward(_Config) ->
    Expected = [[1, 1, 1], [2, 2, 2], [3]],
    Result = ?td:transduce(?td:compose([
                                        ?td:partition_by(fun(X) -> X rem 2 == 0 end),
                                        ?td:take(3)
                                       ]),
                           fun ?td:append/3, [],
                           [1, 1, 1, 2, 2, 2, 3]),
    Expected = Result,
    ok.

t_two_completing_steps(_Config) ->
    Expected = [[[0], [1]], [[2], [3]], [[4], [5]], [[6], [7]], [[8], [9]], [[10]]],
    Result = ?td:transduce(?td:compose([
                                        ?td:partition_by(fun(X) -> X rem 2 == 0 end),
                                        ?td:partition_all(2)
                                       ]),
                           fun ?td:append/3, [],
                           lists:seq(0, 10)),
    Expected = Result,
    ok.

t_partition_stack(_Config) ->
    Expected = [[[0, 1, 2, 3, 4]],
                [[5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19]]],
    Result = ?td:transduce(?td:compose([
                                        ?td:partition_by(fun(X) -> X < 5 end),
                                        ?td:partition_by(fun(X) -> length(X) < 6 end)
                                       ]),
                           fun ?td:append/3, [],
                           lists:seq(0, 19)),
    Expected = Result,
    ok.

t_nones(_Config) ->
    Expected = [[none, none, none],
                [none, none, none],
                [none, none, none],
                [none, none, none],
                [none, none, none]],
    Result = ?td:transduce(?td:compose([
                                        ?td:partition_all(3),
                                        ?td:take(5)
                                       ]),
                           fun ?td:append/3, [],
                           lists:duplicate(20, none)),
    Expected = Result,
    ok.

t_clj1557(_Config) ->
    Expected = [[0]],
    Result = ?td:transduce(?td:compose([
                                        ?td:take(1),
                                        ?td:partition_all(3),
                                        ?td:take(1)
                                       ]),
                           fun ?td:append/3, [],
                           lists:seq(0,14)),
    Expected = Result,
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================
onlyeven(X) ->
    case X rem 2 == 0 of
        true -> X;
        false -> undefined
    end.

onlyeven_idx(I, X) ->
    case I rem 2 == 0 of
        true -> X;
        false -> undefined
    end.

identity(I) -> I.

append_left(undefined, X, St) -> {[X], St};
append_left(R, undefined, St) -> {R, St};
append_left(R, X, St) -> {[X|R], St}.

add(undefined, X, St) -> {X, St};
add(R, undefined, St) -> {R, St};
add(R, X, St) -> {X+R, St}.

geometric_series(A, R) ->
    do_geometric_series(A, R, 0).

do_geometric_series(A, R, Power) ->
    fun() ->
        [round(A * math:pow(R, Power))|do_geometric_series(A, R, Power+1)]
    end.

alternating_transducer(Step0) ->
    TrId = ?td:transt_id(),
    Tr0 = fun({Step, St0}) ->
                  Tr = fun(undefined, X, St1)  -> Step(undefined, X, St1);
                          (R, undefined, St1)  -> Step(R, undefined, St1);
                          (R, X, St1) ->
                               {ok, Sign} = ?td:transt_get(TrId, St1),
                               Step(R, X*Sign, ?td:transt_set(TrId, Sign * -1, St1))
                       end,
                  {Tr, St0}
          end,
    Tr0({Step0, ?td:transt_set(TrId, 1, ?td:transt_new())}).
