%%%-------------------------------------------------------------------
%% @copyright Lukasz Biedrycki
%% @author Lukasz Biedrycki <lukasz.biedrycki@gmail.com>
%%%-------------------------------------------------------------------
-module(transducerls).
-author('Lukasz Biedrycki <lukasz.biedrycki@gmail.com>').

-export([transduce/4, into/3]).
-export([compose/1]).
-export([
         map/1, cat/0, mapcat/1,
         filter/1, remove/1,
         take/1, drop/1,
         take_while/1, drop_while/1,
         take_nth/1,
         replace/1,
         keep/1, keep_indexed/1,
         partition_by/1, partition_all/1,
         dedupe/0,
         random_sample/1
        ]).
-export([append/3]).
-export([ensure_reduced/1]).
-export([transt_new/0, transt_new/1, transt_id/0, transt_get/2, transt_set/3]).

-include("transducerls.hrl").

-record(transt, {
          states = dict:new()    :: dict()
         }).
-opaque transt() :: #transt{}.
-export_type([transt/0]).

compose(Fns) ->
    Reducer = fun({F, FSt}, {G, GSt}) ->
                Tr = fun(Args) -> F(G(Args)) end,
                St = merge(FSt, GSt),
                {Tr, St}
              end,
    ?utils:reduce_funs(Reducer, Fns).

merge(#transt{states=D1}, #transt{states=D2}) ->
    transt_new(dict:merge(fun(_K, V1, _V2) -> V1 end, D1, D2)).

transduce({XForm, St}, {F, FSt}, Start, Items) ->
    transduce({XForm, merge(St, FSt)}, F, Start, Items);
transduce({XForm, St}, F, Start, Items) ->
    Reducer = XForm({F, St}),
    {Result, _} = ?utils:reduce_2(Reducer, Items, Start),
    Result.

into(Target, Xducer, Items) ->
    transduce(Xducer, fun append/3, Target, Items).

%%%===================================================================
%%% Transducers
%%%===================================================================
% @doc Transducer version of map, returns f(item) with each reduction step.
map(F) ->
    Tr0 = fun({Step, St0}) ->
                  Tr1 = fun(undefined, X, St1) -> Step(undefined, X, St1);
                           (R, undefined, St1) -> Step(R, undefined, St1);
                           (R, X, St1)         -> Step(R, F(X), St1) end,
                  {Tr1, St0}
          end,
    {Tr0, transt_new()}.

% @doc Cat transducers (will cat items from nested lists, e.g.).
cat() ->
    Tr0 = fun({Step, St0}) ->
            Tr = fun(undefined, X, St1) -> Step(undefined, X, St1);
                    (R, undefined, St1) -> Step(R, undefined, St1);
                    (R, X, St1)         -> ?utils:reduce_1({Step, St1}, X, R)
                end,
            {Tr, St0}
          end,
    {Tr0, transt_new()}.

% @doc Mapcat transducer - maps to a collection then cats item into one less
% level of nesting.
mapcat(F) ->
    compose([map(F), cat()]).

% @doc Transducer version of filter.
filter(Pred) ->
    Tr0 = fun({Step, St0}) ->
            Tr = fun(undefined, X, St1) -> Step(undefined, X, St1);
                    (R, undefined, St1) -> Step(R, undefined, St1);
                    (R, X, St1)         ->
                        case Pred(X) of
                            true -> Step(R, X, St1);
                            false -> {R, St1}
                        end
                end,
            {Tr, St0}
          end,
    {Tr0, transt_new()}.

% @doc Drops n items from beginning of input sequence.
remove(Pred) ->
    Tr0 = fun({Step, St0}) ->
            Tr = fun(undefined, X, St1) -> Step(undefined, X, St1);
                    (R, undefined, St1) -> Step(R, undefined, St1);
                    (R, X, St1)         ->
                        case Pred(X) of
                            true -> {R, St1};
                            false -> Step(R, X, St1)
                        end
                end,
            {Tr, St0}
          end,
    {Tr0, transt_new()}.

% @doc Takes n values from a collection.
take(N) when N > 0 ->
    TrId = transt_id(),
    Tr0 = fun({Step, St0}) ->
            Tr = fun(undefined, X, St1)  -> Step(undefined, X, St1);
                      (R, undefined, St1)  -> Step(R, undefined, St1);
                      (R, X, St1) ->
                           {ok, M} = transt_get(TrId, St1),
                           case M of
                               0 -> {ensure_reduced(R), St1};
                               _ ->  Step(R, X, transt_set(TrId, M-1, St1))
                           end
                   end,
            {Tr, St0}
          end,
    {Tr0, transt_set(TrId, N, transt_new())}.

% @doc Drops n items from beginning of input sequence.
drop(N) when N > 0 ->
    TrId = transt_id(),
    Tr0 = fun({Step, St0}) ->
                  Tr = fun(undefined, X, St1)   -> Step(undefined, X, St1);
                          (R, undefined, St1)   -> Step(R, undefined, St1);
                          (R, X, St1) ->
                            {ok, M} = transt_get(TrId, St1),
                            case M > 0 of
                                true -> {R, transt_set(TrId, M-1, St1)};
                                false -> Step(R, X, St1)
                            end
                       end,
            {Tr, St0}
          end,
    {Tr0, transt_set(TrId, N, transt_new())}.

% @doc Takes while a condition is true.
take_while(Pred) ->
    Tr0 = fun({Step, St0}) ->
            Tr = fun(undefined, X, St1) -> Step(undefined, X, St1);
                    (R, undefined, St1) -> Step(R, undefined, St1);
                    (R, X, St1)         ->
                        case Pred(X) of
                            true -> Step(R, X, St1);
                            false -> {reduced(R), St1}
                        end
                end,
            {Tr, St0}
          end,
    {Tr0, transt_new()}.

% @doc Drops values so long as a condition is true.
drop_while(Pred) ->
    TrId = transt_id(),
    Tr0 = fun({Step, St0}) ->
            Tr = fun(undefined, X, St1)   -> Step(undefined, X, St1);
                      (R, undefined, St1)   -> Step(R, undefined, St1);
                      (R, X, St1) ->
                        {ok, TrSt} = transt_get(TrId, St1),
                        case TrSt of
                            take -> Step(R, X, St1);
                            drop ->
                                case Pred(X) of
                                    true -> {R, St1};
                                    false -> Step(R, X, transt_set(TrId, take, St1))
                                end
                        end
                   end,
            {Tr, St0}
          end,
    {Tr0, transt_set(TrId, drop, transt_new())}.

% @doc Takes every nth item from input values.
take_nth(N) when N > 0 ->
    TrId = transt_id(),
    Tr0 = fun({Step, St0}) ->
            Tr = fun(undefined, X, St1)  -> Step(undefined, X, St1);
                    (R, undefined, St1)  -> Step(R, undefined, St1);
                    (R, X, St1) ->
                         {ok, Idx} = transt_get(TrId, St1),
                         case Idx rem N == 0 of
                             true ->
                                 Step(R, X, transt_set(TrId, Idx+1, St1));
                             false ->
                                 {R, transt_set(TrId, Idx+1, St1)}
                         end
                 end,
            {Tr, St0}
          end,
    {Tr0, transt_set(TrId, 0, transt_new())}.

% @doc Replaces keys in smap with corresponding values.
replace(Proplist) ->
    Tr0 = fun({Step, St0}) ->
            Tr = fun(undefined, X, St1) -> Step(undefined, X, St1);
                    (R, undefined, St1) -> Step(R, undefined, St1);
                    (R, X, St1)         -> Step(R, proplists:get_value(X, Proplist, X), St1)
                end,
            {Tr, St0}
          end,
    {Tr0, transt_new()}.

% @doc Keep pred items for which pred does not return undefined.
keep(Pred) ->
    Tr0 = fun({Step, St0}) ->
            Tr = fun(undefined, X, St1) -> Step(undefined, X, St1);
                    (R, undefined, St1) -> Step(R, undefined, St1);
                    (R, X, St1)         ->
                         case Pred(X) of
                             undefined -> {R, St1};
                             Res -> Step(R, Res, St1)
                         end
                end,
            {Tr, St0}
          end,
    {Tr0, transt_new()}.

% @doc Keep values where f does not return None. f for keep indexed is a
% function that takes both index and value as inputs.
keep_indexed(Pred) ->
    TrId = transt_id(),
    Tr0 = fun({Step, St0}) ->
            Tr = fun(undefined, X, St1)  -> Step(undefined, X, St1);
                    (R, undefined, St1)  -> Step(R, undefined, St1);
                    (R, X, St1) ->
                         {ok, Idx} = transt_get(TrId, St1),
                         St2 = transt_set(TrId, Idx+1, St1),
                         case Pred(Idx, X) of
                             undefined ->
                                 {R, St2};
                             Res ->
                                 Step(R, Res, St2)
                         end
                end,
            {Tr, St0}
          end,
    {Tr0, transt_set(TrId, 0, transt_new())}.

% @doc Split inputs into lists by starting a new list each time the predicate
% passed in evaluates to a different condition (true/false) than what holds
% for the present list.
partition_by(Pred) ->
    TrId = transt_id(),
    Tr0 = fun({Step, St0}) ->
            Tr = fun(undefined, X, St1) -> Step(undefined, X, St1);
                    (R, undefined, St1) ->
                         {ok, {_, Acc}} = transt_get(TrId, St1),
                         case Acc of
                             [] -> {R, St1};
                             _  ->
                                 {Res, St2} = unreduced(Step(R, lists:reverse(Acc), St1)),
                                 Step(Res, undefined, St2)
                         end;
                    (R, X, St1) ->
                         {ok, {Last, Acc}} = transt_get(TrId, St1),
                         Present = Pred(X),
                         case Last =:= undefined orelse Last =:= Present of
                             true ->
                                 {R, transt_set(TrId, {Present, [X|Acc]}, St1)};
                             false ->
                                 {Res, St2} = Step(R, lists:reverse(Acc), St1),
                                 NewAcc = case Res of
                                              #reduced{} -> [];
                                              _ -> [X]
                                          end,
                                 {Res, transt_set(TrId, {Present, NewAcc}, St2)}
                         end
                   end,
            {Tr, St0}
          end,
    {Tr0, transt_set(TrId, {undefined, []}, transt_new())}.

% @doc Splits inputs into lists of size n.
partition_all(N) when N > 0 ->
    TrId = transt_id(),
    Tr0 = fun({Step, St0}) ->
            Tr = fun(undefined, X, St1) -> Step(undefined, X, St1);
                    (R, undefined, St1) ->
                         {ok, {_, Acc}} = transt_get(TrId, St1),
                         case Acc of
                             [] -> {R, St1};
                             _  ->
                                 {Res, St2} = unreduced(Step(R, lists:reverse(Acc), St1)),
                                 Step(Res, undefined, St2)
                         end;
                    (R, X, St1) ->
                         {ok, {Len0, Acc0}} = transt_get(TrId, St1),
                         Len = Len0 + 1,
                         Acc = [X|Acc0],
                         case Len =:= N of
                             true ->
                                 Step(R, lists:reverse(Acc),
                                      transt_set(TrId, {0, []}, St1));
                             false ->
                                 {R, transt_set(TrId, {Len, Acc}, St1)}
                         end
                   end,
            {Tr, St0}
          end,
    {Tr0, transt_set(TrId, {0, []}, transt_new())}.

% @doc Removes duplicatees that occur in order. Accepts first inputs through
% and drops subsequent duplicates.
dedupe() ->
    TrId = transt_id(),
    Tr0 = fun({Step, St0}) ->
            Tr = fun(undefined, X, St1) -> Step(undefined, X, St1);
                    (R, undefined, St1) -> Step(R, undefined, St1);
                    (R, X, St1) ->
                         {ok, Last} = transt_get(TrId, St1),
                         case Last of
                             [X] ->
                                 {R, St1};
                             _ ->
                                 Step(R, X, transt_set(TrId, [X], St1))
                         end
                end,
            {Tr, St0}
          end,
    {Tr0, transt_set(TrId, [], transt_new())}.

% @doc Has prob probability of returning each input it receives.
random_sample(Prob) ->
    Tr0 = fun({Step, St0}) ->
            Tr = fun(undefined, X, St1) -> Step(undefined, X, St1);
                    (R, undefined, St1) -> Step(R, undefined, St1);
                    (R, X, St1)         ->
                         case random:uniform() < Prob of
                             true -> Step(R, X, St1);
                             false -> {R, St1}
                         end
                end,
            {Tr, St0}
          end,
    {Tr0, transt_new()}.

%%%===================================================================
%%% Helpers
%%%===================================================================
transt_new() ->
    #transt{}.

transt_new(States) ->
    #transt{states=States}.

transt_id() ->
    erlang:phash2(erlang:now()).

transt_get(Id, T) ->
    dict:find(Id, T#transt.states).

transt_set(Id, Val, #transt{states=D}=T) ->
    T#transt{states=dict:store(Id, Val, D)}.

ensure_reduced(#reduced{}=X) -> X;
ensure_reduced(X) -> reduced(X).

append(undefined, X, St) -> {[X], St};
append(R, undefined, St) -> {lists:reverse(R), St};
append(R, X, St) -> {[X|R], St}.

reduced(X) -> #reduced{val=X}.

unreduced({#reduced{val=X}, St}) -> {X, St};
unreduced({X, St}) -> {X, St}.
