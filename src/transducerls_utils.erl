%%%-------------------------------------------------------------------
%% @copyright Lukasz Biedrycki
%% @author Lukasz Biedrycki <lukasz.biedrycki@gmail.com>
%%%-------------------------------------------------------------------
-module(transducerls_utils).

-include("transducerls.hrl").

-export([reduce_1/2, reduce_1/3]).
-export([reduce_2/3]).
-export([reduce_funs/2]).

reduce_1(Fn, Iter, Init) ->
    reduce_1(Fn, [Init|Iter]).

reduce_1({_Fn, _St}, []) -> {error, empty};
reduce_1({_Fn, St}, [V1]) -> {V1, St};
reduce_1({Fn, St1}, [V1,V2|Rest]) ->
    {V, St2} = Fn(V1,V2,St1),
    reduce_1({Fn, St2}, [V|Rest]).

reduce_2(Fn, Gen, Init) when is_function(Gen, 0) ->
    do_reduce_2(Fn, [Init,Gen]);
reduce_2(Fn, List, Init) when is_list(List) ->
    do_reduce_2(Fn, [Init|List]).

do_reduce_2({Fn, State}, [#reduced{val=V}|_]) -> Fn(V, undefined, State);
do_reduce_2({Fn, State}, [V]) -> Fn(V, undefined, State);
do_reduce_2({Fn, State}, [V1,Gen1|Rest]) when is_function(Gen1, 0) ->
    [V2 | Gen2] = Gen1(),
    {V, NewState} = Fn(V1, V2, State),
    do_reduce_2({Fn, NewState}, [V,Gen2|Rest]);
do_reduce_2({Fn, State}, [V1,V2|Rest]) ->
    {V, NewState} = Fn(V1, V2, State),
    do_reduce_2({Fn, NewState}, [V|Rest]).

reduce_funs(Fn, List) ->
    do_reduce_funs(Fn, List).

do_reduce_funs(_, [V]) -> V;
do_reduce_funs(Fn, [V1,V2|Rest]) ->
    do_reduce_funs(Fn, [Fn(V1, V2)|Rest]).
