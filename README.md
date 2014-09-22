transducerls
============

[Transducers](http://clojure.org/transducers) are composable algorithmic transformations.
They are independent from the context of their input and output sources and specify only the essence of the transformation in terms of an individual element.
Transducers compose directly, without awareness of input or creation of intermediate aggregates.

This is an implementation in Erlang and it is based on implementation in
python: [transducers-python](https://github.com/cognitect-labs/transducers-python).

For more information about Clojure transducers and transducer semantics see
the introductory [blog post](http://blog.cognitect.com/blog/2014/8/6/transducers-are-coming)
and this [video](https://www.youtube.com/watch?v=6mTbuzafcII).


## Usage

```erlang
> geometric_series(A, R) ->
>     do_geometric_series(A, R, 0).

> do_geometric_series(A, R, Power) ->
>     fun() ->
>         [round(A * math:pow(R, Power))|do_geometric_series(A, R, Power+1)]
>     end.

> add(undefined, X, St) -> {X, St};
> add(R, undefined, St) -> {R, St};
> add(R, X, St) -> {X+R, St}.

> transducerls:transduce(transducerls:take(5), fun add/3, 0, geometric_series(1, 2)).
31

```
For more examples of use, see the test suite test/transducerls_SUITE.erl.


## How to build it
`make compile`

## How to run tests
`make tests`

## Contributing

If you see something missing or incorrect, do not hesitate to create an issue
or pull request. Thank you!

## Roadmap
- would be nice to have an API to process infinite streams
- consider ets to keep a state - this will simplify transducers implementation

## Changelog

- 0.1.0: Initial Release

## Authors

- Lukasz Biedrycki / @loucash: current implementation
