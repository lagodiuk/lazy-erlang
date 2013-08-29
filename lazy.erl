-module(lazy).
-export([take/2, filter/2, zip/2, transform/2]).
-export([fib/0, seq/2, seq/1, primes/0]).

primes() ->
	fun() ->
		primes(seq(2))
	end.
primes(Seq) ->
	{Prime, Gen} = Seq(),
	NextPrimeGen =
	fun() ->
		primes(filter(Gen, fun(X) -> X rem Prime /= 0 end))
	end,
	{Prime, NextPrimeGen}.

seq(Start) ->
	seq(Start, 1).
seq(Start, Delt) ->
	fun() ->
		seqNext(Start - Delt, Delt)
	end.
seqNext(Curr, Delt) ->
	Next = Curr + Delt,
	Generator =
		fun() ->
			seqNext(Next, Delt)
		end,
	{Next, Generator}.

fib() ->
	fun() ->
		fibNext(1, 1)
	end.
fibNext(Prev, Curr) ->
	Next = Prev + Curr,
	Generator = 
		fun() ->
			fibNext(Curr, Next)
		end,
	{Next, Generator}.

take(N, LazySeq) ->
	{List, _NextGen} =
		lists:foldl(
			fun(_, {List, Gen}) ->
				{Item, NextGen} = Gen(),
				{[Item | List], NextGen}
			end,
			{[], LazySeq},
			lists:seq(1, N)
		),
	lists:reverse(List).

filter(LazySeq, Predicate) ->
	fun() ->
		{Item, Gen} = LazySeq(),
		NextFilter = filter(Gen, Predicate),
		case Predicate(Item) of
			true -> {Item, NextFilter};
			false -> NextFilter()
		end
	end.

zip(LazySeq1, LazySeq2) ->
	fun() ->
		{Item1, Gen1} = LazySeq1(),
		{Item2, Gen2} = LazySeq2(),
		{{Item1, Item2}, zip(Gen1, Gen2)}
	end.

transform(LazySeq, Transformator) ->
	fun() ->
		{Item, Gen} = LazySeq(),
		TransformedItem = Transformator(Item),
		TransformGen = transform(Gen, Transformator),
		{TransformedItem, TransformGen}
	end.
