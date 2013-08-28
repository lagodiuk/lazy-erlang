lazy-erlang
===========

Implementation of lazy calculations concept in Erlang

### Example ###
```erlang
1>
1> c(lazy).
{ok,lazy}
2>   
2> lazy:take(10, lazy:fib()).
[2,3,5,8,13,21,34,55,89,144]
3> 
3> lazy:take(10, lazy:seq(1)).
[1,2,3,4,5,6,7,8,9,10]
4> 
4> lazy:take(10, lazy:filter(lazy:seq(1), fun(X) -> X rem 2 == 0 end)).
[2,4,6,8,10,12,14,16,18,20]
5> 
5> lazy:take(10, lazy:filter(lazy:seq(1), fun(X) -> X rem 2 == 1 end)).
[1,3,5,7,9,11,13,15,17,19]
6> 
6> lazy:take(10, lazy:zip(lazy:seq(1), lazy:fib())).
[{1,2},
 {2,3},
 {3,5},
 {4,8},
 {5,13},
 {6,21},
 {7,34},
 {8,55},
 {9,89},
 {10,144}]
```
