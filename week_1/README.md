# Week 1 Notes

## 1.5

* Erlang shell has some nice Emacs shortcuts. Most of them are outlined at https://github.com/erlang/otp/blob/504d151a57bc93acb3fb7597214a8e62be9c1cfd/lib/stdlib/src/edlin.erl#L186. MAybe write a blog post about these.

* In the shell one can create anonymous functions(lambdas) like `SumOfTwo = fun(A, B) -> A + B end.` and the evaluate like `SumOfTwo(10, 14).`.

* To forget varaibale bindings in the shell, use `f(VariableName).` or `f().` to forget all.

## 1.7

* Did a simple excersise [first.erl](1.7/first.erl).

## 1.8

* Did another excersise for more complex expressions, multiple expressions in a function separated by comma, the last expression is the result of the function. Code at [first.erl](1.8/first.erl). When Erlang files compile, they generate a **.beam** file which is the machine code interpreted by the VM. 

## 1.9

* This is the first excersise one has tho write without the professor writting it. My solution located at [first.erl](1.9/first.erl) and [second.erl](1.9/second.erl).

## 1.10: Erlang data: Numbers and atoms

* Had some nice discussions about the complexity of pattern matching with diffferent data types, still unsure what the complexity for all the types is but got pointed to https://github.com/happi/theBeamBook by Brujo Benavides ( Thanks :) ). Learned that the algorithms used by Erlang are from the book _The implementation of functional programming languages_ by Simon Peyton Jones, where basically patterns are compiled down to decision trees (nested case statements). Also learned that these desicion trees can be seen by compiling to [Core Erlang](http://www.it.uu.se/research/group/hipe/cerl/) or Beam Assembly.
Compiling to core Erlang can be done in the shell with:

```erlang
> c(yourmodule, to_core).
```

or 

```console
$ erlc +to_core yourmodule.erl
```

If you want see asm code of beam you can use the shell

```erlang
> c(yourmodule, 'S').
```

or

```console
$ erlc -S yourmodule.erl
```

* Integers are `bignums`.

* Different bases could be used with `base#number`: `> 2#100 = 4.`.

* Operators: `+ - * / div rem`.

* Booleans are just special atoms `true, false`.

## 1.11: Erlang data: tuples, lists and functions

* For tuples, it's a common idiom to use the first field to indicate the sort of data in the tuple.

```erlang
> {coordinates, {27.876492,-82.82442}}.
```

* Lists and Tuples are similar collection of values that can have elements of different types. We have both because we can do different things with them, lists could be iterated and are built one element at the time and tuples are built in one go.

* Strings are simply list of characters, both `"abc".` and `[97, 98, 99].` print as "abc". Useful to look at the ASCII table http://www.asciitable.com/index/asciifull.gif or we can prefix the character with $, like `[$a, $b, $c]`. For later study [3.5  Standard Unicode Representation](http://erlang.org/doc/apps/stdlib/unicode_usage.html#standard-unicode-representation), still unclear to me how are unicode strings represented fine in the shell but they are not list of ASCII anymore and rather list of Unicode code points. For example `erlang:iolist_to_binary/1` will work fine with "abc" but not with "döpe̊" where you need to use `unicode:characters_to_binary/1,2,3`. 

* Functions can be data

```erlang
1> fun (X) -> X * 2 end.
#Fun<erl_eval.7.126501267>
```

and can be arguments of other functions

```erlang
1> lists:map(fun (X) -> X * 2 end, [1, 2, 3, 4, 5]).
[2,4,6,8,10]
```

And functions can return functions since they are values too.

## 1.12:  Erlang data in practice

* Had to practice the following statements in the shell and guess the results before:

```erlang
not true.
true and false.
length([2,3,4,5]).
[2,3]++[[4]].
(fun (X) -> X+2 end)(40).
fun (X) -> X+2 end(40).
2#100.
100#2.
34#2.
2#34.
[97,98,99].
[$h,$e,$l,$l,$o].
[$h,$e,$l,$l,o].
[34,3,5,36,37].
[34,35,36,37].
false and (3 == (4 div 0)).
false andalso (3 == (4 div 0)).
true orelse (3 == (4 div 0)).
```

I personally was surprised with:

 - `[34,3,5,36,37].` (Thought 3 and 5 will be interpreted as ascii EXT and ENQ but turned it into an integer list because they are not ascii representable).
 - `false andalso (3 == (4 div 0)).` (Thought `andalso` behaved like `and`, turns that `and`/`or` evaluate both sides of the operator but `andalso`/`orelse` are short-circuit operators which will only evaluate the right-side argument if it needs to).
 
* Some nice links that came from discussions are [8.14 Short-Circuit Expressions](https://erlang.org/doc/reference_manual/expressions.html) and https://medium.com/erlang-battleground/there-are-guards-and-guards-71e67d4975d7.

## 1.13: Variables in Erlang
