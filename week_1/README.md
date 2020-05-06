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

* Strings are simply list of characters, both `"abc".` and `[97, 98, 99].` print as "abc". Useful to look at the ASCII table http://www.asciitable.com/index/asciifull.gif or we can prefix the character with $, like `[$a, $b, $c]`.

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
