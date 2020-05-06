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

## 1.10

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

