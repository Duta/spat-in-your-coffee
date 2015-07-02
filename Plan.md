Plan
====

Initially, **no optimisation**.

Full pipeline:
--------------

```
Source code
-> parse ->
Parsed AST
-> rewrite stages ->
Core AST
-> typechecking ->
Typechecked AST
-> generate c ->
C files
```

Rewrite stages:
---------------

Not yet in any particular order.

 - Make all `this`s and `super`s explicit.
 - `for` in terms of `while`
 - `if` in terms of `if/else`
 - Check constructor names match class names.
 - 
