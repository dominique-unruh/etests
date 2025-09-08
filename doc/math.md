# Dealing with math in the grader

First, consider parsing the math using STACK parser, see [stack.md](stack.md).

There are two datatypes for math:
* StackMath -- Math terms represented in Scala. (Has nothing to do with STACK despite the name.) 
  Best for doing programmatic operations on the terms
* SympyExpr -- Math represented as python object (using sympy).
  Allows to use builtin math features from sympy in Python.

You can translate a StackMath term `math` into Sympy by doing `math.toSympyMC(...)`.
This will take into consideration the current math context (see below).


## Math context

You can define a math context with default options for various mathematical operations.
Like this:
```scala 3
given MathContext = MathContext.default
  .setOption.setOtherOptions...
```
Here `.setOption` etc. stands for various configuration options.
The math context from the current scope will be automatically used by various operations.

Important operations:
* `.fixValue("a", x)` -- automatically substitutes variable a by term x in many operations
* `.testValues("a", 1, 2, 3)` -- uses 1,2,3 as test values to insert for variable a in operations
  that do tests with different values (e.g., `checkEqualityNew`)

## Misc math operations

* Compare two sympy values: `a.algebraicEqual(b)`
* Compare two StackMath values using sympy: `a.toSympy().algebraicEqual(b.toSympy())`
* Compare two StackMath using test values for all the variables (test values are configured in the math context):
  `checkEqualityNew(a, b)`.
* Apply python code to a StackMath value `x` (this means that the python code is evaluated when the StackMath value is converted to SympyExpr):
  * Create a `SympyOperator` `op` that performs the desired operation (that class can contain an arbitrary function operating on SympyExpr's)
  * Use `val y = Sympy(op, x)`. Then `y.toSympy()` would, e.g., first transform `x` to a SympyExpr and then apply `op` to it.
* `enumerateMapped`, `forallMapped`, `countMapped`:
  Various operations that use the test values configured by the math context.