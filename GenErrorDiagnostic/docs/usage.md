# SHerrLoc Constraint Language (SCL)

## Overview
SHerrLoc is a general type checker, type inference algorithm, and error localizer over static program properties, including inferred types and information flow restrictions. Given a set of constraints in SCL, SHerrLoc returns a map of inferred types when satisfiable and the most likely sources of error otherwise. For SHerrLoc to be as generalizable as possible, SCL is a highly expressive constraint language, which explains its complexities.

## Example

For a general understanding of SHerrLoc and SCL, see the following example in OCaml. 

```
let div a b =
  if b = 0 then
    ()
  else
    Some (a / b)
  in div 1 2 = Some 0
```

While the error is on line three where the programmer returns `()` instead of `None`, the OCaml compiler reports an error on line 5 instead of line 3, when it first  detects an error while typechecking.

```
5 |     Some (a / b)
        ^^^^
Error: This variant expression is expected to have type unit
       There is no constructor Some within type unit
```

However, SHErrLoc will cite line 3 as the error, seeing `()` disagree in type with `Some (a / b)
`, while the function is used as if returning an option. Since `()` disagrees with the other return type which agrees with the functions usage, SHErrLoc can determine that the more likely cause of error is `()`, rather than `Some (a / b)`. To get this result, the OCaml type constraints need to be translated into SCL. This is done by a modification to the OCaml compiler, EasyOCaml. This modification returns these SCL constraints:

<details>
<summary>Click to view SCL constraints</summary>

```
CONSTRUCTOR array 1
CONSTRUCTOR char 0
CONSTRUCTOR bool 0
CONSTRUCTOR float 0
CONSTRUCTOR exn 0
CONSTRUCTOR format6 6
CONSTRUCTOR int 0
CONSTRUCTOR Pervasives.in_channel 0
CONSTRUCTOR Pervasives.fpclass 0
CONSTRUCTOR int64 0
CONSTRUCTOR list 1
CONSTRUCTOR lazy_t 1
CONSTRUCTOR option 1
CONSTRUCTOR Pervasives.open_flag 0
CONSTRUCTOR Pervasives.ref 1
CONSTRUCTOR unit 0
CONSTRUCTOR string 0
CONSTRUCTOR Pervasives.out_channel 0
CONSTRUCTOR nativeint 0
CONSTRUCTOR int32 0

u6["div":1,4-7] == v6["function a -> function b -> if . then . else .":];[1,0-6,21]
f5["div":1,4-7] == g5["function a -> function b -> if . then . else .":];[1,0-6,21]
e5["let . in .":] == j6["div 1 2 = Some 0":6,5-21];[1,0-6,21]
y6["a":1,8-9] == a7["":];[1,8-9]
w6["function b -> if . then . else .":] == x6["":];[1,8-9]
i5["function b -> if . then . else .":] == h6["":];[1,8-9]
h5["a":1,8-9] == i6["":];[1,8-9]
v6["function a -> function b -> if . then . else .":] == (y6 ["a":1,8-9] -> w6 )["function a -> function b -> if . then . else .":];[1,8-5,16]
g5["function a -> function b -> if . then . else .":] == (h5 ["a":1,8-9] -> i5 )["function a -> function b -> if . then . else .":];[1,8-5,16]
d7["b":1,10-11] == e7["":];[1,10-11]
b7["if . then . else .":] == c7["":];[1,10-11]
k5["if . then . else .":] == f6["":];[1,10-11]
j5["b":1,10-11] == g6["":];[1,10-11]
w6["function b -> if . then . else .":] == (d7 ["b":1,10-11] -> b7 )["function b -> if . then . else .":];[1,10-5,16]
i5["function b -> if . then . else .":] == (j5 ["b":1,10-11] -> k5 )["function b -> if . then . else .":];[1,10-5,16]
h7["b = 0":2,5-10] == bool["b = 0":2,2-5,16];[2,2-5,16]
g7["()":3,4-6] == b7["if . then . else .":];[2,2-5,16]
f7["Some a / b":5,9-16] == b7["if . then . else .":];[2,2-5,16]
u5["Some a / b":5,9-16] == k5["if . then . else .":];[2,2-5,16]
t5["()":3,4-6] == k5["if . then . else .":];[2,2-5,16]
l5["b = 0":2,5-10] == bool["b = 0":2,5-10];[2,2-5,16]
i7["b":2,5-6] == d7["":1,10-11];[2,5-6]
r5["b":2,5-6] == j5["":1,10-11];[2,5-6]
k7["(=)":2,7-8] == (j7 ["b":2,5-6] -> l7 ["(=) b":2,5-8])["":];[2,5-8]
i7["b":2,5-6] == j7["b":2,5-6];[2,5-8]
r5["b":2,5-6] == o5["b":2,5-6];[2,5-8]
p5["(=)":2,7-8] == (o5 ["b":2,5-6] -> n5 ["(=) b":2,5-8])["":];[2,5-8]
m7["0":2,9-10] == n7["0":2,9-10];[2,5-10]
l7["(=) b":2,5-8] == (n7 ["0":2,9-10] -> h7 ["b = 0":2,5-10])["":];[2,5-10]
s5["0":2,9-10] == m5["0":2,9-10];[2,5-10]
n5["(=) b":2,5-8] == (m5 ["0":2,9-10] -> l5 ["b = 0":2,5-10])["":];[2,5-10]
(o7  -> (o7  -> bool ["":2,7-8]) )["":] == k7["(=)":2,7-8];[2,7-8]
(q5  -> (q5  -> bool ["":2,7-8]) )["":] == p5["(=)":2,7-8];[2,7-8]
int["0":2,9-10] == m7["0":2,9-10];[2,9-10]
int["0":2,9-10] == s5["0":2,9-10];[2,9-10]
g7["()":3,4-6] == unit["()":3,4-6];[3,4-6]
t5["()":3,4-6] == unit["()":3,4-6];[3,4-6]
p7["Some a / b":5,9-16] == q7["a / b":5,10-15];[5,9-16]
f7["Some a / b":5,9-16] == (option p7)["Some a / b":5,9-16];[5,9-16]
e6["Some a / b":5,9-16] == w5["a / b":5,10-15];[5,9-16]
u5["Some a / b":5,9-16] == (option e6)["Some a / b":5,9-16];[5,9-16]
r7["a":5,10-11] == y6["":1,8-9];[5,10-11]
c6["a":5,10-11] == h5["":1,8-9];[5,10-11]
t7["(/)":5,12-13] == (s7 ["a":5,10-11] -> u7 ["(/) a":5,10-13])["":];[5,10-13]
r7["a":5,10-11] == s7["a":5,10-11];[5,10-13]
c6["a":5,10-11] == a6["a":5,10-11];[5,10-13]
b6["(/)":5,12-13] == (a6 ["a":5,10-11] -> y5 ["(/) a":5,10-13])["":];[5,10-13]
v7["b":5,14-15] == w7["b":5,14-15];[5,10-15]
u7["(/) a":5,10-13] == (w7 ["b":5,14-15] -> q7 ["a / b":5,10-15])["":];[5,10-15]
d6["b":5,14-15] == x5["b":5,14-15];[5,10-15]
y5["(/) a":5,10-13] == (x5 ["b":5,14-15] -> w5 ["a / b":5,10-15])["":];[5,10-15]
(int ["":5,12-13] -> (int ["":5,12-13] -> int ["":5,12-13]) )["":] == t7["(/)":5,12-13];[5,12-13]
(int ["":5,12-13] -> (int ["":5,12-13] -> int ["":5,12-13]) )["":] == b6["(/)":5,12-13];[5,12-13]
v7["b":5,14-15] == d7["":1,10-11];[5,14-15]
d6["b":5,14-15] == j5["":1,10-11];[5,14-15]
u6["":1,4-7] == t6["div":6,5-8];[6,5-8]
x7["1":6,9-10] == s6["1":6,9-10];[6,5-10]
t6["div":6,5-8] == (s6 ["1":6,9-10] -> r6 ["div 1":6,5-10])["":];[6,5-10]
y7["2":6,11-12] == q6["2":6,11-12];[6,5-12]
r6["div 1":6,5-10] == (q6 ["2":6,11-12] -> p6 ["div 1 2":6,5-12])["":];[6,5-12]
p6["div 1 2":6,5-12] == m6["div 1 2":6,5-12];[6,5-14]
n6["(=)":6,13-14] == (m6 ["div 1 2":6,5-12] -> l6 ["(=) div 1 2":6,5-14])["":];[6,5-14]
a8["Some 0":6,20-21] == k6["Some 0":6,20-21];[6,5-21]
l6["(=) div 1 2":6,5-14] == (k6 ["Some 0":6,20-21] -> j6 ["div 1 2 = Some 0":6,5-21])["":];[6,5-21]
int["1":6,9-10] == x7["1":6,9-10];[6,9-10]
int["2":6,11-12] == y7["2":6,11-12];[6,11-12]
(o6  -> (o6  -> bool ["":6,13-14]) )["":] == n6["(=)":6,13-14];[6,13-14]
int["0":6,20-21] == c8["0":6,20-21];[6,20-21]
d8["Some 0":6,20-21] == c8["0":6,20-21];[6,20-21]
a8["Some 0":6,20-21] == (option d8)["Some 0":6,20-21];[6,20-21]
```

</details>

The first portion is a set of constructor definitions used in OCaml. For two examples,`CONSTRUCTOR int 0` defines `int` as a type constructor taking zero arguments (i.e., a base type), whereas `CONSTRUCTOR option 1` defines `option` as a type constructor taking in one type. 

The second portion defines the constraints for SHErrLoc to investigate. For example, consider 
the constraint `u6["div":1,4-7] == v6["function a -> function b -> if . then . else .":];[1,0-6,21]`. The left side defines a variable `u6` with position `["div":1,4-7]`. This means `u6` is a variable corresponding to the type of the code found on line `1` characters `4-7`, namely `div`. It equates this to the type `v6`, which corresponds to the type of the function of form`function a -> function b -> if . then . else .`. The final position `[1,0-6,21]` indicates this equality comes from lines `1-6`, characters `0-21`. Similarly, this constraint `v6
function b -> if . then . else .":];[1,8-5,16]` says that the above variable `v6` has type equal to `y6 -> w6`, where `y6` is a variable corresponding to `a`. Through these sorts of constraints, SHErrLoc will determine that the use of the type unit on line 3 is incorrect, being present in the unsatisfiable constraints while being less likely to be present in satisfiable constraints. 

Although this was a relatively simple example, where the programmer likely would have caught the error relatively easy, it is easy to see how this problem can get exacerbated as projects get larger. 

## Features & Syntax

The above example displays many features of SCL, but there are some more allowing for more expressive constraints. Beyond declarations and constraint equations, SCL also supports a set of global assumptions. This leads to two basic forms of SCL constraints. 

Without global assumptions (as above):

```
<declarations>

<constraints>
```

With global assumptions:

```
<declarations>

%%

<assumptions>

%%

<constraints>
```

### Declarations

Declarations declare variables, constructors, and functions to be used throughout the rest of the file. 

Variables are just type variables that are not assigned a given type. These are defined by only their name: 

`VARIABLE <name>`

Constructors and functions are more complicated, and analogous to type constructors and type functions. Both are defined by an identifier and an arity. Constructors are defined:

`CONSTRUCTOR <name> <arity>`

while functions are defined 

`FUNCTION <name> <arity>`

### Assumptions

In more complex languages, there are some assumptions in the type system that are important when checking satisfiability. For example, in Haskell, it is always true that `Int` is an instance of the type class `Real`. When a `Real` is needed, an `Int` can be provided. In SCL, this is encoded by the line

`(Int) <= (Real);`

A more complicated assumption in Haskell is that when two types are ordered, then the disjoint union of these two types can be ordered lexicographically. This leads to the assumption:

`axiom a , b . (a) <= (Ord); (b) <= (Ord); => ((Either a b)) <= (Ord);;`

Given types `a` and `b` that are both instances of the type class `Ord`, `Either a b` is also an instance of type class `Ord`. These two examples demonstrate the syntax of denoting these assumptions. An inequality is of the form `<type1> <REL> <type2>` where `<REL>` is one of `==`,`<=` or `>=`. To denote an assumption of just a single inequality, an inequality followed by a semicolon suffices, as in `<inequality>;`. Instead, an assumption could consist of a set of quantified variables, a set of inequalities acting as a premise, and a set of inequalities acting as the conclusion. In this case, the general form is 

`axiom <vars> . <premise> => <conclusion>;`

Here, `<vars>` is a comma-separated list of type variables (possibly with just one type variable)`<premise>` and `<conclusion>` are a semicolon separated list of inequalities (as above). It is also possible for the premise to be empty, in which the form `axiom <vars> . <conclusion>;` can be used.

### Constraints

The constraints are the most important part of an SCL input. Here, the relations between variables are encoded along with information about the source code to provide accurate error reports when needed. These constraints are generally of the form 

`<inequality> <env>; <position>`

The inequality is like above, except the types are augmented with their own `<position>`s. The`<env>` is a local set of assumptions, and `<position>` marks a location in the source code that may be useful in error reports. 

Positions, as described in the OCaml example, correspond to the location in source code where the type element or constraint is derived, so that when an unsatisfactory constraint is detected, there is something to point to. These are of the form 

`["<string>":<range>@<file>]`

The String should correspond to the represented source code, while the range should correspond to the lines and columns within the source code at file `<file>` which this position element represents. A single line range can be represented `l,c1-c2`, representing the line `l` between columns `c1` and `c2`, while a multiline range can be represneted `l1,c1-l2,c2` represent the code between lines `l1`to `l2` and columns `c1` and `c2`.

These positions should be attached to the elements in inequalities so that the inequalities are of the form:

`<type1><position1> <REL> <type2><position2>`

This allows SHErrLoc to be aware of the source of these types, so that if an error is found regarding some variables, that information can be used in the error report. 

Finally, the environment is of the form `{ <inequalities> }`, where the inequalities are the same as in the assumptions (where they can be empty), or it is omitted. 