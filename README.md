# Haskell2

## Run REPL

Install the dependencies and start the repl using `cabal run`. The repl should now be open.

## Syntax

You can declare variables by
``` 
a = 1
name = "John Doe"
```

Arithmetic Expressions
```
1 + 2     - Addition
2 ^ 2     - Power
19 % 2    - Modulo
|-3.4|    - Absolute
print (1 * 2 + 10)
```

String operations
```
"This is " ++ "cool"
print("a" ++ "wesom" ++ "e")
```

Conditional Statements
```
num = 19
if(19%2 == 0) { print("Even") } else { print("Odd") }
```

Run loops using boolean expression
``` 
i = 1
while (i <= 10) { print(i) i=i+1 }
```

Functions
```
fun sayHi() { print("hi!") }
fun sayHiTo(name) { print("hi " ++ name) }
fun double(x) { return x * 2 }
``` 

import (other) files
```
import "cool.cls"
```

## Testing
For verbose output of quickcheck tests, run `cabal repl quickcheck-tests`, then load the quickcheck module with `:l QuickCheckTests`.
Following this, you may run all the tests with `main` or `runTests`.
Run each test individually with `quickCheck prop_*`.
A full list of all property tests can be found by typing `prop_` and using the tab autocomplete feature.