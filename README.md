# Haskell2

## Run REPL

Install the dependencies by running `cabal install` in the project directory, then start the repl using `cabal run`. The repl should now be open.

## Syntax

You can declare variables by
``` 
a = 1
name = "John Doe"
```

Arithmetic Expressions
```
1 + 2
2 ^ 2
19 % 2
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
