import "mul.cls"
import "cmp.cls"
import "square.cls"
print("Hello! What is your name?")
name = input
print("Hi " ++ name)
print("What can I do for you?")
print("1. Multiply two numbers")
print("2. Compare two strings")
print("3. Make a square")
iStr = input
i = toInt(iStr)
if(i == 1) { mul() } else { if(i == 2) { cmp() } else { square() } }
