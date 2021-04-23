fun dashes(n) {str="" i=0 while(i!=n) {str=(str ++ "-") i=i+1} return str }
fun spaces(n) {str="" i=0 while(i!=n) {str=(str ++ " ") i=i+1} return str }
fun printBod(n) { i=0  s = spaces((n*2)-2) s=("|" ++ s ++"|") while(i!=(n - 2)) { print(s) i=i+1 } }
fun square(){ print("Enter side (greater than 2): ") side = input side = toInt(side) d = dashes(side*2) print(d) printBod(side) print(d) }
