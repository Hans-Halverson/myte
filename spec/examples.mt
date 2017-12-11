// Identity
let ident(x) = x

// Simple operations
let add(x) = x + 1
let sub(x) = x - 1
let mult(x) = x * 2
let div(x) = x / 2
let pow(x) = x ^ 2

// Variable number of arguments and expressions
let parab(x) = -x^2 + 1
let sum2(x,y) = x + y
let mean2(x,y) = (x + y) / 2
let four = 2 + 2

// Nested parentheses
let foo(x) = ((x + 2) * 3) + 4

// Built in functions
let log2(x) = ln(x) / ln(2)
let trig1(x) = cos(x)
let trig2(x) = sin(x)
let trig3(x) = tan(x)
let trig4(x) = sec(x)
let trig5(x) = csc(x)
let trig6(x) = cot(x)
let atrig1(x) = acos(x)
let atrig2(x) = asin(x)
let atrig3(x) = atan(x)

// Function composition
let ident2(x) = add(sub(x)) + 0

// Expression evaluation
(3 * 5) + add(5)

// Function operations
max parab
min parab
argmax parab
argmin parab

TODO: Calculus function operations
derivative of parab at x=2
integral from x=0 to x=2 of param

TODO: Function operations that return an expression
derivative parab
integral parab
