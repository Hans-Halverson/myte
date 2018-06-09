package Fibonacci

import std::fmt::println

def fib(x) {
    if (x <= 1) {
        return 1
    } else {
        return fib(x - 1) + fib(x - 2)
    }
}

def main() {
    for (let i = 0, i <= 10, i = i + 1) {
        println(fib(i).toString())
    }
}