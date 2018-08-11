package Fibonacci

import std::fmt::println

def fib(x: int): int {
    if (x <= 1) {
        return 1
    } else {
        return fib(x - 1) + fib(x - 2)
    }
}

def main(): int {
    for (let i = 0, i <= 10, i = i + 1) {
        println(fib(i))
    }

    return 0
}