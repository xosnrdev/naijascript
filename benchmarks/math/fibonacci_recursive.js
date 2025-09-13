// Fibonacci recursive - JavaScript  
// Tests function call overhead and recursion performance

function fib(n) {
    if (n < 2) {
        return n;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}

// Test with n=30 (reasonable for tree-walk interpreter)
const result = fib(30);
console.log(result);