// Fibonacci iterative - JavaScript
// Tests loop performance and arithmetic operations

function fibIterative(n) {
    if (n < 2) {
        return n;
    } else {
        let a = 0;
        let b = 1;
        for (let i = 2; i <= n; i++) {
            const temp = a + b;
            a = b;
            b = temp;
        }
        return b;
    }
}

// Test with n=50 (avoiding precision issues while being computationally intensive)
const result = fibIterative(50);
console.log(result);