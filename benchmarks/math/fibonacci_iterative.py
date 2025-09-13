# Fibonacci iterative - Python
# Tests loop performance and arithmetic operations

def fib_iterative(n):
    if n < 2:
        return n
    else:
        a = 0
        b = 1
        for i in range(2, n + 1):
            temp = a + b
            a = b
            b = temp
        return b

# Test with n=50 (avoiding precision issues while being computationally intensive)
result = fib_iterative(50)
print(result)