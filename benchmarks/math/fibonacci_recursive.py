# Fibonacci recursive - Python
# Tests function call overhead and recursion performance

def fib(n):
    if n < 2:
        return n
    else:
        return fib(n - 1) + fib(n - 2)

# Test with n=30 (reasonable for tree-walk interpreter)
result = fib(30)
print(result)