# Prime checking - Python
# Tests conditional logic and mathematical operations

def is_prime(n):
    if n < 2:
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False
    
    i = 3
    while i * i <= n:
        if n % i == 0:
            return False
        i += 2
    return True

# Count primes up to 10000
count = 0
for i in range(2, 10001):
    if is_prime(i):
        count += 1

print(count)