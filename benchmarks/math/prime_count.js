// Prime checking - JavaScript
// Tests conditional logic and mathematical operations

function isPrime(n) {
    if (n < 2) {
        return false;
    }
    if (n === 2) {
        return true;
    }
    if (n % 2 === 0) {
        return false;
    }
    
    for (let i = 3; i * i <= n; i += 2) {
        if (n % i === 0) {
            return false;
        }
    }
    return true;
}

// Count primes up to 10000
let count = 0;
for (let i = 2; i <= 10000; i++) {
    if (isPrime(i)) {
        count++;
    }
}

console.log(count);