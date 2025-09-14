// String interpolation - JavaScript
// Tests template literal performance

function generateMessage(name, age, score) {
    return `Hello ${name}, you are ${age} years old and scored ${score} points!`;
}

// Generate many interpolated strings
let totalLength = 0;

for (let i = 0; i < 50000; i++) {
    const msg = generateMessage("Alice", i, i * 2);
    totalLength += msg.length;
}

console.log(totalLength);