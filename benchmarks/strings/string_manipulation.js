// String manipulation - JavaScript
// Tests native string methods

const text = "The Quick Brown Fox Jumps Over The Lazy Dog";
let count = 0;

for (let i = 0; i < 10000; i++) {
    const upperText = text.toUpperCase();
    const lowerText = text.toLowerCase();
    const pos = lowerText.indexOf("fox");
    const sliced = upperText.slice(0, 10);
    count += sliced.length + pos;
}

console.log(count);