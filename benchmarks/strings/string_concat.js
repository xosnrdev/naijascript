// String concatenation - JavaScript
// Tests V8 string optimization

let result = "";

for (let i = 0; i < 10000; i++) {
    result += "item" + i + "|";
}

console.log(result.length);