// Mathematical built-ins - JavaScript
// Tests native Math object functions

let sum = 0;

for (let i = 1; i <= 10000; i++) {
    const sqrtVal = Math.sqrt(i);
    const absVal = Math.abs(sqrtVal - 50);
    const floorVal = Math.floor(absVal);
    const ceilVal = Math.ceil(absVal);
    sum += floorVal + ceilVal;
}

console.log(sum);