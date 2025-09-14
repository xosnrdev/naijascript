# String concatenation - Python  
# Tests Python string concatenation

result = ""

for i in range(10000):
    result += f"item{i}|"

print(len(result))