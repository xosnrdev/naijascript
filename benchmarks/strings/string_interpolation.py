# String interpolation - Python
# Tests f-string performance

def generate_message(name, age, score):
    return f"Hello {name}, you are {age} years old and scored {score} points!"

# Generate many interpolated strings
total_length = 0

for i in range(50000):
    msg = generate_message("Alice", i, i * 2)
    total_length += len(msg)

print(total_length)