# String manipulation - Python
# Tests native string methods

text = "The Quick Brown Fox Jumps Over The Lazy Dog"
count = 0

for i in range(10000):
    upper_text = text.upper()
    lower_text = text.lower()
    pos = lower_text.find("fox")
    sliced = upper_text[0:10]
    count += len(sliced) + pos

print(count)