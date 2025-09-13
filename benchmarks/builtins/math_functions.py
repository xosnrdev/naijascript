# Mathematical built-ins - Python
# Tests native math module functions

import math

sum_total = 0

for i in range(1, 10001):
    sqrt_val = math.sqrt(i)
    abs_val = abs(sqrt_val - 50)
    floor_val = math.floor(abs_val)
    ceil_val = math.ceil(abs_val)
    sum_total += floor_val + ceil_val

print(sum_total)