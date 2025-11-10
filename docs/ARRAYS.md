# Arrays

NaijaScript arrays hold heterogeneous values and are written with square brackets.

## Literals

Declare arrays:

```naijascript
make foo get ["apple", "banana", "orange"]
make bar get [1, 2, 3, 4]
make mixed get ["text", 42, true]
make nested get [[1, 2], [3, 4]]
```

## Indexing and assignment

Access elements with zero-based indexes and assign by indexing:

```naijascript
make foo get ["a", "b", "c"]
make first get foo[0]    # "a"
foo[1] get "bee"         # foo becomes ["a", "bee", "c"]
```

Nested arrays use chained indexes:

```naijascript
make nested get [[1,2], [3,4]]
make val get nested[1][0]   # 3
nested[0][1] get 9          # nested becomes [[1,9], [3,4]]
```

## Methods

Available methods and return types:

| Method        | Description                                    | Returns  |
| ------------- | ---------------------------------------------- | -------- |
| `len()`       | Number of elements in the array                | `Number` |
| `push(value)` | Append value to the end of the array           | `Array`  |
| `pop()`       | Removes the last element of the array (if any) | `Array`  |
| `reverse()`   | Reverses the elements of the array in place    | `Array`  |

Note: `push`, `pop`, and `reverse` mutates the array and yields the array for chaining.
