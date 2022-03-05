# parchment
A statically typed functional programming language that compiles to ~~currently very slow~~ performant web assembly.

## note
Parchment is very much a work in progress. If you use it, expect it to be buggy, incomplete, slow and have a terrible UX. You have been warned.

## language features
Parchment is a relatively simple language but it will get more complex as it evolves.

### math
In general, math expressions are fine. 
`3 + 4 * 5` = `23`

### blocks, let and variables
```sml
{
    let x = 5;
    let y = x + 3;
    x * y - 4;
}
```
evaluates to `5 * (5 + 3) - 4` = `36`

we can also nest blocks
```sml
{
    let x = {
        let y = 5;
        y + 1;
    };
    x + 1;
}
```
evaluates to 7.

nested blocks support lexical scope, so
```sml
{
    let x = {
        let y = 5;
        y;
    };
    y;
```
is a compile time error.

If a block doesn't have a final expression then it returns a value of the unit type `()`.

### functions
Parchment only has anonymous functions. 
`fn x -> x + 1` is a function that takes a number and adds one. It has type `Num -> Num`.
`fn x -> x` is a generic function that has type `a -> a`.
`fn x y -> x` is not a function that takes two parameters, it instead takes a single parameter, then returns a second function that takes a single parameter and returns the original value. It has type `a -> (b -> a)`, or `a -> b -> a`

To call a function, just use a space:
```sml
{
    let add1 = fn x -> x + 1;
    add1 5;
}
```

### tuples
You can pack values into tuples pretty easily. `(1, 2)` has type `(Num, Num)`, `(true, 1, false)` has type `(Bool, Num, Bool)`

### records
Similar to tuples, it is easy to pack values into into records. `{a: 5, b: true}` has type `{a: Num, b: Bool}`

### pattern matching
To be able to do anything interesting with records and tuples, you need to be able to get values out of them as well as put values in. `fn (fst, snd) -> fst` is a function that takes a tuple containing two values and returns the first one. It has type `(a, b) -> a`. `fn {a:x, b:y} -> x + y` is a function that takes a record with two integer fields a and b, and adds them together. It has type `{a: Num, b: Num} -> Num`. You can also pattern match when binding with `let`:

```sml
{
    let (a, b) = (5, 6);
    a + b;
}
```

```sml
{
    let {a: x, b: y} = {a: 5, b: 6};
    x + y;
}
```
Behave as you would expect.
