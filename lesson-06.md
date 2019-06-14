# Lists

- identify parts of a list
- how to build lists
- understand role of lists in FP
- use common functions on a list
- basics of lazy evaluation

## Anatomy

Lists are inherently recursive: a list is either an empty list or an
element followed by another list.

```
Prelude> head [1,2,3]
1
Prelude> head [[1,2], [3,4], [5,6]]
[1,2]
Prelude> tail [1,2,3]
[2,3]
Prelude> tail [3]
[]
```

What if we call `head` or `tail` on an empty list?

```
Prelude> head []
*** Exception: Prelude.head: empty list
Prelude> tail []
*** Exception: Prelude.tail: empty list
```

To build a list we use the `cons` operator

```
Prelude> 1:[]
[1]
Prelude> 1:2:3:4:[]
[1,2,3,4]
```

Note that also strings are actually lists

```
Prelude> 'h':'e':'l':'l':'o':[]
"hello"
Prelude> "he" ++ "llo"
"hello"
```

We can generate a list of numbers like this

```
Prelude> [1..10]
[1,2,3,4,5,6,7,8,9,10]
```

And we can also generate an infinite list!

```
Prelude> infinite = [1..]
Prelude> take 10 infinite
[1,2,3,4,5,6,7,8,9,10]
```

As you can see, the code is being lazily evaluated, that is: the infinite
list is generated only up to the point that we need.

## Common functions

### !!

```
Prelude> [1,2,3] !! 0
1
Prelude> [1,2,3] !! 10
*** Exception: Prelude.!!: index too large
```

### length

```
Prelude> length [1..20]
20
Prelude> length "quicksand"
9
```

### reverse

```
Prelude> reverse [1,2,3]
[3,2,1]
Prelude> reverse "cheese"
"eseehc"
```

### elem

```
Prelude> elem 12 [0..15]
True
Prelude> elem 'p' "cheese"
False
```

### take and drop

```
Prelude> take 5 [1..20]
[1,2,3,4,5]
Prelude> take 3 "wonderful"
"won"
Prelude> drop 5 [1..10]
[6,7,8,9,10]
Prelude> drop 3 "gesundeit"
"undeit"
```

### zip

```
Prelude> zip [1,2,3] [2,4,6]
[(1,2),(2,4),(3,6)]

Prelude> zip "dog" "rabbit"
[('d','r'),('o','a'),('g','b')]

Prelude> zip ['a'..'f'] [1..]
[('a',1),('b',2),('c',3),('d',4),('e',5),('f',6)]
```

### cycle

```
Prelude> take 10 $ cycle [1,2,3]
[1,2,3,1,2,3,1,2,3,1]
```

Let's say we want to assign people to groups

```
Prelude> zip (cycle [1..3]) ["Ju", "Joan", "Jack", "Judas", "Jerry"]
[(1,"Ju"),(2,"Joan"),(3,"Jack"),(1,"Judas"),(2,"Jerry")]
```

### repeat

```
Prelude> take 10 $ repeat 'h'
"hhhhhhhhhh"
```
