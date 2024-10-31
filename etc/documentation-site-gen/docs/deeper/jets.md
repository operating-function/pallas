# Jets

::::warning
TODO: More thorough explanation + examples.
::::

You may notice that the language contains very few operations, and doing everything with just increment/decrement is not very practical.

In practice, runtime systems need to implement more operations natively. Multiplication of natural numbers, for example, is prohibitively expensive if implemented directly on top of the PLAN primitives.

To solve this, PLAN adopts the "Jets" concept from Urbit.

Jets work by choosing a specific function, and making that "special". This one, for example, is special:

```
toNat=(4 (0 499848736628 1 (0 (2 0 3) 1)))
toNat=<{499848736628 1 (0 (2 0 3) 1)}>
```

When the runtime runs a special function, instead of running the legal code as usual, it runs a special built-in routine. The optimized version needs to have exactly the same behavior as the unoptimized version. This is is similar to the concept of an "Intrinsic Function"

At first brush, this may seem like an over-complicated and roundabout way to add new primitives. But remember that the perfect-forward-compatibility guarantee means that we can never change the shape of data. And also remember that functions are not opaque objects. So these details cannot be hidden away, adding a new primitive in the normal way, would be a breaking change that requires all data be migrated to a new format.

In theory, this system creates perfect compatibility between implementations. If an implementation doesn't know about a special function, then it will still arrive at the same answer.

However, in practice, an extremely slow programs is not so different from a program that doesn't work at all. So, changing or removing a special function is, in practice, a breaking change. We solve this by just never doing that. The set of special functions is specified in another document.

Furthermore, _adding_ new special functions can sometimes be a backward-compatibility breaking change. Specifically, if we add a new special function, and then write code that depends on that function being fast, then the resulting database will run very slowly on old implementations.

