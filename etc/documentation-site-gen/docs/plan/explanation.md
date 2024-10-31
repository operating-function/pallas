# Explanation

## The E() Function

```
E(o:@)     = o
E(o:<x>)   = o
E(o:(f x)) =  
    E(f)      
    when A(f)=1 
        o <- X(o,o)
        E(o)       
    o              
E(o:{n a b}) =     
    if a!=0 then o else 
        o <- <>         
        o <- R(0,o,b) 
        E(o)
```

The `E()` function is responsible for evaluating an expression to its weak head normal form (WHNF). It takes a single parameter `o`, which represents the expression to be evaluated.

**Procedure:**

1. It checks the type of the input expression `o`:
   * If the type of `o` is `'hol'` (indicating a hole or an undefined value):
     * It raises an exception indicating a loop, as evaluating a hole is not allowed.
   * If the type of `o` is `'app'` (indicating an application):
     * It recursively calls `E()` on the head of the application (`o.head`) to evaluate the function to its WHNF.
     * It then checks if the arity of the evaluated function (`A(o.head)`) is equal to 1:
       * If the arity is 1, it means the function is saturated (has enough arguments).
       * It updates `o` by calling `S(o)` to simplify the expression by removing any unnecessary pins in the head of the application.
       * It then calls `X(o, o)` to perform one step of simplification on the saturated expression.
       * It updates `o` with the simplified expression.
       * It recursively calls `E()` on the updated `o` to continue the evaluation process.
2. Finally, it returns the evaluated expression `o`.

The `E()` function relies on the `A()` function to determine the arity of a function, the `S()` function to simplify the expression by removing unnecessary pins, and the `X()` function to perform the actual simplification step on a saturated expression.

The evaluation process continues recursively until the expression reaches its WHNF.

## The X() Function

```
X((f x), e)         = X(f,e)
X(<p>, e)           = X(p,e)
X({n a b}, e)       = R(a,e,b)
X(0, (_ n a b))     = {N(n) N(a) F(b)}
X(1, (_ p l a n x)) = P(p,l,a,n,E(x))
X(2, (_ z p x))     = C(z,p,N(x))
X(3, (_ x))         = N(x)+1
X(4, (_ x))         = <F(x)>
```

The `X()` function is responsible for performing one step of evaluation on a saturated expression.&#x20;

Inputs:

* `k`: The expression to be simplified.
* `e`: The environment.

**Procedure:**

* If the input is an app, pin, or law, `X()` applies the corresponding reduction rules based on the PLAN specification.
* If the input is a special operation, it executes the associated built-in routine.

There are four built-in operations, indicated by the first four natural numbers.

* `(0 n a b)`: Construct a new law: `{n a b}`
*   `(1 p l a n x)`: Pattern match on the value `x` (is it a pin, a law, an app, or a nat?).

    ```
    case x of
        PIN <i>     -> p i
        LAW {n,a,b} -> l n a b
        APP (f x)   -> a f x
        NAT x       -> n x
    ```
*   `(2 z p x)`: Pattern match on a natural number (is it zero or (n+1)?).

    ```
    case x of
        0 -> z
        x -> p (x-1)
    ```
* `(3 x)`: Increment a natural number.
* `(4 x)`: Normalize x and construct a new pin `<x>`.

More specifically:

* If `k` is 0:
  * It takes the second element of `e`, evaluates it using `F()`, and returns a new `'pin'` containing the evaluated value.
* If `k` is 1:
  * It extracts the name (`n`), arity (`a`), and body (`b`) from `e`.
  * It evaluates the name and arity using `N()` to convert them to natural numbers.
  * It returns a new `'law'` with the evaluated name, arity, and body (evaluated using `F()`).
* If `k` is 2:
  * It extracts the value `x` from `e`, evaluates it using `N()` to convert it to a `'nat'`, increments it by 1, and returns the result as a new `'nat'`.
* If `k` is 3:
  * It extracts the values `z`, `p`, and `x` from `e`.
  * It evaluates `x` using `N()` to convert it to a `'nat'`.
  * It calls the `C()` function with `z`, `p`, and the evaluated `x` to perform case matching on the `'nat'`.
* If `k` is 4:
  * It extracts the values `p`, `l`, `a`, `n`, and `x` from `e`.
  * It evaluates `x` using `E()` to reduce it to weak head normal form.
  * It calls the `P()` function with `p`, `l`, `a`, `n`, and the evaluated `x` to perform pattern matching on the PLAN value.

## The C() Function

```
C(z,p,n) = if n=0 then z else (p (n-1))
```

The `C()` function is responsible for pattern matching on natural numbers.

**Procedure:**

* If `n` is zero, it executes the action `z`.
* If `n` is greater than zero, it executes the action `p`, passing `n - 1` as the argument to `p`.
* If `n` does not match any of the specified patterns, the function does not perform any action.

## The P() Function

```
P(p,l,a,n,(f x))   = (a f x)
P(p,l,a,n,<x>)     = (p x)
P(p,l,a,n,{n a b}) = (l n a b)
P(p,l,a,n,x:@}     = (n x)
```

The `P()` function is responsible for pattern matching and executing the corresponding actions based on the type of the input expression.

**Inputs**:&#x20;

* `p`: The action to be executed if the input expression is a pin.
* `l`: The action to be executed if the input expression is a law.
* `a`: The action to be executed if the input expression is an app.
* `n`: The action to be executed if the input expression is a natural number.
* `o`: The input expression to be pattern matched.

**Procedure:**

ALL REMOVED FOR DOCUSAURUS
