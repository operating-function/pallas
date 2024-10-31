# R-Expressions

## Rex - R-Expressions

Rex is an alternative to Lisp's S-Expressions that provides a much more flexible syntax while still having a simple, uniform model.

S-Expressions have a structure that looks something like this:

```
name := [a-zA-Z0-9_$!#%&*+,./:<=>?@\\^`|~]+-]+

exp :=
   | name
   | ()
   | (exp)
   | (exp exp...)
```

For Example:

```
((+ 3) (+ 3 4))

(print (* 3 4))
```

Where R-Expression have a structure that looks something like:

```
rune := [$!#%&*+,./:<=>?@\\^`|~-]+
name := [a-zA-Z0-9_]+

exp :=
    | name
    | (rune)
    | (rune exp)
    | (rune exp exp...)
    | exp exp
```

Notice that every (nested expression) must start with a rune, and expressions may be juxtaposed:

```
(+ 3)(+ 4 5)

print(* 4 5)
```

The advantage is that this structure can be unambiguously encoded to text in a large variety of different ways. There are five different layouts, and they can be mixed freely.

*   Nested Layout:

    ```
    (= x 3)(| print)(| concat msg (| show (* x x)))
    ```
*   Open Layout (indentation sensitive):

    ```
    = x 3
    | print
    | concat msg
        | show
            * x x

    = x 3
    | print
    | concat msg | show * x x
    ```
*   Infix Layout:

    ```
    (x = 3)(| print)(concat | msg | (show | (x * x)))
    ```
*   Closed Layouts: (nested and |prefix)

    ```
    (x=3)(print|(concat|msg)(show|(x*x)))
    ```
*   Or any combination:

    ```
    = x 3
    | print
    | concat msg (| show x*x)
    ```

There is also some syntactic sugar.

*   (no-rune nested expressions) default to `|` rune.

    ```
    = x 3
    | print
    (concat msg (show x*x))
    ```
*   You can write multiple runes within a () form (which work the same was as open-form:

    ```
    (x=3)(|print)(| concat msg | show x*x)
    ```

All of these different forms parse into the exact same R-expression structure.

This gives a much more expressive syntax, but still gives you all of the advantages of s-expressions:

* Very simple parser and printer.
* Generic parser and pretty-printer that can be shared between languages.
* Enables traditional Lisp meta-programming (defmacro + syntax-rules).

#### An Example

In R-Expressions:

```
= (tabToPairs tab)
@ ks | listFromRow tabKeys-tab
@ vs | listFromRow tab
| listToRow (listZip ks vs)
```

In S-Expressions:

```
(define (tab-to-pairs tab)
 (let ((ks (list-from-row (tab-keys tab)))
       (vs (list-from-row tab)))
  (list-to-row (list-zip ks vs))))
```

## The Rex Grammar

```
namechar = [a-zA-Z_]
runechar = [$!#%&*+,-./:<=>?@\\^`|~]

/"""(.*)/     -> P # Page
/'''(.*)/     -> P # Page
/;(.*)/       -> C # Comment
/(namechar+)/ -> N # Name
/(runechar+)/ -> R # Rune
/( +)/        -> S # Whitespace
/'([^']*)'/   -> T # Text
/"([^"]*)"/   -> T # Text

leaf = P | T | N
shut = (nest | leaf)+
shin = shut (R shut)*
form = R shin | shin
frag = R shin | R | shin

# In infix mode, always occurs after `form`.
plix = ')'
     | R S form plix
     | S ')'
     | S R S form plix
     | S form plix

# In prefix mode, always occurs after `R` or `form`.
pree = ')'
     | S ')'
     | S R pree
     | S form pree

# Body of parenthesis, decides if infix or prefix
prest = ')'
      | form plix
      | R pree

# Body of [bracket expression]
broke = ']'
      | S ']'
      | S frag broke

# Body of {curly expression}
carl = '}'
     | S '}'
     | S frag carl

# [Any] (nested) {expression}
nest = '(' prest
     | '(' S prest
     | '[' broke
     | '[' frag broke
     | '{' carl
     | '{' frag carl

# Open space (can include line-comment)
open = C
     | S C
     | S

loan = EOF
     | open EOF
     | open frag loan

lean = EOF
     | frag loan

line = open lean
     | lean
```

##
