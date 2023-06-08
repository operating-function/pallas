Sire Spec
=========

This is my first attempt at a specification for Sire.

This specification is "forward looking", in that it specifies how Sire
*will* work, not exactly how it works now.

This is written in a fairly high-level, using somewhat hand-wavy notation.
It tries to specify all behavior without getting bogged down in details.

This document is intended to contain enough detail to write a correct
implementation.  But it is likely not quite there yet.


State State
-----------

Sire is a state machine, with a transition function that takes in
Rex trees:

    EXEC     : SireState -> Rex -> SireState
    EVALUATE : SireState -> Rex -> PlanValue
    EXPAND   : SireState -> PlanValue -> Rex -> (SireState, Rex)
    READ     : SireState -> Rex -> (SireState, AST)
    COMPILE  : AST -> PlanValue

    record SireState {
        nextKey : Nat                     --  The name of the current module.
        current : Str                     --  The name of the current module.
        scope   : Tab Str Bind            --  Current global namespace.
        keyed   : Tab Nat Bind            --  All bindings by key.
        modules : Tab Str (Tab Str Bind)  --  Loaded modules.
    }

    record Bind {
        value  : PlanValue          --  The value of the binder
        module : Str                --  Where was this defined?
        name   : Str                --  What name was this defined under?
        key    : Nat                --  The binding-key of the binder.
        Props  : Tab Nat PlanValue  --  Arbitrary properties, set by macros.
    }

    initialState : SireState = {
        nextKey = 0
        current = ""
        scope   = {}
        keyed   = {}
        modules = {}
    }


Pseudo Code
-----------

A `SireState` values is threaded throughout.  But this uses imperative
notation for brevity, omitting the details of the state threading.

All of the logic below is duplicated with the # prefix.  For example,
the rule for the `#-` rune is the same as the rule for the `-` rune.
This allows macros to replace built-in runes without making the primitive
version inaccessible.


### Macro Expansion

    EXPAND : SireState -> PlanValue -> Rex -> (SireState, Rex)

    EXPAND(st, macro, rex) := 

        let crash(badSyntax, errorMessage) := * Print pretty error and die. *

        let v2 st rex := return (st, rex);

        return APPLY(macro, st, rex, crash, v2));


### Executing Commands

    EXEC : SireState -> Rex -> SireState

    EXEC(st, expr) := 

        -- Macros expansion happens *before* parsing at every step.
        -- Macros can shaddow built-in syntax.

        if exists st.scope[expr.rune] then

            macro = st.scope[expr.rune];
            expr   = EXPAND(macro, expr);
            EXEC(expr);

        else case expr of

            --  Expressions at the top level are short-hand for (_ = expr);

            (| ...)..   ==>  EXEC(= _ $expr);
            (- ...)..   ==>  EXEC(= _ $expr);
            (** ...)..  ==>  EXEC(= _ $expr);
            (@ ...)..   ==>  EXEC(= _ $expr);
            (@@ ...)..  ==>  EXEC(= _ $expr);
            (^  ...)..  ==>  EXEC(= _ $expr);
            (&  ...)..  ==>  EXEC(= _ $expr);
            (?  ...)..  ==>  EXEC(= _ $expr);
            (?? ...)..  ==>  EXEC(= _ $expr);


            --  * executes multiple commands within one block.

            (*)           ==> ;
            (* ...)(* ..) ==> EXEC(* ... ..);
            (* cmd ..)    ==> EXEC(cmd); EXEC(* ..);


            --  ^-^ enters a new module (archiving the last one)

            (### foo) ==>

                assert (st.current == "");
                st.current = "foo";

            (### foo <- bar) ==>

                assert (st.current == "bar");
                st.modules["bar"] = st.scope;
                st.scope          = {};
                st.current        = "foo";


            --  ^-^ filters the current scope.

            (^-^ syms..)(^-^ more..).. ==>

                Sire((^-^ syms.. more..)..)

            (^-^ syms..) ==>

                st.scope = st.scope `intersect {syms..}


            --  !!= is a simple "assert equals"

            (!!= x y)(!!= ...).. ==>

                (* (!!= x y) (!!= ...)..)

            (!!= x y) ==>

                xv = EVALUATE(x);
                yv = EVALUATE(y);
                assert (xv == yv);


            --   < does a deep-print of a value.

            (< x) ==>

                xv = EVALUATE(x);
                PRINT_RECURSIVE(x);


            --   /+ pulls in symbols from earlier modules

            (/+ args...)(/+ ...).. ==>

                Sire(* (/+ args...) (/+ ...)..);

            (/+ foo) ==>

                st.scope = st.scope `union` st.modules["foo"]

            (/+ foo [syms..]) ==>

                st.scope = st.scope `union`
                               ( st.modules["foo"] `intersect` {syms..} )


            --  = creates a new binding.

            (= ....)(= ...).. ==>

                EXEC(* (= ....) (= ...)...);

            (= var value) ==>

                key = st.nextKey;
                st.nextKey = key + 1;
                EXEC(= key var value);

            (= key var value) ==>

                valu = EVALUATE(value);
                bind = Bind { value st.module var key {} };
                st.keyed[key] = bind;
                st.scope[key] = bind;


### Evaluation


    EVALUATE : SireState -> Rex -> Any

    EVALUATE(st, expr) := 

        ast = READ(expr);
        COMPILE(ast);


### Reading

    Read : SireState -> Rex -> Ast

    READ(st, expr)

        -- Macros expansion happens *before* parsing at every step.
        -- Macros can shaddow built-in syntax.

        if exists st.scope[exp.rune] then

            macro = st.scope[exp.rune];
            exp   = EXPAND(macro, exp);
            READ(exp);

        else case expr of

            --  | is function application

            (| x)         ==> READ(x)
            (| x xs..)..  ==> READ(| x xs.. ..)
            (| x y zs..)  ==>
                exprs = map READ (x :: y :: zs..)
                APPLY(exprs..)


            --  - means exactly the same things as |

            (- x)         ==> READ(x)
            (- x xs..)..  ==> READ(| x xs.. ..)
            (- x y zs..)  ==>
                exprs = map READ (x :: y :: zs..)
                APPLY(exprs..)


            --  ** marks a function to be inlined.

            (** x) ==> INLINE(READ(x))


            --  @ defines a let binding.

            (@ n v)b   ==>  READ(@ n v b)
            (@ n v b)  ==>
                nk = READ_KEY(n)
                vx = READ(v)
                bx = READ(b)
                LET(nk, vx, vb)


            --  @@ is the same as @ except that bindings may self-reference.

            (@@ n v)b   ==>  READ(@@ n v b)
            (@@ n v b)  ==>
                nk = READ_KEY(n)
                vx = READ(v)
                bx = READ(b)
                LETREC(nk, vx, vb)


            --  ^ is just a convenience wrapper around @.

            (^  ...)..    ==>  READ(^ ... ..)
            (^ x xs.. v)  ==>  READ(@ {_} v (x xs...))


            --  & defines an anonymous function (no self-reference possible)

            (& ..)body         ==>  (& .. body)
            (& arg body)       ==>  (& (arg) body)
            (& (arg ..) body)  ==>

                args = map(READ_KEY, args)
                body = READ(body)
                FUNC(tag=0, self=NONE, pin=No, inline=No, arg, body}


            --  ? defines a named function (that may self-reference)

            (? ..)body                   ==>  (? .. body)
            (? fun body)                 ==>  (? "fun" (fun) body)
            (? (fun ..) body)            ==>  (? "fun" (fun ..) body)
            (? (**fun ..) body)          ==>  (? "fun" (**fun ..) body)
            (? tag (fun args..) body)    ==>
            (? tag (**fun args..) body)  ==>

                inline = {{ Is the ** annotation present? }}
                self   = SOME(READ_KEY(fun))
                ags    = map READ_KEY args
                body   = READ(body)
                tag    = READ_NAT(tag)
                FUNC(tag, self, pin=No, inline, arg, body}


            --  ?? is the same as ? execpt that the result is pinned.

            (?? ..)body                   ==>  (?? .. body)
            (?? fun body)                 ==>  (?? "fun" (fun) body)
            (?? (fun ..) body)            ==>  (?? "fun" (fun ..) body)
            (?? tag (fun args..) body)    ==>
            (?? tag (**fun args..) body)  ==>

                inline = {{ is the ** marker there? }}
                self   = SOME(READ_KEY(fun))
                args   = map READ_KEY args
                body   = READ(body)
                tag    = READ_NAT(tag)
                FUNC(tag, self, pin=Yes, inline, args, body)


### Compilation


    COMPILE : Ast -> PlanValue

    COMPILE(ast) :=

        1.  Replace all references to globals with inline constants.

        2.  Give every local binding a unique name (and update
            references to match).

        3.  Apply optimizations

            3a. Mark all always-inline functions.

                (ifNot a b c)  ==>  (**ifNot a b c)

            3b. Inline all marked, saturated function applications.

                (**IfNot a b c)  ==>  (if a c b)

            3d. Eliminate bindings to constant values (let a=3 in (a, a))

                let a = 3 in (a, a)  ==>  (3, 3)

            3d. Eliminate trivial rebindings (let a = b)

                let a = b in (a, b)  ==>  (b, b)

            3e. Eliminate unused bindings (let a = 3 in 4)

                let a = 3 in b  ==>  b

            3f. Eliminate single-use bindings (let a=3 in a)

                let a=3 in (a, b)  ==>  (3, b)

        4.  Lambda lift all nested functions.

            4a) If a function is slated for lifting, lift functions
                nested inside it first.
            4b) Give every binder fresh names.
            4c) Find free variables.
            4d) Add free variables to the argument list.
            4e) Rename self reference.
            4f) Bind (@ oldSelfRef (newSelfRef freeVars..))
            4g) Done

        5.  Apply the same set of optimizations again.

        6.  Compile to PLAN

            6a) At this point, there are no non-nested functions and no
                free variables.

            6b) Replace each variable reference with a number (1 + args +
                let stack depth)  (this corresponds with the PLAN
                reference system)

            6c) Traverse the ast and translate to the equivalent PLAN.
                Apply the following optimizations along the way:

                -   (APP (CNS x) (CNS y)) can be turned into (CNS (x y))
                    if the arity of x is greater than 1.

                -   Constants that are not valid code fragments do not
                    need to be (2 x) wrapped.
