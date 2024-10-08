# This is a direct translation of PLAN_SPEC.txt to Python3.


### Representation #############################################################

def Hol():      return Val( ()      )
def Nat(i):     return Val( i       )
def Pin(item):  return Val( (item,) )
def App(f,x):   return Val( (f,x)   )
def Law(n,a,b): return Val( (n,a,b) )

class Val:
    __match_args__ = ("val",)

    def __init__(self, val):
        self.box = [val]

    # TODO: this works, but it doesn't seem quite right.
    # Why do I need to mutate both?
    def update(self, x):
        self.box[0] = x.box[0]
        self.box = x.box

    def getType(self):
        if type(self.val) == int: return 'nat'
        return ['hol', 'pin', 'app', 'law'][len(self.val)]

    def toList(self):
        x = []
        while (self.type == 'app'):
            x.append(self.tail)
            self = self.head
        x.append(self)
        x.reverse()
        return x

    item = property(fget=(lambda self: self.box[0][0]))
    name = property(fget=(lambda self: self.box[0][0]))
    args = property(fget=(lambda self: self.box[0][1]))
    body = property(fget=(lambda self: self.box[0][2]))
    head = property(fget=(lambda self: self.box[0][0]))
    tail = property(fget=(lambda self: self.box[0][1]))
    type = property(fget=getType)
    list = property(fget=toList)
    nat  = property(fget=(lambda self: self.val))
    val  = property(fget=(lambda self: self.box[0]))
    id   = property(fget=(lambda self: id(self.box)))

    def __eq__(self, other):
        return self.val == other.val

    def __repr__(self):
        t = self.type
        v = self.val
        if t == 'nat': return repr(v)
        if t == 'hol': return '<>'
        if t == 'pin': return f'<{v[0]}>'
        if t == 'law': return f'{{{v[0]} {v[1]} {v[2]}}}'
        if t == 'app': return f"({' '.join([repr(x) for x in self.list])})"


### Rules ######################################################################

# Index into a list (represented as App nodes)
def I(f, o, n):
    match (n, o.type):
        case (0, 'app'): return o.tail
        case (0, _):     return o
        case (_, 'app'): return I(f, o.head, n-1)
        case (_, _):     return f

# Arity of a value
def A(o):
    match o.type:
        case 'app': return A(o.head)-1
        case 'pin':
            if (o.item.type == 'nat'):
                return I(Nat(1), desugar((6,1,3,1)), o.item.nat).nat
            return A(o.item)
        case 'law': return o.args.nat
        case 'nat': return 0
        case 'hol': raise Exception("<<loop>>")

# Cast to nat
def N(o):
    E(o)
    return (o if o.type == 'nat' else Nat(0))

# Run a law body
def R(n,e,b):
    if b.type == 'nat' and b.nat <= n:
        return I(b, e, n - b.nat)

    match b.list:
        case [Val(0), f, x]: return App(R(n,e,f), R(n,e,x))
        case [Val(0), x]:    return x
        case _:              return b

def L(i,n,e,x):
    match x.list:
        case [Val(1), v, b]:
            I(999, e, n-i).update(R(n,e,v))
            return L(i+1,n,e,b)
        case _:
            return R(n,e,x)

def B(a,n,e,b,x):
    match x.list:
        case [Val(1), _, k]: return B(a, n+1, App(e, Hol()), b, k)
        case _:              return L(a+1,n,e,b)

# Pattern match on PLAN values
def C(p,l,a,z,m,o):
    match o.type:
        case 'app': return App(App(a,o.head), o.tail)
        case 'pin': return App(p, o.item)
        case 'law': return App(App(App(l, o.name), o.args), o.body)
        case 'nat': return z if o.nat==0 else App(m, Nat(o.nat - 1))

# Simplify a closure by removing useless pins in the head.
def S(o):
    if o.type == 'app':
        match o.head.type:
            case 'app': return App(S(o.head), o.tail)
            case 'pin':
                if o.head.item.type != 'law':
                    return S(App(o.head.item, o.tail))
    return o

# Execute one simplification step for a saturated expression
def X(k,e):
    match (k.type, k.nat):
        case ('app', _): return X(k.head, e)
        case ('pin', _): return X(k.item, e)
        case ('law', _): return B(k.args.nat, k.args.nat, e, k.body, k.body)
        case ('hol', _): raise Exception("<<loop>>")
        case (_, 0):
            (_,x) = e.list
            return Pin(F(x))
        case (_, 1):
            (_,n,a,b) = e.list;
            arity = N(a)
            if arity.nat == 0: raise Exception(("crash", e))
            return Law(N(n),arity,F(b))
        case (_, 2):
            (_,x) = e.list
            return Nat(N(x).nat + 1)
        case (_, 3):
            (_,p,l,a,z,m,o) = e.list
            return C(p,l,a,z,m,E(o))
        case _:
            raise Exception(("crash", e))

# Force a full evaluation to Normal-Form
def F(o):
    E(o)
    if o.type == 'app':
        F(o.head)
        F(o.tail)
    return o

# Evaluate to Weak-Head-Normal-Form
def E(o):
    match o.type:
        case 'hol': raise Exception("<<loop>>")
        case 'app':
            E(o.head)
            if A(o.head)==1:
                o.update(S(o))
                new = X(o,o)
              # print(f"        {o}  ==>  {new}")
                o.update(new)
                E(o)
    return o


### Running Some Examples ######################################################

def desugar(exp):
    if type(exp) == int:
        return Nat(exp)
    if type(exp) == tuple:
        l = len(exp)
        if l == 0: return Hol()
        if l == 1: return desugar(exp[0])
        return App(desugar(exp[:-1]), desugar(exp[-1]))
    return exp

def assertEq(self, other):
    t = self.type
    if t != other.type:
        raise Exception(("typeDiffers", self.type, other.type))
    if t == 'hol':
        raise Exception("result has loop")
    if t == 'nat' and self.nat != other.nat:
        raise Exception(("natDiffers", self.nat, other.nat))
    if t == 'pin':
        assertEq(self.item, other.item)
    if t == 'law':
        assertEq(self.name, other.name)
        assertEq(self.args, other.args)
        assertEq(self.body, other.body)
    if t == 'app':
        assertEq(self.head, other.head)
        assertEq(self.tail, other.tail)

def go(expected, testCase):
    expected = desugar(expected)
    testCase = desugar(testCase)
    print("assert", expected, "==", testCase)
    expect = F(expected)
    result = F(testCase)
    try:
        assertEq(expect, result)
    except Exception as e:
        print("FAILED", expect, "!=", result)
        raise e

def Op(x): return Pin(Nat(x))

MkPin=Op(0)
MkLaw=Op(1)
Inc=Op(2)
Case=Op(3) # NatCase PlanCase

# inc(4)
# 5
go(5,
   (Inc, 4))

# inc(1 9)
# inc(0)
# 1
go(1,
   (Inc, (1, 9)))

#  (##3 1 0 0 0 0 (##0 9))
#  (1,9)
go((1,9),
   (Case, 1, 0, 0, 0, 0, (MkPin, 9)))

#  inc-(##3 1 0 0 0 0 (##0 9)) => inc(1 9) = inc(0)  ==>  1
go(1,
   (Inc, (Case, 1, 0, 0, 0, 0, (MkPin, 9))))

#  ##1 1 2 (0 (##2 7)) 3 4
#  ##1 1 2 (0 8) 3 4
#  (_ _ & 8) 3 4
#  8
go(8,
   ((MkLaw,1,2,(0,(Inc,7))), 3, 4))

#  ##1 1 2 (0 ##2 7) 3 4
#  ##1 1 2 (0 8) 3 4
#  (_ _ & Inc 1 7) 3 4
#  Inc 1 7
#  8
go(8,
   ((MkLaw,1,2,(0,Inc,7)), 3, 4))

go( (MkLaw,1,2,0), ((MkLaw,1,2,0), 9, 7 ))

go( 9, ((MkLaw,1,2,1), 9, 7 ))
go( 7, ((MkLaw,1,2,2), 9, 7 ))
go( 3, ((MkLaw,1,2,3), 9, 7 ))

# pins
go( (MkPin,(0,1),2,3),     ((MkPin,(0,1)), 2, 3)                 )
go( (MkLaw,1,2,0),         ((MkPin,1), 1, 2, 0)                  )
go( (MkLaw,1,2,0),         ((MkPin,(MkLaw,1)), 2, 0)             )
go( (MkPin,(MkLaw,1,2,0)), ((MkPin,(MkLaw,1,2,0)), 3, 4)         )
go( (MkPin,(MkLaw,1,2,0)), ((MkPin,(MkPin,(MkLaw,1,2,0))), 3, 4) )

# let bindings
go( 9, ((MkLaw,0,1,1),          9 )) # ? ($0 $1) | 9
go( 9, ((MkLaw,0,1, (1, 1, 2)), 9 )) # ? ($0 $1) @ $2=$1 | $2

# refer to later binder from an earlier one.
go( 9,
    (MkLaw,0,1, # ? ($0 $1)
      (1, 3,    # @ $2 = $3
      (1, 9,    # @ $3 = 9
       2)),     # $2
    9))

# more complex example
go( (1,(0,2)),         # =?= (1 (0 2))
    (MkLaw,0,1,        #   | ? ($0 $1)
      (1, (0,(0,0),3), #     @ $2 = (0,$3)
      (1, (0,2),       #     @ $3 = 2
       (0,1,2))),      #     | ($1 $2)
    1))                #   1

# trivial cycles are okay if not used.
go( 7,          # =?= 7
    (MkLaw,0,1, #   | ? ($0 $1)
      (1, 7,    #     @ $2 = 7
      (1, 3,    #     @ $3 = $3
                #     $2
       2)),     #   9
    9))

# Pattern matching on arrays.
go( (2, (3, 4, 5), 6),            #  =?= (2 (3 4 5) 6)
    (Case,2,2,2,2,2,(3,4,5,6)))   #    | #33 2 2 2 2 2 (3 4 5 6)
