# Cogs

::::warning
TODO. Don't read this yet.
:::;

the state of every cog is a function.
Let's say that you have state machine that takes in numbers, and spits out numbers.

```
type Machine = Integer -> (Integer, Machine)
```

We can make a machine that allows a number to be retrieved and modified:

```
= (machine state input)
@ newState (intAdd input state)
| (newState, machine newState)
```

`(machine 0)` will retrieve the state.  
And `(machine 5)` will increase by five. `(machine -5)` will decrease by five, etc.
The way that the machine "stores the number", is by just returning a **closure** that contains that number.

If the number is 5, then the state of the machine is `(machine 5)`.  If you give that the input 5, then the new state of the machine will be `(machine 10)`.

in this ultra-trivialized state machine, the event log is just a list of numbers.  And a snapshot is just the actual PLAN value `(machine n)`.

---

`modifyState` uses `readRef` and `writeRef` to modify "slots" in the `kern.sire` state:

```
> Ref CogState > (CogState > CogState) > Cog ()
= (modifyState vSt fun return)
: (PIN old) < readRef vSt
@ srv       | **getServThread old
@ pNew      | PIN (fun old)
: _         < writeRef vSt pNew
: _         < cancelFork srv (syscall (**HTTP_SERV | fileServer pNew))
| return ()
```
