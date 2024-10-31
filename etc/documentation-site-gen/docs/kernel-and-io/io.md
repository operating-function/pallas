# IO

::::warning
TODO. Don't read this yet.
::::

## IO and `runCog`

- syscall, fork

```sire
(runCog someCog)
```

```sire
= (runCog act)

@ st,tid (allocateThread emptyState)
; emptyState is: (~[], [], [])
; (the ~[] in emptyState will evaluate to 0)

; allocateThread returns the State and ThreadId
; like this: [[0 [0] [0]] 0]
; so "@ st,tid (allocateThread emptyState)" results in:
;  - tid=0
;  - st=[0 [0] [0]]

; when allocating a thread in a non-empty state:

= someState (~[10], [], [])
;; looks like this: someState=[[10 0] [] []]
= st,tid (allocateThread someState)
;;;;;;
;   _g1005=[[0 [] []] 10]
;    st=[0 [] []]
;    tid=10

; another one:
= someOtherSTate (~[16 42], [], [])
;; looks like this: someOtherSTate=[[16 [42 0]] [] []]
= st,tid (alloc someOtherSTate)
;;;;;;;
;    _g1015=[[[42 0] [] []] 16]
;    st=[[42 0] [] []]
;    tid=16


@ st     (act nullThread tid st)
| ioLoop st (**getRequestsRow st)
```

allocateThread:

- if the free list is empty, (so evaluates to `0`)
- set `key` to `len slots`. in `emptyState` this is `[]`, or `0`
- set `slots` to concatenation of `slots` and `0`. in `emptyState` this is `[0]`
- set `requests` to concatenation of `requests` and `0`. in `emptyState` this is `[0]`
- set kernel state (`st`) to `[free slots requests]`. in `emptyState` this is
`[0 [0] [0]]` due to above steps.
