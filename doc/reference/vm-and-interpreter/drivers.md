---
id: drivers-reference
title: Drivers Reference
---

# Drivers Reference

::::warning[TODO]
Forthcoming
::::

We can get a more exact feel for the format by looking at a few rewrites:

```
driver0
<VM injects message ID and input>

(driver0 mid0 input0)
=> (driver1, effects1)
<VM handles effects1>

driver1
<VM injects message ID and input>

(driver1 mid1 input1)
=> (driver2, effects2)
<VM handles effects2>

driver2
<...>
```

The only thing that’s required for something to be a driver is that its arity is 2, and that it returns a new driver and an array of effects. 

