---
id: cogs-reference
title: Cogs Reference
---

# Cogs Reference

::::warning[TODO]
Forthcoming
::::

We can get a more exact feel for the format by looking at a few rewrites:

```unset
(cog0 state0 query0 workers0)
<VM injects inputs0>

(cog0 state0 query0 workers0 inputs0)
=> ((cog1 state1 query1 workers1), outputs1)

<VM launches and kills workers, makes state1 and query1 available, and sends outputs1 to drivers>

(cog1 state1 query1 workers1)
<VM injects inputs1>

(cog1 state1 query1 workers1 inputs1)
=> ((cog2 state2 query2 workers2), outputs2)

<VM launches and kills workers, makes state2 and query2 available, and sends outputs2 to drivers>

(cog2 state2 query2 workers2)
<...>
```
