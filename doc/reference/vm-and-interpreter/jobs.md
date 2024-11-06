---
id: jobs-reference
title: Jobs Reference
---

# Jobs Reference

::::warning[TODO]
Additional content forthcoming
::::

```
type Job = Array PLAN
```

A job is an array of minimum length 2, e.g. [f a] or [f a b c d], where the first element is expected to be a function. The VM will apply the function to the rest of the array, evaluate the resulting expression in parallel, and then later inject the result as an input to the cog.

The only thing to note about jobs is that the result might be big, so instead of writing it to the event log, we simply record the job itself together with a note saying whether it completed or crashed. Since PLAN is deterministic, the same job will yield the same result during any future replay. If the job itself contains any big inputs, these can simply be pinned before the job is launched, so that only their hashes get written to the log.
