---
description: 'Document Type: Tutorial'
---

# A Webserver Cog

We used some pieces of a webserver cog on the last page just to exemplify how JSON works. As a reminder, this was the summary:

> For an example of providing a JSON response from a cog to a web client, we'll pull some lines from `sire/demo_runcog_site.sire`, an example app that allows a web client to start and stop toy timer cogs as well as check the current status of any cogs in the app.

We're going to explain this entire file now, because it has everything you need to build a useful full-stack web app.

::::warning
Coming soon: This page will thoroughly cover every line of the cog under examination.

Until this page is finished, we'll leave a few terse hints here for the intrepid learner to get started. It's a great final challenge.
::::

::::info
Define the shape of our kernel and threads
::::
```sire
# record KernelState
| KERNEL_STATE
* cogStatus  : Tab Nat CogStatus
* servThread : ThreadId
* files      : (HMap Str (ContentType, Pin Bar))

> ThreadId > KernelState
= (newState servThread)
| KERNEL_STATE
* emptyTab
* servThread
* hmEmpty largeConfig
```

::::info
Set up a fileserver binding
::::

```sire
= (fileServer (PIN st) [method path headers (PIN body)])
# switch method
* _ | NONE
* GET
  | **fmapMaybe | hmLookup (barNat path) (**getFiles st)
  & [type (PIN content)]
  @ head | [(b#{content-type}, type)]
  [200 b#gotcha head content]
```

::::info
Set up a JSON request handler binding
::::
```sire
> Ref KernelState > HttpReq > Cog ()
= (handleReq vSt request return)
;| trk [%req request]
@ [rid method path headers pBody@(PIN body)] request
# switch method
* POST
  # switch path
  * b#{/spin}
  ; <etc> ;
```

::::info
Start a cog with with the kernel we defined and some threads for the various HTTP handlers
::::
```sire
> Ref KernelState > Cog Void
= (runHttpServer vSt return)
: ??(rhs_heard req) < syscall HTTP_HEAR
: _                 < handleReq vSt req
| runHttpServer vSt return

> Cog ()
= (launchKernel return)
: servThread  < fork (syscall (**HTTP_SERV emptyFileServer))
: vSt         < newRef (PIN | newState servThread)
: httpThread1 < fork (runHttpServer vSt)
| return ()

main=(runCog launchKernel)
```

