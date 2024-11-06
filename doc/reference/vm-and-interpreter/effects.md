---
id: effects-reference
title: Effects Reference
---

# Effects Reference

::::warning[TODO]
Additional content forthcoming
::::

Below is the full list of effects that are currently available to drivers:

```unset
type Conn = Nat
type IP   = Nat
type Port = Nat

data Eff a where
    COG_READ  :: Any ->           Eff Any              -- Concurrent cog query
    COG_WRITE :: Any ->           Eff Void             -- Cog input. Never returns!

    TIME_WHEN ::                  Eff Nat              -- Get current time
    TIME_WAIT :: Nat ->           Eff Unit             -- Wake at time t

    TCP_OPEN  :: IP   -> Port  -> Eff Conn             -- Returns connection ID
    TCP_GIVE  :: Conn -> Bytes -> Eff Nat              -- Returns bytes sent
    TCP_HEAR  ::                  Eff (Conn, IP, Port) -- Accept incoming connection
    TCP_TAKE  :: Conn ->          Eff Bytes            -- Receive bytes
    TCP_PORT  ::                  Eff Nat              -- Get my TCP port
    TCP_SHUT  :: Conn ->          Eff Unit             -- Close a connection
```
