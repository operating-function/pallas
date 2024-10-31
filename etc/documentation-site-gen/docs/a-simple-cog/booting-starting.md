---
description: 'Document Type: Tutorial'
---

# Booting, Starting, Caching and Related Topics

::::warning
This page has many areas that need to be filled in.  
::::

## Boot

If you got adventurous in the previous section, you may have experimented with
deleting the `~/my_ships/hello_world` ship directory and running the 

```
pallas boot ~/my_ships/hello_world sire/hello_world_cog.sire
```

Command a second time.

If so, you may have been surprised that you didn't see the `trk` "hello world"
on subsequent boots!

The output would have ended with something like:

```
("datom","LOADED FROM CACHE!")
("prelude","LOADED FROM CACHE!")
("hello_world_cog","LOADED FROM CACHE!")
```

Notice that your `hello_world_cog.sire` file is being mentioned here in the `"LOADED
FROM CACHE!"` message.

::::warning
Coming soon: Explanation of caching
::::

If you _change_ `hello_world_cog.sire`, delete the ship, and re-boot, you'll get a
fresh run with the new code (say we changed our `trk` string):

```
("datom","LOADED FROM CACHE!")
("prelude","LOADED FROM CACHE!")
(helloWorld a)=(_Trace:{hello worlp} a-0)

{hello worlp}
```

# Booting vs Starting

When you `pallas boot`, it creates a machine and runs the cog code (which is when we saw the `trk`
output). But you may have noticed that the ship _shut down_ after completing
that task. Most of the time you want a ship to keep running and continue talking
to the outside world.

To start an existing machine, you use the `start` command:


```
pallas start ~/my-ships/hello_world
```

```
<...>
[{2024-01-16T16:26:57.740950492Z} {REPLAY TIME: 0 ns}]

HTTP_SPINNING
_http_port=39023


= _http_port_file
} /home/your-user/my-ships/hello_world/11233545598979337509.http.port


Shutting down...
```

Hm, it didn't `trk` "hello world" - it just seemed to immediately shut down
again.

## Another Example Cog

Here's another simple cog, `countdown_cog.sire`, to illustrate this concept:

```sire
#### countdown_cog <- prelude

:|  prelude

;;;;;

= (countDownFrom count return)
: ??(syscall_time now) < syscall TIME_WHEN
| trk [%currentCount count %currentTime now]
| if (isZero count)
  | trk ["now we're done"]
  | return ()
| countDownFrom (dec count) return

main=(runCog | countDownFrom 5)
```

This is a simple looping/recursive cog. On each run, it starts off with a counter value and stores the current time to the binding `now` by asking the host machine `TIME_WHEN` (we'll discuss `syscall` more later). On the next line it prints out that time.  
Then it checks if the counter value `isZero`. If it is, it prints out a completion message, otherwise it decrements the counter value (`(dec count)`) and recursively calls itself with that updated value.

Incidentally, this is also the first time we're seeing indentation in sire.

Another detail to notice here: in `main=` we're passing the argument `5` to the
`countDownFrom` function: `main=(runCog | countDownFrom 5)`.

Here's a reminder of the binding for `countDownFrom`:
```sire
= (countDownFrom count return)
```

`countDownFrom` takes an additional arguments, `count`, along with the usual `return`. So when we kick the cog off, we have to provide that `count` value, which the simple function uses for its loop iterations.

Let's boot it:

```
pallas boot ~/my-ships/countdown sire/countdown_cog.sire
```

```
<...>
("datom","LOADED FROM CACHE!")
("prelude","LOADED FROM CACHE!")
= (countDownFrom a b)
| syscall:[%time 0 %when]
| syscall_time-a-b-countDownFrom

= main
| KERNEL 0 0
  [syscall_time-5-DONE-countDownFrom]
| [[%time 0 %when]]

("cache hash","C4N5JkXvBuoBUUJNwh7nWQAbtM48n7zsew65V1qMLnMF")
```

No `trk` message on boot this time...


::::warning
Coming soon: This will be explained
::::

Let's start the cog:

```
pallas start ~/my-ships/countdown
```

```
[{2024-01-17T15:36:04.532911132Z} {REPLAY TIME: 0 ns}]
HTTP_SPINNING

_http_port=35509


= _http_port_file
} ~/my-ships/countdown/13693678241121744017.http.port



++ [%trk {2024-01-17T15:36:04.534179128Z}]
++ [%currentCount 5 %currentTime 1705505764]



++ [%trk {2024-01-17T15:36:04.53453922Z}]
++ [%currentCount 4 %currentTime 1705505764]



++ [%trk {2024-01-17T15:36:04.534646906Z}]
++ [%currentCount 3 %currentTime 1705505764]



++ [%trk {2024-01-17T15:36:04.534727221Z}]
++ [%currentCount 2 %currentTime 1705505764]



++ [%trk {2024-01-17T15:36:04.534809672Z}]
++ [%currentCount 1 %currentTime 1705505764]


++ [%trk {2024-01-17T15:36:04.53489128Z}]
++ [%currentCount 0 %currentTime 1705505764]


++ [%trk {2024-01-17T15:36:04.534922057Z}]
++ [{now we're done}]
```

Now we see the `trk` output, counting down from the `count` value we provided.

Try deleting the `~/my-ships/countdown` directory, re-booting and re-starting
without changing anything about `countdown_cog.sire`. You'll notice that you get the
`trk` output every time you re-boot and re-start even though you didn't change
the source code.

 
::::warning
Coming soon: This will be explained
::::

# Next

Our next goal is to construct a long-running cog that acts as a web server and keeps some state. In order to talk to a web client, our cog will need to reply in a manner that the browser understands. Beyond basic HTTP codes, we also want to send back useful data, and we're going to do so in JSON. So we'll take a little detour into how to encode Pallas nats as JSON.
