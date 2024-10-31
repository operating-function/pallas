---
description: 'Document Type: Explanation'
---

# JSON

## Handling JSON requests

We're going to look at some selections from the `sire/demo_mandelbrot_ui.sire` cog that you saw when you first installed Pallas on your system.

```sire
:| json

;; <etc> ;;


= (asJsonRow m)
# datacase m
* JVEC|v | SOME v
* _      | NONE

= (asJsonNum m)
# datacase m
* JNUM|n | SOME n
* _      | NONE

(bindMaybe mVal k)=(maybeCase mVal NONE k)

;; <etc> ;;

= (parseFract jsonBS)
@ res@[json leftover] (parseJson jsonBS)
: rw < **bindMaybe (asJsonRow json)
: w < **bindMaybe (asJsonNum idx-0-rw)
: h < **bindMaybe (asJsonNum idx-1-rw)
| SOME [w h]

;; <etc> ;;
# switch method
* POST
  # switch path
  * b#{/fract}
    # datacase (parseFract body)
    * NONE
      : _ < fork (syscall (**HTTP_ECHO rid 400 b#bad [] b#{}))
      | return ()
    * (SOME dims)
      @ [w h] dims
      : !fractBar < trkTimer {mandelbrotDemo} (mandelbrotDemo w h)
      : _ < fork (syscall (**HTTP_ECHO rid 200 b#ok [] fractBar))
      | return ()
;; <etc> ;;
```
Taking that one chunk at a time:

```
:| json
```

Import `json.sire`, from which we'll need the `parseJson` function soon.

```
= (asJsonRow m)
# datacase m
* JVEC|v | SOME v
* _      | NONE

= (asJsonNum m)
# datacase m
* JNUM|n | SOME n
* _      | NONE

(bindMaybe mVal k)=(maybeCase mVal NONE k)
```

We'll look at `datacase` next, but consider it a sort of switch or if/else for now.

`asJsonRow` takes some parsed JSON and returns it as a sire row (**if** there's anything there. That's where `datacase` comes in).  
`asJsonNum` takes some parsed JSON and returns it as a nat.  
`sire/json.sire` has similar helpers for JSON's nulls, boolens, strings and maps (objects) (`JNULL`, `JFALSE`, `JTRUE`, `JSTR`, `JMAP`).

```
= (parseFract jsonBS)
@ res@[json leftover] (parseJson jsonBS)
: rw < **bindMaybe (asJsonRow json)
: w < **bindMaybe (asJsonNum idx-0-rw)
: h < **bindMaybe (asJsonNum idx-1-rw)
| SOME [w h]
```

The mandelbrot backend accepts a `width` and `height` from the client side and generates a fractal of these dimensions.

`parseFract` takes a JSON-y bar (which it happens to expect to be a single "row" of json numbers) and uses the above-described two helpers to bind these values to `w` and `h`. Width and height.

Now that we have all the handlers in place, we can actually take a request from the browser:

```
# switch method
* POST
  # switch path
  * b#{/fract}
    # datacase (parseFract body)
    * NONE
      : _ < fork (syscall (**HTTP_ECHO rid 400 b#bad [] b#{}))
      | return ()
    * (SOME dims)
      @ [w h] dims
      : !fractBar < trkTimer {mandelbrotDemo} (mandelbrotDemo w h)
      : _ < fork (syscall (**HTTP_ECHO rid 200 b#ok [] fractBar))
      | return ()
```

We haven't yet looked at how a webserver cog works, but the point here is:
- We get a `POST` request at a particular path from the web client
- We parse the request body as JSON
- Assuming we got dimensions as expected, we generate the fractal (`fractBar`) and respond to the request with this value. Responding with JSON is what we're looking at next.

## Responding **with** JSON

For an example of providing a JSON response from a cog to a web client, we'll pull some lines from `sire/demo_runcog_site.sire`, an example app that allows a web client to start and stop toy timer cogs as well as check the current status of any cogs in the app:

```sire
:| json

;; <etc> ;;

= jsonContentType [(b#{content-type}, b#{application/json})]
```

Once again, `json` is imported, which will give us many functions we need.  
`jsonContentType` is a convenient binding that will make it easier to set response headers.

```sire
# switch method
* GET
  # switch path
  * b#{/status}
    ; Builds a JSON blob of all the details for the UI on the other side.
    : (PIN st) < readRef vSt
    @ kvs | tabToPairs | **getCogStatus st
    @ jval
        | JVEC
        : [cogid status] < foreach kvs
        | JMAP
        ## =cogid (JSTR | natBar | showNat cogid)
        ## =status
            # datacase status
            * (STARTED _ _) | JMAP #[Spinning=JNULL]
            * (STOPPED a)
                 @ tag | idx 0 a
                 | if | eql 0 tag
                     | JMAP #[Finished=JNULL]
                 | if | eql 1 tag
                     | JMAP #[Finished=JNULL]
                 | if | eql 2 tag
                     ; TODO: print the values out and send them.
                     | JMAP #[Crashed=(JMAP #[op={op} arg={arg}])]
                 | else
                     | JMAP #[TimedOut=(JSTR | natBar | showNat | idx 1 a)]
    @ resultBS | printJson jval
    : _ < fork (syscall (**HTTP_ECHO rid 200 b#ok jsonContentType resultBS))
    | return ()
```

And again, we're handling an HTTP request by switching on `method` and `path` in order to handle an HTTP `GET /status`.

We haven't seen `readRef` and a few other bits here, but suffice it to say that we're fetching some values from the cog's "state" and binding them to `kvs`.

The `jval` binding is doing most of the work here. The mandelbrot cog we saw previously used some helper functions for JSON parsing while here the JSON encoding is happening directly inline with `JMAP` and related functions from the `json` library.  
It's good to see both approaches.

We won't go through this line-by-line - you can handle it by this stage. The broad strokes are:

- For each of the timer cogs we know about, encode their statuses depending on which state they're in. These are encoded as a row of JSON objects (`JMAP`).
- Get a JSON bar and appropriate headers and provide an HTTP 200 response with these values.

After doing two `POST /spin` requests to start a couple timers, a `GET /status` HTTP response provides this JSON body:

```json
[
    {
        "cogid": "8766384119476525368",
        "status": {
            "Finished": null
        }
    },
    {
        "cogid": "9108945462363633005",
        "status": {
            "Spinning": null
        }
    }
]
```

For completeness, here are the headers:

```
HTTP/1.1 200 ok
Date: Tue, 07 May 2024 05:47:43 GMT
Server: Warp/3.3.25
Transfer-Encoding: chunked
content-type: application/json
```

---

Pallas is talking JSON to a web browser. Very nice.

We promised to tell you a bit about `datacase` and cog state, so let's do that next.
