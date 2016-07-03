# Haskell Memcached Client

`Network.Memcached`: A Memcached client implementation in Haskell.

Supports the most popular memcached commands, including `set`, `add`,
`replace`, `get`, `delete`, `incr`, and `decr`.  

Memcached protocol and command reference:
https://github.com/memcached/memcached/blob/master/doc/protocol.txt

Based off an original work by "olegkat":
https://github.com/olegkat/haskell-memcached

## Safety

Most importantly, `Network.Memcached` attempts to increase the safety of
memcached's intentionally loose protocol.  

### Example: key-value expiration

For example, memcached's `exptime` is a stringified integer value that
could mean:

- "never expire" when it is *equal to zero*,
- "immediately expire" when it is *negative*,
- "expire in x seconds" when it is *between 1 and 2592000* (30 days), and
- "expire on a specific unix timestamp" when it is greater than 2592000.

In any non-trivial application, it's hard, if not impossible, to guarantee
that `exptime` won't be accidentally interpreted by the wrong assumption,
either through misconfiguration or computation.

To weaken this footgun, `Network.Memcached` uses an algebraic data type:

```haskell
data Expiry =
    Never |
    Asap |
    Seconds Word32 |
    Date UTCTime
```

By choosing the appropriate constructor, developers avoid accidentally
wandering into the wrong `exptime` assumption, and computations are performed
in the correct data type (`Word32` or `UTCTime`).  

You may have noticed that there's still a way to make a mistake.  Specifically,
using the `Seconds` constructor with a value greater then 30 days:
`let exp = Seconds 2592001`.  The current codebase handles this at run-time
(unfortunately), by clipping the `Seconds` value to the range 1..2592000.

I've recently discovered [Refined](https://nikita-volkov.github.io/refined/),
which could be a great way to restrict the `Seconds` value to the correct
range at *compile time*.  PR, anyone?

(Coincidentally, the `Date` constructor doesn't have quite the same problem.
The only potentially problematic dates are the unix epoch (1970-01-01T00:00:00)
and the following 30 days. It would be ideal to handle the `Date` constructor
with a refined type, too, even if these dates are more unlikely to affect
a well-designed application.)

### Example: Connections

`Network.Memcached` also prevents improper connection setup via a useful GADT.
The `Server c` data type is parameterized by connection state.  

- `Server NoConnection` represents a disconnected (but configured) server
- `Server Connection` represents a connected server
- `Server AutoConnection` represents a server that will connect automatically
    whenever a memcached command is performed

The only way to call memcached commands is on a `Server Connection`
(ignoring `Server AutoConnection` for the moment).  The easiest way to
obtain a `Server Connection` is via `connect`, which expects a
`Server NoConnection` as input, and the easiest way to obtain a
`Server NoConnection` is to call `configure` with the appropriate
configuration settings.  

The following flow is ensured to be correct without any loss of flexibility:

`configure` --> `connect` --> <memcached commands> --> `disconnect`

You simply can't call `connect` without a properly configured, disconnected
server, and you can't mistakenly reuse an old, possibly disconnected connection.
(See the `Network.Memcached.Server` module.)

Auto-connection works similarly.  Clustering is also integrated with
the `Server` GADT. (See Clustering below.)

## Clustering

Clustering support has been added (see `Network.Memcached.Cluster`), but
only supports an overly simplistic hashing algorithm at the moment.  If
anybody wants to submit a PR for
[ketama](http://www.last.fm/user/RJ/journal/2007/04/10/rz_libketama_-_a_consistent_hashing_algo_for_memcache_clients)
support, that would be awesome!

## Helper

The `cachedOp` function in `Network.Memcached.Helpers` may be used to
transform a simple key-value IO operation (`k -> IO v`) into a key-value
IO operation that caches key-value pairs via memcached.  

Example:

```haskell
-- `cachedDbOp` can now be used wherever `normalDbOp` can be used.
let cachedDbOp = cachedOp normalDbOp connectedServer
```

## Sample code

Here's one way to use this lib.  See the `Network.Memcached.Server` module
for more info.

```haskell
-- Sample creation:
do
    let defaultValue = "foo"
    -- `configure` a "prototype" connection to generate connected clients
    let host = "memcached-server"
        port = 1234
        proto = configure (Seconds 120) Nothing
    -- `connect` to the memcached server.
    mc <- connect proto host (Just port)
    -- `get` a value for a specific `key`.
    cached <- get mc key
    -- Check if we got it.
    case cached of
        Just es -> do
            -- Got it!  Convert from string to intended type.
            return $ read es
        Nothing -> do
            -- You could do all sorts of things here, such as calculate and
            -- store the value in memcached (via the `set` command), for instance.
            -- We'll just return a default value:
            return defaultValue
```

Implementors should consider using `System.Timeout (timeout)` to detect hung servers.  
