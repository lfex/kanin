# kanin RabbitMQ Documentation

## [OTP Library](index.md)

*High-level kanin API for RabbitMQ/AMQP*


### Table of Contents

* [Starting a Server](#starting-a-server-)
* [Connections and Channels](#connections-and-channels-)
* [AMQP Connection URIs](#amqp-connection-uris-)


### Starting a Server [&#x219F;](#table-of-contents)

The kanin application starts a kanin `gen_server` under the covers. The
recommended way to start the application and all of its dependencies is:

```cl
lfe> (application:ensure_all_started 'kanin)
```

Note that, without any additional overriding configuration ,this will create a
default connection to RabbitMQ running on localhost.

Calling `ensure_all_started` will result in output something like the
following:

```cl
23:16:47.195 [info] Application lager started on node nonode@nohost
23:16:47.195 [info] Application jsx started on node nonode@nohost
23:16:47.195 [info] Application rabbit_common started on node nonode@nohost
23:16:47.199 [info] Application amqp_client started on node nonode@nohost
23:16:47.244 [info] Application kanin started on node nonode@nohost
#(ok
  (xmerl compiler syntax_tools goldrush lager jsx rabbit_common
   amqp_client kanin))
```

When in development mode, you may also start the server with one of
`kanin:start/{0,2}`. The two-arity version of the function is how OTP starts
the kanin application, and it takes as it's first arguement `'normal` and as
its second argument any of the following:

* `'()`
* `#("amqp://...")` (see below for more on connection strings)
* `#("amqp://..." gen-server-options)`

In the last case, the gen-server options are the standard options expected
by Erlang/OTP  when starting a`gen_server`.

If your application is being deployed to production (for which you will have
hopefully created an OTP release) and your RabbitMQ server requires non-default
connection values, you will need to update your
[sys.config](http://erlang.org/doc/man/config.html)
file to pass a correct connection URI to kanin (see also
[app.src docs](http://erlang.org/doc/man/app.html)). For example:

```erlang
...
[{kanin, [mod, {kanin, ["alice:secret@amqp://rabbithost.example.com:1234"]}]}].
...
```

You may also want to pass a second parameter to the `kanin` start module, the
options for the kanin gen-server.


### Connections and Channels [&#x219F;](#table-of-contents)

When a kanin application is started, a default connection and channel is
created automatically. You may obtain the pids for these LFE/Erlang processes
by calling the following functions:

```cl
lfe> (kanin:get-conn)
<0.104.0>
lfe> (kanin:get-conn 'default)
<0.104.0>
lfe> (kanin:get-chan)
<0.115.0>
lfe> (kanin:get-chan 'default)
<0.115.0>
```

If you chose to create new connections and channels, you may do so with
these:

```cl
lfe> (set admin-conn 'admin)
admin
lfe> (set admin-chan 'admin)
admin
lfe> (kanin:add-conn admin-conn)
<0.129.0>
lfe> (kanin:add-chan admin-conn admin-chan)
<0.139.0>
```

You may retried the pids for these at any time by calling `get-conn/1` and
`get-chan/1`, as above. You may also list all connections and channels with the
following functions:

```cl
lfe> (kanin:get-conns)
(#(admin <0.129.0>) #(default <0.104.0>))
lfe> (kanin:get-chans)
(#(admin <0.139.0>) #(default <0.115.0>))
```


### AMQP Connection URIs [&#x219F;](#table-of-contents)

In the high-level kanin API, connection URIs are the only way to specify
broker connection options. AMQP URIs are defined with the following ABNF
rules:

```
amqp_URI       = "amqp://" amqp_authority [ "/" vhost ] [ "?" query ]
amqp_authority = [ amqp_userinfo "@" ] host [ ":" port ]
amqp_userinfo  = username [ ":" password ]
username       = *( unreserved / pct-encoded / sub-delims )
password       = *( unreserved / pct-encoded / sub-delims )
vhost          = segment
```

Here are some examples:

| Parameter                                      | Username | Password | Host    | Port  | Vhost   |
|------------------------------------------------|----------|----------|---------|-------|---------|
| amqp://alice:secret@host:10000/vhost           | "alice"  | "secret" | "host"  | 10000 | "vhost" |
| amqp://bo%62:%73ecret@h%6fst:10000/%76host     | "bob"    | "secret" | "host"  | 10000 | "vhost" |
| amqp://                                        |          |          |         |       |         |
| amqp://:@/                                     | ""       | ""       |         |       | ""      |
| amqp://carol@                                  | "carol"  |          |         |       |         |
| amqp://dave:secret@                            | "dave"   | "secret" |         |       |         |
| amqp://host                                    |          |          | "host"  |       |         |
| amqp://:10000                                  |          |          |         | 10000 |         |
| amqp:///vhost                                  |          |          |         |       | "vhost" |
| amqp://host/                                   |          |          | "host"  |       | ""      |
| amqp://host/%2f                                |          |          | "host"  |       | "/"     |
| amqp://[::1]                                   |          |          | "[::1]" |       |         |
