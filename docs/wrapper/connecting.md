# kanin RabbitMQ Documentation

## [Wrapper Libraries](index.md)

### Table of Contents

* [Connecting](#connecting-)
  * [Connecting to a Broker](#connecting-to-a-broker-)
  * [Connecting To A Broker with AMQP URIs](#connecting-to-a-broker-with-amqp-uris-)


### Connecting

#### Connecting to a Broker [&#x219F;](#table-of-contents)

The `kanin-conn` module is used to start a connection to the broker. It
requires either a direction connection options record or a network connection
options record. These may be generated either by creating the records directly
or providing connection information in a URI and having those parsed to
generate the record for you.

Example optoins from record-creation:

```cl
lfe> (make-amqp_params_direct)
#(amqp_params_direct none none #"/" nonode@nohost none ())
```

and

```cl
lfe> (make-amqp_params_network host "localhost")
#(amqp_params_network
  #"guest"
  #"guest"
  #"/"
  "localhost"
  undefined 0 0 0 infinity none
  (#Fun<amqp_auth_mechanisms.plain.3> #Fun<amqp_auth_mechanisms.amqplain.3>)
  () ())
```

Example options from URI-parsing:

```cl
lfe> (kanin-uri:parse "amqp://dave:secret@")
#(ok #(amqp_params_direct #"dave" #"secret" #"/" nonode@nohost none ()))
```

and

```cl
lfe> (kanin-uri:parse "amqp://alice:secret@host:10000/vhost")
#(ok
  #(amqp_params_network
    #"alice"
    #"secret"
    #"vhost"
    "host"
    10000 0 0 10 infinity none
    (#Fun<amqp_uri.11.121287672> #Fun<amqp_uri.11.121287672>)
    () ()))
```

(For more information in this, see the section below:
"Connecting To A Broker with AMQP URIs".)

To use these options to create an actual connection, use the `kanin-conn`
module:

```cl
lfe> (set `#(ok ,net-opts) (kanin-uri:parse "amqp://localhost"))
lfe> (set `#(ok ,conn) (kanin-conn:start net-opts))
```

That's in the REPL; in an application using `kanin`, you'd want to use
something like a `(let ...)` statement.

The `ksnin-conn:start` function returns `#(ok ,conn)`, where
`conn` is the pid of a process that maintains a permanent
connection to the broker.

In case of an error, the above call returns `#(error ,error)`.

The example above has just `"localhost"` as a parameter. However, there will
often be many more than that.

An AMQP broker contains objects organised into groups called virtual hosts. The
concept of virtual hosts gives an administrator the facility to partition a
broker resource into separate domains and restrict access to the objects
contained within these groups. AMQP connections require client authentication
and the authorisation to access specific virtual hosts.

The `(make-amqp_params_network)` record macro sets the following default
values:

| Parameter         |  Default Value  |
|-------------------|-----------------|
| username          |  guest          |
| password          |  guest          |
| virtual_host      |  /              |
| host              |  localhost      |
| post              |  5672           |
| channel_max       |  0              |
| frame_max         |  0              |
| heartbeat         |  0              |
| ssl_options       |  none           |
| auth_mechanisms   |  `(list #'amqp_auth_mechanisms:plain/3 #'amqp_auth_mechanisms:amqplain/3)` |
| client_properties |  `'()`        |

These values are only the defaults that will work with an out of the box broker
running on the same host. If the broker or the environment has been configured
differently, these values can be overridden to match the actual deployment
scenario.

SSL options can also be specified globally using the `ssl_options` environment
key for the `amqp-client` application. They will be merged with the SSL
parameters from the URI (the latter will take precedence).

If a client wishes to run inside the same Erlang cluster as the RabbitMQ broker,
it can start a direct connection that optimises away the AMQP codec. To start a
direct connection, use `kanin-conn:start/1` with the parameter set to an
`(make-amqp_params_direct)` record.

Providing a username and password is optional, since the direct client is
considered trusted anyway. If a username and password are provided then they
will be checked and made available to authentication backends. If a username is
supplied, but no password, then the user is considered trusted and logged in
unconditionally. If neither username nor password are provided then the
connection will be considered to be from a "dummy" user which can connect to any
virtual host and issue any AMQP command.

The `(make-amqp_params_direct)` record macro sets the following default
values:

| Parameter         |  Default Value  |
|-------------------|-----------------|
| username          |  `#"guest"`     |
| password          |  `#"guest"`     |
| virtual_host      |  `#"/"`         |
| node              |  `(node)`       |
| client_properties |  `'()`          |


#### Connecting To A Broker with AMQP URIs [&#x219F;](#table-of-contents)

Instead of working the `(make-amqp_params_*)` records directly, [AMQP
URIs](https://www.rabbitmq.com/uri-spec.html) may be used. The
`(kanin-uri:parse/1)` function is provided for this purpose. It parses an URI
and returns the equivalent `amqp_params_*` record. Diverging from the spec,
if the hostname is omitted, the connection is assumed to be direct and an
`amqp_params_direct` record is returned. In addition to the standard host,
port, user, password and vhost parameters, extra parameters may be specified via
the query string (e.g. `"?heartbeat=5"`).

AMQP URIs are defined with the following ABNF rules:

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

Using a connection URI, you can creat either direct or network connection
options, and then use those to connect via the `kanin-conn:start` function (as
demonstrated above).
