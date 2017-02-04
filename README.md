# kanin

[![Build Status][travis badge]][travis]
[![LFE Versions][lfe badge]][lfe]
[![Erlang Versions][erlang badge]][versions]
[![Tags][github tags badge]][github tags]
[![Downloads][hex downloads]][hex package]

[![][project-logo]][project-logo-source]

*An LFE Wrapper for the Erlang RabbitMQ (AMQP) Client*


## Table of Contents

* [Introduction](#introduction-)
* [Installation](#installation-)
* [Documentation](#documentation-)
* [Supported RabbitMQ Modules](#supported-modules-)
* [Usage](#usage-)
  * [Copyright Notice](#copyright-notice-)
  * [The LFE AMQP Client Library](#the-lfe-amqp-client-library-)
  * [Programming Model](#programming-model-)
  * [AMQP Commands](#amqp-commands-)
  * [Including Header Files](#including-header-files-)
  * [Connecting to a Broker](#connecting-to-a-broker-)
  * [Connecting To A Broker with AMQP URIs](#connecting-to-a-broker-with-amqp-uris-)
* [License](#license-)


## Introduction [&#x219F;](#table-of-contents)

The kanin library is a wrapper for various modules in the Erlang AMQP client
library. kanin was created in order to not only provide a less verbose AMQP
client library for LFE hackers, but one that was also more Lispy.


## Installation [&#x219F;](#table-of-contents)

To pull in kanin as part of your project, just add it to your `rebar.config`
deps:

```erlang
  {deps, [
    ...
    {kanin, {git, "git@github.com:lfex/kanin.git", {branch, "master"}}}
  ]}.
```

And then do:

```bash
    $ rebar3 compile
```


## Documentation [&#x219F;](#table-of-contents)

Below, in the "Usage" section, you will find information about using Kanin
with RabbitMQ in LFE projects.

Also, you may be interested in the [Kanin
tutorials](http://billo.gitbooks.io/lfe-rabbitmq-tutorials/), which
have been translated into LFE from the official RabbitMQ docs for Python and Erlang.


## Supported Modules [&#x219F;](#table-of-contents)

The following `amqp_*` modules have been included in kanin:
 * `amqp_channel` -> `kanin-chan`
 * `amqp_connection` -> `kanin-conn`
 * `amqp_uri` -> `kanin-uri`

If your favorite `amqp_*` module is not among those, feel free to submit a
new ticket requesting the addition of your desired module(s) or submit a
pull request with the desired inclusion added.


## Usage [&#x219F;](#table-of-contents)


### Copyright Notice [&#x219F;](#table-of-contents)

The following content was copied from
[the Erlang Client User Guide](https://www.rabbitmq.com/erlang-client-user-guide.html)
on the [RabbitMQ site](https://www.rabbitmq.com/).
The original copyright was in 2014, held by Pivotal Software, Inc.


### The LFE AMQP Client Library [&#x219F;](#table-of-contents)

The AMQP client provides an Erlang interface to compliant AMQP brokers. The
client follows the AMQP execution model and implements the wire level
marshaling required to encode and decode AMQP commands in a protocol
conformant fashion. AMQP is a connection orientated protocol and multiplexes
parallel interactions via multiple channels within a connection.

This user guide assumes that the reader is familiar with basic concepts of AMQP
and understands exchanges, queues and bindings. This information is contained in
the protocol documentation on the AMQP website. For details and exact
definitions, please see
[the AMQP specification document](http://www.amqp.org/).

The basic usage of the client follows these broad steps:

 * Establish a connection to a broker
 * Create a new channel within the open connection
 * Execute AMQP commands with a channel such as sending and receiving messages,
   creating exchanges and queue or defining routing rules between exchanges and
   queues
 * When no longer required, close the channel and the connection


### Programming Model [&#x219F;](#table-of-contents)

Once a connection has been established, and a channel has been opened, an
LFE application will typically use the `kanin-chan:call/{2,3}` and
`kanin-chan:cast/{2,3}` functions to achieve most of the things it needs to
do.

The underlying Erlang AMQP client library is made up of two layers:

 * A high level logical layer that follows the AMQP execution model, and
 * A low level driver layer that is responsible for providing a physical
   transport to a broker.

There are two drivers in the client library:

 * The network driver establishes a TCP connection to a protocol compliant AMQP
   broker and encodes each command according to the specification. To use this
   driver, start a connection using `kanin-conn:start/1` with the parameter
   set to an `#amqp_params_network` record.

 * The direct driver uses native Erlang messaging instead of sending AMQP
   encoded commands over TCP. This approach avoids the overhead of marshaling
   commands onto and off the wire. However, the direct driver can only be used
   in conjunction with the RabbitMQ broker and the client code must be deployed
   into the same Erlang cluster. To use the direct driver, start a connection
   using `kanin-conn:start/1` with the parameter set to an
   `#amqp_params_direct` record.

At run-time, the Erlang client library re-uses a subset of the functionality
from the RabbitMQ broker. In order to keep the a client deployment independent
of RabbitMQ, the Erlang client build process produces an archive containing all
of the common modules. This archive is then put onto the load path of the client
application.

For more detailed information on the API, please refer to the reference
documentation.

Furthermore, the test suite that is part of the source distribution of the
client library contains many complete examples of how to program against the
API.


### AMQP Commands [&#x219F;](#table-of-contents)

The general mechanism of interacting with the broker is to send and receive AMQP
commands that are defined in the protocol documentation. During build process,
the machine-readable version of the AMQP specification is used to auto-generate
Erlang records for each command. The code generation process also defines
sensible default values for each command. Using default values allows the
programmer to write terser code - it is only necessary to override a field if
you require non-default behaviour. The definition of each command can be
consulted in the `include/rabbit-framing.lfe` header file. For example,
when using the `(make-exchange.declare ...)` record-creating macro,
specifying the following:

```cl
(make-exchange.declare exchange (list_to_binary "my_exchange"))
```

is equivalent to this:

```cl
(make-exchange.declare
  exchange #"my_exchange"
  ticket 0
  type #"direct"
  passive 'false
  durable 'false
  auto_delete 'false
  internal 'false
  nowait 'false
  arguments '())
```


### Including Header Files [&#x219F;](#table-of-contents)

The LFE client uses a number of record definitions which you will encounter
in this guide. These records fall into two broad categories:

* Auto-generated AMQP command definitions from the machine readable version of
  the specification
* Definitions of data structures that are commonly used throughout the client

To gain access to these records, you need to include the `amqp-client.lfe`
file in every module that uses the Erlang client:

```cl
(include-lib "kanin/include/amqp-client.lfe")
```


### Connecting to a Broker [&#x219F;](#table-of-contents)

The `kanin-conn` module is used to start a connection to the broker. It
requires either a direction connection options record or a network connection
options record. These may be generated either by creating the records directly
or providing connection information in a URI and having those parsed to
generate the record for you.

Example record-creation:

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

Example option URI parsing:

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


### Connecting To A Broker with AMQP URIs [&#x219F;](#table-of-contents)

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


### Creating Channels [&#x219F;](#table-of-contents)

TBD


### Managing Exchanges and Queues [&#x219F;](#table-of-contents)

TBD


### Sending Messages [&#x219F;](#table-of-contents)

TBD


### Receiving Messages [&#x219F;](#table-of-contents)

TBD


### Subscribing to Queues [&#x219F;](#table-of-contents)

TBD


### Subscribing Internals [&#x219F;](#table-of-contents)

TBD


### Closing Channels and the Connection [&#x219F;](#table-of-contents)

TBD


### Complete Example [&#x219F;](#table-of-contents)

TBD


### Client Deployment [&#x219F;](#table-of-contents)

TBD


### Egress Flow Control [&#x219F;](#table-of-contents)

TBD


### Ingress Flow Control [&#x219F;](#table-of-contents)

TBD


### Handling Returned Messages [&#x219F;](#table-of-contents)

TBD


## License [&#x219F;](#table-of-contents)

Apache License, Version 2.0

Copyright © 2014-2017, BilloSystems, Ltd. Co.

Copyright © 2015-2017, Ricardo Lanziano

Copyright © 2015-2017, Duncan McGreggor


<!-- Named page links below: /-->

[project-logo]: resources/images/kanin-small.png
[project-logo-large]: resources/images/kanin.png
[project-logo-source]: http://aquarius-galuxy.deviantart.com/art/Rabbit-Drawing-176749973
[org]: https://github.com/lfex
[github]: https://github.com/lfex/kanin
[gitlab]: https://gitlab.com/lfex/kanin
[travis]: https://travis-ci.org/lfex/kanin
[travis badge]: https://img.shields.io/travis/lfex/kanin.svg
[lfe]: https://github.com/rvirding/lfe
[lfe badge]: https://img.shields.io/badge/lfe-1.2+-blue.svg
[erlang badge]: https://img.shields.io/badge/erlang-17+-blue.svg
[versions]: https://github.com/lfex/kanin/blob/master/.travis.yml
[github tags]: https://github.com/lfex/kanin/tags
[github tags badge]: https://img.shields.io/github/tag/lfex/kanin.svg
[github downloads]: https://img.shields.io/github/downloads/atom/atom/total.svg
[hex badge]: https://img.shields.io/hexpm/v/kanin.svg
[hex package]: https://hex.pm/packages/kanin
[hex downloads]: https://img.shields.io/hexpm/dt/kanin.svg
