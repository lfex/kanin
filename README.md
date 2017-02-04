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
  * **Notices**
  * [Copyright Notice](#copyright-notice-)
  * **Getting Started**
  * [The LFE AMQP Client Library](#the-lfe-amqp-client-library-)
  * [Programming Model](#programming-model-)
  * [AMQP Commands](#amqp-commands-)
  * [Including Header Files](#including-header-files-)
  * **Connecting**
  * [Connecting to a Broker](#connecting-to-a-broker-)
  * [Connecting To A Broker with AMQP URIs](#connecting-to-a-broker-with-amqp-uris-)
  * **Channels, Exchanges, Queues, and Messages**
  * [Creating Channels](#creating-hannels-)
  * [Managing Exchanges and Queues](#managing-exchanges-and-queues-)
  * [Sending Messages](#sending-messages-)
  * [Receiving Messages](#receiving-messages-)
  * [Subscribing to Queues](#subscribing-to-queues-)
  * [Subscribing Internals](#subscribing-internals-)
  * [Closing Channels and the Connection](#closing-channels-and-the-connection-)
  * **Examples**
  * [Complete Example](#complete-example-)
  * **Flow Control**
  * [Delivery Flow Control](#delivery-flow-control-)
  * [Blocked Connections](#blocked-connections-)
  * [Handling Returned Messages](#handling-returned-messages-)
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
(make-exchange.declare exchange #"my_exchange")
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

Once a connection to the broker has been established, the `kanin-conn` module
can be used to create channels:

```cl
lfe> (set `#(ok ,chan) (kanin-conn:open-channel conn))
#(ok <0.114.0>)
```

This function takes the pid of the connection process and returns
`#(ok, chan)`, where `chan` is a pid that encapsulates a server side channel.


### Managing Exchanges and Queues [&#x219F;](#table-of-contents)

Once a channel has been established, the `kanin-chan` module can be used to
manage the fundamental objects within AMQP, namely exchanges and queues. The
following function creates an exchange called `my-exchange`, which by default,
is the direct exchange:

```cl
lfe> (set declare (make-exchange.declare exchange #"my-exchange"))
#(exchange.declare 0 #"my-exchange" #"direct" false false false false false ())
lfe> (kanin-chan:call chan declare)
#(exchange.declare_ok)
```

Similarly, a queue called `my-queue` is created by this code:

```cl
lfe> (set declare (make-queue.declare queue #"my-queue"))
#(queue.declare 0 #"my-queue" false false false false false ())
lfe> (kanin-chan:call chan declare)
#(queue.declare_ok #"my-queue" 0 0)
```

In many scenarios, a client is not interested in the actual name of the queue
it wishes to receive messages from. In this case, it is possible to let the
broker generate a random name for a queue. To do this, send a
`queue.declare` command and leave the queue attribute undefined:

```cl
lfe> (kanin-chan:call chan (make-queue.declare))
#(queue.declare_ok #"amq.gen-nQEU2b7bjcGEi39TsYxXcg" 0 0)
```

The server will auto-generate a queue name and return this name as part of the
acknowledgement.

To create a routing rule from an exchange to a queue, the `queue.bind`
command is used:


```cl
lfe> (set binding (make-queue.bind queue #"my-queue"
                                   exchange #"my-exchange"
                                   routing_key #"my-key"))
#(queue.bind 0 #"my-queue" #"my-exchange" #"my-key" false ())
lfe> (kanin-chan:call chan binding)
#(queue.bind_ok)
```

When this routing rule is no longer required, this route can be deleted using
the `queue.unbind` command:

```cl
lfe> (set binding (make-queue.unbind queue #"my-queue"
                                     exchange #"my-exchange"
                                     routing_key #"my-key"))
#(queue.unbind 0 #"my-queue" #"my-exchange" #"my-key" ())
lfe> (kanin-chan:call chan binding)
#(queue.unbind_ok)
```

An exchange can be deleted by the `exchange.delete` command:

```cl
lfe> (set delete (make-exchange.delete exchange #"my-exchange"))
#(exchange.delete 0 #"my-exchange" false false)
lfe> (kanin-chan:call chan delete)
#(exchange.delete_ok)
```

Similarly, a queue is deleted using the `queue.delete` command:

```cl
lfe> (set delete (make-queue.delete queue #"my-queue"))
#(queue.delete 0 #"my-queue" false false false)
lfe> (kanin-chan:call chan delete)
#(queue.delete_ok 0)
```

Note that we used `kanin-chan:call/2` in the examples above, since we sent
AMQP synchronous methods. It is generally advisable to use `kanin-
chan:call/{2,3}` for synchronous methods, rather than `kanin-chan:cast/{2,3}`,
even though both functions work with both sync and async method types. The
difference between the two functions is that `kanin-chan:call/{2,3}` blocks
the calling process until the reply comes back from the server (for sync
methods) or the method has been sent on the wire (for async methods), whereas
`kanin-chan:cast/{2,3}` returns `ok` immediately. Thus, only by using `kanin-
chan:call/{2,3}` do we have direct feedback when the broker has acknowledged
our command.


### Sending Messages [&#x219F;](#table-of-contents)

To send a message to an exchange with a particular routing key, the
`basic.publish` command in conjunction with the #amqp_msg{} record is used:

```cl
lfe> (set msg (make-amqp_msg payload #"foobar"))
#(amqp_msg
  #(P_basic ...)
  #"foobar")
lfe> (set pub (make-basic.publish exchange #"my-exchange" routing_key #"my-key"))
#(basic.publish 0 #"my-exchange" #"my-key" false false)
lfe> (kanin-chan:cast chan pub msg)
ok
```

By default, the properties field of the `amqp_msg` record contains a minimal
implementation of the `P_basic` properties structure. If an application
needs to override any of the defaults, for example, to send persistent
messages, the `amqp_msg` needs to be constructed accordingly:

```cl
lfe> (set payload #"foobaz")
#"foobaz"
lfe> (set pub (make-basic.publish exchange #"my-exchange" routing_key #"my-key"))
#(basic.publish 0 #"my-exchange" #"my-key" false false)
```

Set persistent delivery mode property:

```cl
lfe> (set basic-properties (make-P_basic delivery_mode 2))
#(P_basic ... 2 ...)
```

Continue with message creation and send:

```cl
lfe> (set msg (make-amqp_msg props basic-properties payload #"foobar"))
#(amqp_msg
  #(P_basic ... 2 ...)
  #"foobar")
lfe> (kanin-chan:cast chan pub msg)
ok
```

The full list of message headers is explained in the AMQP protocol
documentation.

Remember that the AMQP `basic.publish` command is asynchronous. This means
that the server will not send a response to it, unless the message is not
deliverable. In this case, the message will be returned to the client. This
operation is described in the "Handling Returned Messages" section below.

### Receiving Messages [&#x219F;](#table-of-contents)

The simplest way to receive a message is to poll an existing queue. This is
achieved using the `basic.get` command:

```cl
lfe> (set get-cmd (make-basic.get queue #"my-queue" no_ack 'true))
#(basic.get 0 #"my-queue" true)
lfe> (set `#(,(= (match-basic.get_ok) ok) ,content) (kanin-chan:call chan get-cmd))
#(#(basic.get_ok ...)
  #(amqp_msg
    #(P_basic ...)
    #"foobar"))
lfe> ok
#(basic.get_ok ...)
lfe> content
#(amqp_msg
  #(P_basic ...)
  #"foobar")
```

The payload that is returned is an Erlang binary, and it is up to the
application to decode it, as the structure of this content is opaque to the
AMQP protocol.

If the queue were empty when the `basic.get` command was invoked, then the
channel will return an `basic.get_empty` message, as illustrated here:

```cl
lfe> (set (= (match-basic.get_empty) content)
          (kanin-chan:call chan get-cmd))
#(basic.get_empty #"")
lfe> content
#(basic.get_empty #"")
```

Note that the previous example sets the `no_ack` flag on the `basic.get`
command. This tells the broker that the receiver will not send an
acknowledgement of the message. In doing so, the broker can absolve itself of
the responsibility for delivery - once it believes it has delivered a message,
then it is free to assume that consuming application has taken responsibility
for it. In general, a lot of applications will not want these semantics,
rather, they will want to explicitly acknowledge the receipt of a message.
This is done with the `basic.ack` command, where the `no_ack` field is turned
off by default:

```cl
lfe> (set get-cmd (make-basic.get queue #"my-queue"))
#(basic.get 0 #"my-queue" false)
lfe> (set `#(,(match-basic.get_ok delivery_tag msg-tag) ,content)
          (kanin-chan:call chan get-cmd))
#(...)
lfe> msg-tag
7
```

After the application has done what it needs to do with the response, it can
acknowledge the message:

```cl
lfe> (kanin-chan:cast chan (make-basic.ack delivery_tag msg-tag))
ok
```

Notice that we sent the `basic.ack` command using `kanin-chan:cast/2`
instead of `kanin-chan:call/2`. This is because the broker will not send a
response to an acknowledgement, i.e. it is a fire and forget command.

Receiving messages by polling a queue is not as as efficient as subscribing a
consumer to a queue, so consideration should be taken when receiving large
volumes of messages.


### Subscribing to Queues [&#x219F;](#table-of-contents)

TBD


### Subscribing Internals [&#x219F;](#table-of-contents)

The channel uses a module implementing the `amqp_gen_consumer` behaviour to
determine its behaviour with regard to subscribing related events.
Effectively, this modules handles client-side consumer registration and
routing of deliveries to the appropriate consumers.

For instance, the default consumer module, `amqp_selective_consumer`, keeps
track of which processes are subscribed to which queues and routes deliveries
appropriately; in addition, if the channel gives it a delivery for an unknown
consumer, it will pass it to a default consumer, should one be registered.

By contrast, `amqp_direct_consumer` simply forwards all the messages it
receives from the channel to its only registered consumer.

The consumer module for a channel is chosen when the channel is opened by
setting the second parameter to `kanin-conn:open-channel/2`. The consumer
module implements the `amqp_gen_consumer` behaviour and thus implements
functions to handle receiving `basic.consume`, `basic.consume_ok`,
`basic.cancel`, `basic.cancel_ok` methods as well as publishes.

See the API documentation for details.


### Closing Channels and the Connection [&#x219F;](#table-of-contents)

TBD


### Complete Example [&#x219F;](#table-of-contents)

TBD


### Delivery Flow Control [&#x219F;](#table-of-contents)

TBD


### Blocked Connections [&#x219F;](#table-of-contents)

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
