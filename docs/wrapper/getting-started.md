# kanin RabbitMQ Documentation

## [Wrapper Libraries](index.md)

### Table of Contents

* [Getting Started](#getting-started-)
  * [The LFE AMQP Client Library](#the-lfe-amqp-client-library-)
  * [Programming Model](#programming-model-)
  * [AMQP Commands](#amqp-commands-)
  * [Including Header Files](#including-header-files-)


### Getting Started [&#x219F;](#table-of-contents)

#### The LFE AMQP Client Library [&#x219F;](#table-of-contents)

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


#### Programming Model [&#x219F;](#table-of-contents)

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


#### AMQP Commands [&#x219F;](#table-of-contents)

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


#### Including Header Files [&#x219F;](#table-of-contents)

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
