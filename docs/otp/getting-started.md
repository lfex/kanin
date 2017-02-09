# kanin RabbitMQ Documentation

## [OTP Library](index.md)

*High-level kanin API for RabbitMQ/AMQP*


### Table of Contents

* [Getting Started](#getting-started-)
  * [The LFE AMQP Client Library](#the-lfe-amqp-client-library-)
  * [Programming Model](#programming-model-)
  * [AMQP Commands](#amqp-commands-)
  * [Including Header Files](#including-header-files-)


### Getting Started [&#x219F;](#table-of-contents)

#### The LFE AMQP Client Library [&#x219F;](#table-of-contents)

The kanin AMQP client provides an LFE interface to compliant AMQP brokers. The
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

 * Run a kanin application (this is a long-running process -- `gen_server` in
   this case) which:
   * creates a default connection to a broker
   * creates a default channel with the default connection, and
   * maintains connection(s)/channel(s) state
 * Execute AMQP commands such as sending and receiving messages,
   creating exchanges and queue or defining routing rules between exchanges and
   queues
 * When no longer required, stop the long-running process


#### Programming Model [&#x219F;](#table-of-contents)

Once the kanin application is started, an LFE application will typically use
the `kanin:call/{1,2,3}` and `kanin:cast/{1,2,3}` functions to achieve most of
the things it needs to do. Using OTP internals (i.e., `gen_server:call` and
`gen_server:cast`), these functions call into the kanin application's
long-running process to execute AMQP methods using the application state (which
includes potentially many connections and channels, referencable by name).


#### AMQP Commands [&#x219F;](#table-of-contents)

The general mechanism of interacting with the broker is to send and receive
AMQP commands that are defined in the protocol documentation. During build
process, the machine-readable version of the AMQP specification is used to
auto-generate Erlang records for each command. The code generation process
also defines sensible default values for each command. Using default values
allows the programmer to write terser code - it is only necessary to override
a field if you require non-default behaviour. The definition of each command
can be consulted in the `kanin/include/rabbit-framing.lfe` header file. For
example, when using the `(make-exchange.declare ...)` record-creating macro,
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
