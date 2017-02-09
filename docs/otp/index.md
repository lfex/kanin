# kanin RabbitMQ Documentation

## OTP Library

*High-level kanin API for RabbitMQ/AMQP*


### Table of Contents

* [Copyright Notice](#copyright-notice-)
* [Modules](#supported-modules-)
* [Getting Started](getting-started.md)
  * The LFE AMQP Client Library
  * Programming Model
  * AMQP Commands
  * Including Header Files
* [Connecting](connecting.md)
  * Connecting to a Broker
  * Connecting To A Broker with AMQP URIs
* [Channels, Exchanges, Queues, and Messages](working-with.md)
  * Creating Channels
  * Managing Exchanges and Queues
  * Sending Messages
  * Receiving Messages
  * Subscribing to Queues
  * Subscribing Internals
  * Closing Channels and the Connection
* [Examples](examples.md)
  * Complete Example
* [Flow Control](flow-control.md)
  * Delivery Flow Control
  * Blocked Connections
  * Handling Returned Messages


### Modules [&#x219F;](#table-of-contents)

The high-level kanin API is meant to be used from a single module: `kanin`.
Under the covers, it makes use of the following modules, however:
 * `kanin-server`
 * TBD

Not all functions from `kanin-conn` and `kanin-chan` are implemented in the
`kanin` module. If one is missing that you would like added, feel free to
submit a new ticket requesting the addition of your desired module(s) or
submit a pull request with the desired inclusion added.


### Copyright Notice [&#x219F;](#table-of-contents)

The following content was copied from
[the low-level API kanin Client Guide](../wrapper/index.md),
which, in turn, was copied from
[the Erlang Client User Guide](https://www.rabbitmq.com/erlang-client-user-guide.html)
on the [RabbitMQ site](https://www.rabbitmq.com/).
The original copyright was in 2014, held by Pivotal Software, Inc.
