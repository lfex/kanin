# kanin RabbitMQ Documentation

## [Wrapper Libraries](index.md)

*Low-level kanin API for RabbitMQ/AMQP*


### Table of Contents

* [Flow Control](#flow-control-)
  * [Delivery Flow Control](#delivery-flow-control-)
  * [Blocked Connections](#blocked-connections-)
  * [Handling Returned Messages](#handling-returned-messages-)


### Flow Control [&#x219F;](#table-of-contents)

#### Delivery Flow Control [&#x219F;](#table-of-contents)

By default, there is no flow control within a channel other than normal TCP
back-pressure. A consumer can set the size of the prefetch buffer that the
broker will maintain for outstanding unacknowledged messages on a single
channel. This is achieved using the `basic.qos` command:

```cl
lfe> (kanin-chan:call
       chan
       (make-basic.qos prefetch_count prefetch))
```

Applications typically should set the prefetch count, which means the
processing speed of the consumer will exert back-pressure on the flow of
messages in that channel.


#### Blocked Connections [&#x219F;](#table-of-contents)

When an AMQP broker is running low on resources, for example by hitting a
memory high watermark, it may choose to stop reading from publishers' network
sockets.

RabbitMQ introduces a mechanism to allow clients to be told this has taken
place - invoke `kanin-conn:register-blocked-handler/2` giving the pid of a
process to which `(make-connection.blocked)` and `(make-connection.unblocked)`
messages may be sent.


#### Handling Returned Messages [&#x219F;](#table-of-contents)

The broker will return undeliverable messages back to the originating client.
These are messages published either with the immediate or mandatory flags set.
In order for the application to get notified of a return, it must register a
callback process that can process `(make-basic.return)` commands. Here is an
example of unrouteable message:

```cl
(kanin-chan:register-return-handler chan (self))
(kanin-chan:call
  chan
  (make-exchange.declare exchange some-name))
(let ((pub (make-basic.publish
             exchange some-name
             routing_key some-key
             mandatory 'true)))
  (kanin-chan:call chan pub (make-amqp_msg payload #"some payload"))
  (receive
    (`#(,(match-basic.return reply_text #"unroutable" exchange some-name)
        ,content)
      ;; Do something with the returned message
      ...)))
      (make-basic.return)))))
```
