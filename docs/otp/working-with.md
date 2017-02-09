# kanin RabbitMQ Documentation

## [OTP Library](index.md)

*High-level kanin API for RabbitMQ/AMQP*


### Table of Contents

* [Working with Channels, Exchanges, Queues, and Messages](#)
  * [Creating Channels](#creating-hannels-)
  * [Managing Exchanges and Queues](#managing-exchanges-and-queues-)
  * [Sending Messages](#sending-messages-)
  * [Receiving Messages](#receiving-messages-)
  * [Subscribing to Queues](#subscribing-to-queues-)
  * [Subscribing Internals](#subscribing-internals-)
  * [Closing Channels and the Connection](#closing-channels-and-the-connection-)


### Working with Exchanges, Queues, and Messages [&#x219F;](#table-of-contents)


#### Creating Channels [&#x219F;](#table-of-contents)

#### Managing Exchanges and Queues [&#x219F;](#table-of-contents)

Once the kanin application has started, the `kanin` module can be used to
manage the fundamental objects within AMQP, namely exchanges and queues. The
following function creates an exchange called `my-exchange`, which by default,
is the direct exchange:

```cl
lfe> (set declare (make-exchange.declare exchange #"my-exchange"))
#(exchange.declare 0 #"my-exchange" #"direct" false false false false false ())
lfe> (kanin:call declare)
#(exchange.declare_ok)
```

Similarly, a queue called `my-queue` is created by this code:

```cl
lfe> (set declare (make-queue.declare queue #"my-queue"))
#(queue.declare 0 #"my-queue" false false false false false ())
lfe> (kanin:call declare)
#(queue.declare_ok #"my-queue" 0 0)
```

In many scenarios, a client is not interested in the actual name of the queue
it wishes to receive messages from. In this case, it is possible to let the
broker generate a random name for a queue. To do this, send a
`queue.declare` command and leave the queue attribute undefined:

```cl
lfe> (kanin:call (make-queue.declare))
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
lfe> (kanin:call binding)
#(queue.bind_ok)
```

When this routing rule is no longer required, this route can be deleted using
the `queue.unbind` command:

```cl
lfe> (set binding (make-queue.unbind queue #"my-queue"
                                     exchange #"my-exchange"
                                     routing_key #"my-key"))
#(queue.unbind 0 #"my-queue" #"my-exchange" #"my-key" ())
lfe> (kanin:call binding)
#(queue.unbind_ok)
```

An exchange can be deleted by the `exchange.delete` command:

```cl
lfe> (set delete (make-exchange.delete exchange #"my-exchange"))
#(exchange.delete 0 #"my-exchange" false false)
lfe> (kanin:call delete)
#(exchange.delete_ok)
```

Similarly, a queue is deleted using the `queue.delete` command:

```cl
lfe> (set delete (make-queue.delete queue #"my-queue"))
#(queue.delete 0 #"my-queue" false false false)
lfe> (kanin:call delete)
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


#### Sending Messages [&#x219F;](#table-of-contents)

To send a message to an exchange with a particular routing key, the
`basic.publish` command in conjunction with the #amqp_msg{} record is used:

```cl
lfe> (set msg (make-amqp_msg payload #"foobar"))
#(amqp_msg
  #(P_basic ...)
  #"foobar")
lfe> (set pub (make-basic.publish exchange #"my-exchange" routing_key #"my-key"))
#(basic.publish 0 #"my-exchange" #"my-key" false false)
lfe> (kanin:cast pub msg)
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
lfe> (kanin:cast pub msg)
ok
```

The full list of message headers is explained in the AMQP protocol
documentation.

Remember that the AMQP `basic.publish` command is asynchronous. This means
that the server will not send a response to it, unless the message is not
deliverable. In this case, the message will be returned to the client. This
operation is described in the "Handling Returned Messages" section below.


#### Receiving Messages [&#x219F;](#table-of-contents)

The simplest way to receive a message is to poll an existing queue. This is
achieved using the `basic.get` command:

```cl
lfe> (set get-cmd (make-basic.get queue #"my-queue" no_ack 'true))
#(basic.get 0 #"my-queue" true)
lfe> (set `#(,(= (match-basic.get_ok) ok) ,content) (kanin:call get-cmd))
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

If the queue was empty when the `basic.get` command was invoked, then the
channel will return an `basic.get_empty` message, as illustrated here:

```cl
lfe> (set (= (match-basic.get_empty) content) (kanin:call get-cmd))
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
          (kanin:call get-cmd))
#(...)
lfe> msg-tag
7
```

After the application has done what it needs to do with the response, it can
acknowledge the message:

```cl
lfe> (kanin:cast (make-basic.ack delivery_tag msg-tag))
ok
```

Notice that we sent the `basic.ack` command using `kanin:cast/1`
instead of `kanin:call/1`. This is because the broker will not send a
response to an acknowledgement, i.e. it is a fire and forget command.

Receiving messages by polling a queue is not as as efficient as subscribing a
consumer to a queue, so consideration should be taken when receiving large
volumes of messages.


#### Subscribing to Queues [&#x219F;](#table-of-contents)

As indicated in the "Receiving Messages" section, subscribing to a queue can
be a more efficient means of consuming messages than the polling mechanism. To
subscribe to a queue, the `basic.consume` command is used in one of two
forms:

```cl
lfe> (set sub (make-basic.consume queue #"my-queue"))
#(basic.consume 0 #"my-queue" #"" false false false false ())
lfe> (set (match-basic.consume_ok consumer_tag cnsm-tag)
          (kanin:subscribe sub consumer-pid))
```

or

```cl
lfe> (set sub (make-basic.consume queue #"my-queue"))
#(basic.consume 0 #"my-queue" #"" false false false false ())
lfe> (set (match-basic.consume_ok consumer_tag cnsm-tag)
          (kanin:call sub))
#(basic.consume_ok #"amq.ctag-QDTEY6V7duBFu_k86wayzg")
lfe> cnsm-tag
#"amq.ctag-QDTEY6V7duBFu_k86wayzg"
```

The `consumer-pid` argument is the pid of a process to which the client
library will deliver messages. This can be an arbitrary Erlang process,
including the process that initiated the subscription. The `basic.consume_ok`
notification contains a tag that identifies the subscription. This is used at
a later point in time to cancel the subscription. This notification is sent
both to the process that created the subscription (as the return value to
`kanin-chan:subscribe/3`) and as a message to the consumer process.

When a consumer process is subscribed to a queue, it will receive messages in
its mailbox. An example receive loop looks like this:

```cl
(defun loop ()
  (receive
    ;; This is the first message received
    ((match-basic.consume_ok)
      (loop))
    ;; This is received when the subscription is cancelled
    ((match-basic.cancel_ok)
      'ok)
    ;; A delivery
    (`#(,(match-basic.deliver delivery_tag tag) ,content)
      ;; do somehting with the message payload here ...
      (lfe_io:format "Payload: ~p~n" `(,content))
      (kanin:cast (make-basic.ack delivery_tag tag))
      (loop))))
```

In this simple example, the process consumes the subscription notification and
then proceeds to wait for delivery messages to arrive in its mailbox. When
messages are received from the mailbox, the loop does something useful with
the message and sends a receipt acknowledge back to the broker. If the
subscription is cancelled, either by the consumer itself or some other
process, a cancellation notification will be sent to the consumer process. In
this scenario, the receive loop just exits. If the application does not wish
to explicitly acknowledge message receipts, it should set the `no_ack` flag on
the subscription request.

To run the loop, spawn the function, subscribe the pid, and publish some
messages:

```cl
lfe> (set consumer-pid (spawn #'loop/0))
<0.141.0>
lfe> (kanin:subscribe sub consumer-pid)
#(basic.consume_ok #"amq.ctag-wGk8K-6_YH-ovKODkDZQKA")
lfe> (kanin:cast pub msg)
ok
Payload: #(amqp_msg
           #(P_basic ...)
           #"foobar")
```

To cancel a subscription, use the tag that the broker passed back with the
`basic.consume_ok` acknowledgement:

```cl
lfe> (kanin:call (make-basic.cancel consumer_tag tag))
```


#### Closing Channels and the Connection [&#x219F;](#table-of-contents)

When a channel is no longer required, a client should close it. This is
achieved using `kanin:close/1`. Here's how you'd close the default kanin
channel:

```cl
lfe> (kanin:close #(chan default))
ok
```

To close the connection, the same function is used, but with the connection
keyword:

```cl
lfe> (kanin:close #(conn default))
ok
```

If you wish to close all channels and connections, you can use `kanin:close/0`:

```cl
lfe> (kanin:close)
ok
```

Note at this point you will still have a running kanin application (and its
gen-server), but with no open connections and channels. If you wish to keep
using this server, you will need to add one or more connections and channels,
e.g.:

```cl
lfe> (kanin:add-conn 'default)
<0.146.0>
lfe> (kanin:add-chan 'default)
<0.156.0>
```

If you wish to close all channels/connections as well as stop the
application, you can accomplish all with one call:

```cl
lfe> (kanin:stop)
ok
```
