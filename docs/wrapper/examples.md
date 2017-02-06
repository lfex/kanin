# kanin RabbitMQ Documentation

## [Wrapper Libraries](index.md)

### Table of Contents

* [Examples](#examples-)
  * [Complete Example](#complete-example-)


### Examples [&#x219F;](#table-of-contents)

#### Complete Example [&#x219F;](#table-of-contents)


This shows a complete example:

```cl
(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "kanin/include/amqp-client.lfe")

(defun example ()
  (let* (;; Get connection options
         (`#(ok ,opts) (kanin-uri:parse "amqp://localhost"))
         ;; Start a network connection
         (`#(ok ,conn) (kanin-conn:start opts))
         ;; Open a channel on the connection
         (`#(ok ,chan) (kanin-conn:open-channel conn))
         (example-exchange #"example-exchange")
         (example-queue #"example-queue")
         (example-key #"example-key"))
    ;; Declare an exchange
    (kanin-chan:call
      chan
      (make-exchange.declare
        exchange example-exchange))
    ;; Declare a queue
    (kanin-chan:call
      chan
      (make-queue.declare
        queue example-queue))
    ;; Bind an exchange and queue with a routing key
    (kanin-chan:call
      chan
      (make-queue.bind
        queue example-queue
        exchange example-exchange
        routing_key example-key))
    ;; Publish a message
    (let ((msg (make-amqp_msg payload #"foobar"))
          (pub (make-basic.publish
                 exchange example-exchange
                 routing_key example-key)))
      (kanin-chan:cast chan pub msg))
    ;; Get the message back from the queue
    (let* ((get-cmd (make-basic.get queue example-queue))
          (`#(,(match-basic.get_ok delivery_tag tag)
              ,(match-amqp_msg payload msg-payload))
            (kanin-chan:call chan get-cmd)))
      ;; Do something with the message payload
      (io:format "~nGot message: ~p~n" `(,msg-payload))
      ;; Ack the message
      (kanin-chan:cast chan (make-basic.ack delivery_tag tag)))
    ;; Close the channel
    (kanin-chan:close chan)
    ;; Close the connection
    (kanin-conn:close conn)))
```

To run from the LFE REPL, simply paste the above, and then run `(example)`.

In the example above, we created an exchange and queue, and bound the the two.
We then created a message and published it to the exchange. The message was
then dequeued and acknowledged.

