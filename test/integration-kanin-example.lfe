(defmodule integration-kanin-example
  (behaviour ltest-integration))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "kanin/include/amqp-client.lfe")

(deftest example
  (kanin:start)
  (let ((example-exchange #"example-exchange")
        (example-queue #"example-queue")
        (example-key #"example-key"))
    ;; Declare an exchange
    (kanin:call
      (make-exchange.declare
        exchange example-exchange))
    ;; Declare a queue
    (kanin:call
      (make-queue.declare
        queue example-queue))
    ;; Bind an exchange and queue with a routing key
    (kanin:call
      (make-queue.bind
        queue example-queue
        exchange example-exchange
        routing_key example-key))
    ;; Publish a message
    (let ((msg (make-amqp_msg payload #"foobar"))
          (pub (make-basic.publish
                 exchange example-exchange
                 routing_key example-key)))
      (kanin:cast pub msg))
    ;; Get the message back from the queue
    (let* ((get-cmd (make-basic.get queue example-queue))
          (`#(,(match-basic.get_ok delivery_tag tag)
              ,(match-amqp_msg payload msg-payload))
            (kanin:call get-cmd)))
      ;; Do something with the message payload
      (is-equal #"foobar" msg-payload)
      ;; Ack the message
      (kanin:cast (make-basic.ack delivery_tag tag))))
  ;; Close the channel(s) and connection(s)
  (kanin:close))
