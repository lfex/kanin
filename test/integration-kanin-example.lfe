(defmodule integration-kanin-example
  (behaviour ltest-integration))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "kanin/include/amqp-client.lfe")

;; XXX This test works and runs with eunit, but due to an ltest runner issue
;;     (see https://github.com/lfex/ltest/issues/57), this can't be run with
;;     the test runner, and therefore, the integration test(s) can't be
;;     isolated. That, in turn, means it can't be run on Travis and must be set
;;     to skip (since RabbitMQ isn't set up on Travis, and I don't really want
;;     to).
(deftestskip example
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
      (is-equal #"foobar" msg-payload)
      ;; Ack the message
      (kanin-chan:cast chan (make-basic.ack delivery_tag tag)))
    ;; Close the channel
    (kanin-chan:close chan)
    ;; Close the connection
    (kanin-conn:close conn)))
