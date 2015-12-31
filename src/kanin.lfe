(defmodule kanin
  (export all))

(include-lib "amqp_client/include/amqp_client.hrl")

(defun noop ()
  'noop)
