(defun PROTOCOL_PORT () 5672)
(defun FRAME_METHOD () 1)
(defun FRAME_HEADER () 2)
(defun FRAME_BODY () 3)
(defun FRAME_HEARTBEAT () 8)
(defun FRAME_MIN_SIZE () 4096)
(defun FRAME_END () 206)
(defun REPLY_SUCCESS () 200)
(defun CONTENT_TOO_LARGE () 311)
(defun NO_ROUTE () 312)
(defun NO_CONSUMERS () 313)
(defun ACCESS_REFUSED () 403)
(defun NOT_FOUND () 404)
(defun RESOURCE_LOCKED () 405)
(defun PRECONDITION_FAILED () 406)
(defun CONNECTION_FORCED () 320)
(defun INVALID_PATH () 402)
(defun FRAME_ERROR () 501)
(defun SYNTAX_ERROR () 502)
(defun COMMAND_INVALID () 503)
(defun CHANNEL_ERROR () 504)
(defun UNEXPECTED_FRAME () 505)
(defun RESOURCE_ERROR () 506)
(defun NOT_ALLOWED () 530)
(defun NOT_IMPLEMENTED () 540)
(defun INTERNAL_ERROR () 541)
(defun FRAME_OOB_METHOD () 4)
(defun FRAME_OOB_HEADER () 5)
(defun FRAME_OOB_BODY () 6)
(defun FRAME_TRACE () 7)
(defun NOT_DELIVERED () 310)

;; Method field records
(defrecord connection.start
  (version_major 0)
  (version_minor 9)
  server_properties
  (mechanisms #"PLAIN")
  (locales #"en_US"))

(defrecord connection.start_ok
  client_properties
  (mechanism #"PLAIN")
  response
  (locale #"en_US"))

(defrecord connection.secure
  challenge)

(defrecord connection.secure_ok
  response)

(defrecord connection.tune
  (channel_max 0)
  (frame_max 0)
  (heartbeat 0))

(defrecord connection.tune_ok
  (channel_max 0)
  (frame_max 0)
  (heartbeat 0))

(defrecord connection.open
  (virtual_host #"/")
  (capabilities #"")
  (insist 'false))

(defrecord connection.open_ok
  (known_hosts #""))

(defrecord connection.close
  reply_code
  (reply_text #"")
  class_id
  method_id)

(defrecord connection.close_ok
  data)

(defrecord connection.blocked
  (reason #""))

(defrecord connection.unblocked
  data)

(defrecord connection.redirect
  host
  (known_hosts #""))

(defrecord channel.open
  (out_of_band #""))

(defrecord channel.open_ok
  (channel_id #""))

(defrecord channel.flow
  active)

(defrecord channel.flow_ok
  active)

(defrecord channel.close
  reply_code
  (reply_text (binary))
  class_id
  method_id)

(defrecord channel.close_ok
  data)

(defrecord channel.alert
  reply_code
  (reply_text #"")
  (details '()))

(defrecord access.request
  (realm  #"/data")
  (exclusive 'false)
  (passive 'true)
  (active 'true)
  (write 'true)
  (read 'true))

(defrecord access.request_ok
  (ticket 1))

(defrecord exchange.declare
  (ticket 0)
  exchange
  (type #"direct")
  (passive 'false)
  (durable 'false)
  (auto_delete 'false)
  (internal 'false)
  (nowait 'false)
  (arguments '()))

(defrecord exchange.declare_ok
  data)

(defrecord exchange.delete
  (ticket 0)
  exchange
  (if_unused 'false)
  (nowait 'false))

(defrecord exchange.delete_ok
  data)

(defrecord exchange.bind
  (ticket 0)
  destination
  source
  (routing_key #"")
  (nowait 'false)
  (arguments '()))

(defrecord exchange.bind_ok
  data)

(defrecord exchange.unbind
  (ticket 0)
  destination
  source
  (routing_key #"")
  (nowait 'false)
  (arguments '()))

(defrecord exchange.unbind_ok
  data)

(defrecord queue.declare
  (ticket 0)
  (queue #"")
  (passive 'false)
  (durable 'false)
  (exclusive 'false)
  (auto_delete 'false)
  (nowait 'false)
  (arguments '()))

(defrecord queue.declare_ok
  queue
  message_count
  consumer_count)

(defrecord queue.bind
  (ticket 0)
  (queue #"")
  exchange
  (routing_key #"")
  (nowait 'false)
  (arguments '()))

(defrecord queue.bind_ok
  data)

(defrecord queue.purge
  (ticket 0)
  (queue #"")
  (nowait 'false))

(defrecord queue.purge_ok
  message_count)

(defrecord queue.delete
  (ticket 0)
  (queue #"")
  (if_unused 'false)
  (if_empty 'false)
  (nowait 'false))

(defrecord queue.delete_ok
  message_count)

(defrecord queue.unbind
  (ticket 0)
  (queue #"")
  exchange
  (routing_key #"")
  (arguments '()))

(defrecord queue.unbind_ok
  data)

(defrecord basic.qos
  (prefetch_size 0)
  (prefetch_count 0)
  (global 'false))

(defrecord basic.qos_ok
  data)

(defrecord basic.consume
  (ticket 0)
  (queue #"")
  (consumer_tag #"")
  (no_local 'false)
  (no_ack 'false)
  (exclusive 'false)
  (nowait 'false)
  (arguments '()))

(defrecord basic.consume_ok
  consumer_tag)

(defrecord basic.cancel
  consumer_tag
  nowait false)

(defrecord basic.cancel_ok
  consumer_tag)

(defrecord basic.publish
  (ticket 0)
  (exchange #"")
  (routing_key #"")
  (mandatory 'false)
  (immediate 'false))

(defrecord basic.return
  reply_code
  (reply_text #"")
  exchange
  routing_key)

(defrecord basic.deliver
  consumer_tag
  delivery_tag
  (redelivered 'false)
  exchange
  routing_key)

(defrecord basic.get
  (ticket 0)
  (queue #"")
  (no_ack 'false))

(defrecord basic.get_ok
  delivery_tag
  (redelivered 'false)
  exchange
  routing_key
  message_count)

(defrecord basic.get_empty
  (cluster_id #""))

(defrecord basic.ack
  (delivery_tag 0)
  (multiple 'false))

(defrecord basic.reject
  delivery_tag
  (requeue 'true))

(defrecord basic.recover_async
  (requeue 'false))

(defrecord basic.recover
  (requeue 'false))

(defrecord basic.recover_ok
  data)

(defrecord basic.nack
  (delivery_tag 0)
  (multiple 'false)
  (requeue 'true))

(defrecord basic.credit
  (consumer_tag #"")
  credit
  drain)

(defrecord basic.credit_ok
  available)

(defrecord basic.credit_drained
  (consumer_tag #"")
  credit_drained)

(defrecord tx.select
  data)

(defrecord tx.select_ok
  data)

(defrecord tx.commit
  data)

(defrecord tx.commit_ok
  data)

(defrecord tx.rollback
  data)

(defrecord tx.rollback_ok
  data)

(defrecord confirm.select
  (nowait 'false))

(defrecord confirm.select_ok
  data)

(defrecord file.qos
  (prefetch_size 0)
  (prefetch_count 0)
  (global 'false))

(defrecord file.qos_ok
  data)

(defrecord file.consume
  (ticket 1)
  (queue #"")
  (consumer_tag #"")
  (no_local 'false)
  (no_ack 'false)
  (exclusive 'false)
  (nowait 'false))

(defrecord file.consume_ok
  consumer_tag)

(defrecord file.cancel
  consumer_tag
  (nowait 'false))

(defrecord file.cancel_ok
  consumer_tag)

(defrecord file.open
  identifier
  content_size)

(defrecord file.open_ok
  staged_size)

(defrecord file.stage
  data)

(defrecord file.publish
  (ticket 1)
  (exchange #"")
  (routing_key #"")
  (mandatory 'false)
  (immediate 'false)
  identifier)

(defrecord file.return
  (reply_code 200)
  (reply_text #"")
  exchange
  routing_key)

(defrecord file.deliver
  consumer_tag
  delivery_tag
  (redelivered 'false)
  exchange
  routing_key
  identifier)

(defrecord file.ack
  (delivery_tag 0)
  (multiple 'false))

(defrecord file.reject
  delivery_tag
  (requeue 'true))

(defrecord stream.qos
  (prefetch_size 0)
  (prefetch_count 0)
  (consume_rate 0)
  (global 'false))

(defrecord stream.qos_ok
  data)

(defrecord stream.consume
  (ticket 1)
  (queue #"")
  (consumer_tag #"")
  (no_local 'false)
  (exclusive 'false)
  (nowait 'false))

(defrecord stream.consume_ok
  consumer_tag)

(defrecord stream.cancel
  consumer_tag
  (nowait 'false))

(defrecord stream.cancel_ok
  consumer_tag)

(defrecord stream.publish
  (ticket 1)
  (exchange #"")
  (routing_key #"")
  (mandatory 'false)
  (immediate 'false))

(defrecord stream.return
  (reply_code 200)
  (reply_text #"")
  exchange
  routing_key)

(defrecord stream.deliver
  consumer_tag
  delivery_tag
  exchange
  queue)

(defrecord dtx.select
  data)

(defrecord dtx.select_ok
  data)

(defrecord dtx.start
  dtx_identifier)

(defrecord dtx.start_ok
  data)

(defrecord tunnel.request
  meta_data)

(defrecord test.integer
  integer_1
  integer_2
  integer_3
  integer_4
  operation)

(defrecord test.integer_ok
  result)

(defrecord test.string
  string_1
  string_2
  operation)

(defrecord test.string_ok
  result)

(defrecord test.table
  table
  integer_op
  string_op)

(defrecord test.table_ok
  integer_result
  string_result)

(defrecord test.content
  data)

(defrecord test.content_ok
  content_checksum)

;; Class property records
(defrecord P_connection
  data)

(defrecord P_channel
  data)

(defrecord P_access
  data)

(defrecord P_exchange
  data)

(defrecord P_queue
  data)

(defrecord P_basic
  content_type
  content_encoding
  headers
  delivery_mode
  priority
  correlation_id
  reply_to
  expiration
  message_id
  timestamp
  type
  user_id
  app_id
  cluster_id)

(defrecord P_tx
  data)

(defrecord P_confirm
  data)

(defrecord P_file
  content_type
  content_encoding
  headers
  priority
  reply_to
  message_id
  filename
  timestamp
  cluster_id)

(defrecord P_stream
  content_type
  content_encoding
  headers
  priority
  timestamp)

(defrecord P_dtx
  data)

(defrecord P_tunnel
  headers
  proxy_name
  data_name
  durable
  broadcast)

(defrecord P_test
  data)

(defun loaded-rabbit-framing ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).
  This function needs to be the last one in this include."
  'ok)
