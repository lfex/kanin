;;;; This include file is only temporary, until a bug is fixed in LFE.
;;;; As such, everything is named just like things are named in
;;;; amqp_lib/amqp_client.hrl, since that will be used eventially and
;;;; this file will be deleted.
;;;;
;;;; For more information on the LFE bug, see the comment at the top of
;;;; src/kanin.lfe.
(include-lib "rabbit_common/include/rabbit.hrl")
(include-lib "kanin/include/rabbit-framing.lfe")

(defrecord amqp_msg
  (props (make-P_basic))
  (payload (binary)))

(defrecord amqp_params_network
  (username #"guest")
  (password #"guest")
  (virtual_host #"/")
  (host "localhost")
  (port 'undefined)
  (channel_max 0)
  (frame_max 0)
  (heartbeat 0)
  (connection_timeout 'infinity)
  (ssl_options 'none)
  (auth_mechanisms
    (list #'amqp_auth_mechanisms:plain/3
          #'amqp_auth_mechanisms:amqplain/3))
  (client_properties '())
  (socket_options '()))

(defrecord amqp_params_direct
  (username 'none)
  (password 'none)
  (virtual_host #"/")
  (node (node))
  (adapter_info 'none)
  (client_properties '()))

(defrecord amqp_adapter_info
  (host 'unknown)
  (port 'unknown)
  (peer_host 'unknown)
  (peer_port 'unknown)
  (name 'unknown)
  (protocol 'unknown)
  (additional_info '()))
