(defmodule kanin-uri
  (export all))

(include-lib "kanin/include/kanin-uri-macros.lfe")
(include-lib "amqp_client/include/amqp_client.hrl")

;; XXX These two functions should be pulled in via the macro instead of being
;; defined here. However, there seems to be an issue with the auth mechanism
;; parsing/setting when using amqp_uri:parse (in particular, the private
;; function amqp_uri:mechanisms/1 doesn't see to work probperly, neither in LFE
;; nor Erlang). For more info, see issue #4:
;;    https://github.com/billosys/kanin/issues/4
(defun parse (uri)
  (parse uri #b("/")))

(defun parse (uri vhost)
  (parse
    uri
    vhost
    (list #'amqp_auth_mechanisms:plain/3
          #'amqp_auth_mechanisms:amqplain/3)))

(defun parse (uri vhost auth-mechs)
  (let* ((`#(,status ,raw-opts) (amqp_uri:parse uri vhost))
         (opts (set-amqp_params_network-auth_mechanisms raw-opts auth-mechs)))
    `#(,status ,opts)))
