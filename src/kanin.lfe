(defmodule kanin
  (behaviour application)
  (export all))

(include-lib "kanin/include/amqp-client.lfe")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Management   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start ()
  (start 'normal '()))

(defun start
  (('normal '())
    (kanin-server:start))
  (('normal `#(,uri))
    (kanin-server:start uri))
  (('normal `#(,uri ,genserver-opts))
    (kanin-server:start uri genserver-opts)))

(defun start_link ()
  (start))

(defun stop ()
  (stop '()))

(defun stop (_state)
  (kanin-server:stop)
  'ok)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Casts   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cast (amqp-method)
  (gen_server:cast 'kanin-server `#(cast ,amqp-method none default)))

(defun cast (amqp-method content)
  (gen_server:cast 'kanin-server `#(cast ,amqp-method ,content default)))

(defun cast (amqp-method content chan-key)
  (gen_server:cast 'kanin-server `#(cast ,amqp-method ,content ,chan-key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Calls   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Configuration

(defun get-uri ()
  (gen_server:call 'kanin-server 'uri))

(defun get-opts ()
  (gen_server:call 'kanin-server 'opts))

(defun get-state ()
  (gen_server:call 'kanin-server 'state))

;;; Connections

(defun get-conns ()
  (gen_server:call 'kanin-server 'conns))

(defun get-conn ()
  (gen_server:call 'kanin-server 'conn))

(defun get-conn (conn-key)
  (gen_server:call 'kanin-server `#(conn ,conn-key)))

(defun add-conn (conn-key)
  (gen_server:call 'kanin-server `#(add-conn ,conn-key)))

;;; Channels

(defun get-chans ()
  (gen_server:call 'kanin-server 'chans))

(defun get-chan ()
  (gen_server:call 'kanin-server 'chan))

(defun get-chan (chan-key)
  (gen_server:call 'kanin-server `#(chan ,chan-key)))

(defun add-chan (chan-key)
  (gen_server:call 'kanin-server `#(add-chan ,chan-key)))

(defun add-chan (conn-key chan-key)
  (gen_server:call 'kanin-server `#(add-chan ,conn-key ,chan-key)))

;;; Calls  &tc.

(defun call (amqp-method)
  (gen_server:call 'kanin-server `#(call ,amqp-method none default)))

(defun call (amqp-method content)
  (gen_server:call 'kanin-server `#(call ,amqp-method ,content default)))

(defun call (amqp-method content chan-key)
  (gen_server:call 'kanin-server `#(call ,amqp-method ,content ,chan-key)))

(defun subscribe (amqp-method subscriber)
  (gen_server:call
    'kanin-server
    `#(subscribe ,amqp-method ,subscriber default)))

(defun subscribe (amqp-method subscriber chan-key)
  (gen_server:call
    'kanin-server
    `#(subscribe ,amqp-method ,subscriber ,chan-key)))

;;; All

(defun close ()
  (gen_server:call 'kanin-server 'close))
