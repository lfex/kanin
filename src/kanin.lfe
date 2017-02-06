(defmodule kanin
  (export all))

(include-lib "kanin/include/amqp-client.lfe")

(defun start ()
  (kanin-server:start))

(defun start (uri)
  (kanin-server:start uri))

(defun start (uri genserver-opts)
  (kanin-server:start uri genserver-opts))

(defun stop ()
  (kanin-server:stop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Casts   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cast-placeholder ()
  (gen_server:cast 'kanin-server 'cast-placeholder))

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

(defun del-conn (conn-key)
  'noop)

;;; Channels

(defun get-chans ()
  (gen_server:call 'kanin-server 'chans))

(defun get-chan ()
  (gen_server:call 'kanin-server 'chan))

(defun get-chan (chan-key)
  (gen_server:call 'kanin-server `#(chan ,chan-key)))

(defun add-chan (conn chan-key)
  'noop)

(defun del-chan (chan-key)
  'noop)

;;; All

(defun close ()
  (gen_server:call 'kanin-server 'close))
