(defmodule kanin-server
  (behaviour gen_server)
  (export
    ;; gen_server implementation
    (start 0)
    (stop 0)
    ;; callback implementation
    (init 1)
    (handle_call 3)
    (handle_cast 2)
    (handle_info 2)
    (terminate 2)
    (code_change 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Configuration & State   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun server-name () (MODULE))
(defun callback-module () (MODULE))
(defun register-name () `#(local ,(server-name)))
(defun unknown-command () #(error "Unknown command."))
(defun default-uri () "amqp://localhost")
(defun default-genserver-opts () '())

(defrecord server-state
  (uri "")
  (conn-opts '())
  (conns '())
  (chans '()))

;;; Configuration

(defun get-uri (state)
  (server-state-uri state))

(defun get-opts (state)
  (server-state-conn-opts state))

;;; Connections

(defun get-connections (state)
  (server-state-conns state))

(defun get-connection (state conn-key)
  (proplists:get_value conn-key (get-connections state)))

(defun get-default-connection (state)
  (get-connection state 'default))

(defun add-connection (state conn-key)
  (let ((`#(ok ,conn) (kanin-conn:start (get-opts state))))
    (set-server-state-conns
      state
      (lists:merge
        (get-connections state)
        `(#(,conn-key ,conn))))))

(defun delete-connection (state conn-key)
  'noop)

;;; Channels

(defun get-channels (state)
  (server-state-chans state))

(defun get-channel (state chan-key)
  (proplists:get_value 'default (get-channels state)))

(defun get-default-channel (state)
  (get-channel state 'default))

(defun add-channel (state conn-key chan-key)
  'noop)

(defun delete-channel (state chan-key)
  'noop)

;;; Initialization

(defun init-state (uri)
  (let* ((`#(ok ,opts) (kanin-uri:parse uri))
         (`#(ok ,conn) (kanin-conn:start opts))
         (`#(ok ,chan) (kanin-conn:open-channel conn)))
    (make-server-state
      uri uri
      conn-opts opts
      conns `(#(default ,conn))
      chans `(#(default ,chan)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Gen Server   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start ()
  (start (default-uri)))

(defun start (uri)
  (start uri (default-genserver-opts)))

(defun start (uri genserver-opts)
  (gen_server:start (register-name)
                    (callback-module)
                    (init-state uri)
                    genserver-opts))

(defun stop ()
  (gen_server:call (server-name) 'stop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Callbacks   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init (initial-state)
  `#(ok ,initial-state))

(defun handle_cast
  (('cast-placeholder state-data)
    `#(noreply ,state-data)))

(defun handle_call
  ;; Config
  (('uri _ state-data)
    `#(reply ,(get-uri state-data) ,state-data))
  (('opts _ state-data)
    `#(reply ,(get-opts state-data) ,state-data))
  (('state _ state-data)
    `#(reply ,state-data ,state-data))
  ;; Connections
  (('conns _ state-data)
    `#(reply ,(get-connections state-data) ,state-data))
  (('conn _ state-data)
    `#(reply ,(get-default-connection state-data) ,state-data))
  ((`#(conn ,key) _ state-data)
    `#(reply ,(get-connection state-data key) ,state-data))
  ((`#(add-conn ,key) _ state-data)
    (let ((new-state (add-connection state-data key)))
      `#(reply ,(get-connection new-state key) ,new-state)))
  ;; Channels
  (('chans _ state-data)
    `#(reply ,(get-channels state-data) ,state-data))
  (('chan _ state-data)
    `#(reply ,(get-default-channel state-data) ,state-data))
  ((`#(chan ,key) _ state-data)
    `#(reply ,(get-channel state-data key) ,state-data))
  ;; Other
  (('stop _ state-data)
    `#(stop shutdown ok state-data))
  ((message _ state-data)
    `#(reply ,(unknown-command) ,state-data)))

(defun handle_info
  ((`#(EXIT ,_ normal) state-data)
   `#(noreply ,state-data))
  ((`#(EXIT ,pid ,reason) state-data)
   (io:format "Process ~p exited! (Reason: ~p)~n" `(,pid ,reason))
   `#(noreply ,state-data))
  ((_msg state-data)
   `#(noreply ,state-data)))

(defun terminate (_reason _state-data)
  'ok)

(defun code_change (_old-version state _extra)
  `#(ok ,state))
