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
;;;   Configuration   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun server-name () (MODULE))
(defun callback-module () (MODULE))
(defun initial-state () 0)
(defun genserver-opts () '())
(defun register-name () `#(local ,(server-name)))
(defun unknown-command () #(error "Unknown command."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Gen Server   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start ()
  (gen_server:start (register-name)
                    (callback-module)
                    (initial-state)
                    (genserver-opts)))

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
  (('call-placeholder _ state-data)
    `#(reply ,state-data ,state-data))
  (('stop _ state-data)
    `#(stop shutdown ok state-data))
  ((message _ state-data)
    `#(reply ,(unknown-command) ,state-data)))

(defun handle_info
  ((`#(EXIT ,_pid normal) state-data)
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
