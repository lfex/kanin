(eval-when-compile

  (defun get-api-funcs ()
    '((call 2) (call 3)
      (cast 2) (cast 3)
      (cast-flow 3)
      (close 1) (close 3)
      (register-return-handler 2)
      (unregister-return-handler 1)
      (register-flow-handler 2)
      (unregister-flow-handler 1)
      (register-confirm-handler 2)
      (unregister-confirm-handler 1)
      (call-consumer 2)
      (subscribe 3)
      (next-publish-seqno 1)
      (wait-for-confirms 1) (wait-for-confirms 2)
      (wait-for-confirms-or-die 1) (wait-for-confirms-or-die 2)
      (start-link 5)
      (set-writer 2)
      (connection-closing 3)
      (open 1)
      (init 1)
      (terminate 2)
      (code-change 3)
      (handle-call 3)
      (handle-cast 2)
      (handle-info 2))))

(defmacro generate-api ()
  `(progn ,@(kla:make-funcs (get-api-funcs) 'amqp_channel)))

(generate-api)

(defun loaded-kanin-chan-macros ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).
  This function needs to be the last one in this include."
  'ok)
