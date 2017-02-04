(eval-when-compile

  (defun get-api-funcs ()
    '(;(parse 1) (parse 2)
      (remove-credentials 1))))

(defmacro generate-api ()
  `(progn ,@(kla:make-funcs (get-api-funcs) 'amqp_uri)))

(generate-api)

(defun loaded-kanin-uri-macros ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).
  This function needs to be the last one in this include."
  'ok)
