(defmodule unit-kanin-uri-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(defun get-test-file ()
  "./_build/test/lib/kanin/ebin/kanin-uri.beam")

(deftest function-checks
  (is (is_function #'kanin-uri:parse/1))
  (is (is_function #'kanin-uri:parse/2))
  (is (is_function #'kanin-uri:remove-credentials/1)))

; XXX the following aren't passing right now; see ticket #3:
;       https://github.com/billosys/kanin/issues/3
; (deftest function-negative-checks
;   (is-not (is_function #'kanin-uri:parse/3))
;   (is-not (is_function #'kanin-uri:parse1/2))
;   (is-not (is_function #'kanin-uri:remove-credentials/2))
;   (is-not (is_function #'kanin-uri:unescape-string/1)))

(deftest export-count
  (let* ((chunks (beam_lib:chunks (get-test-file) '(exports)))
         (exports (proplists:get_value
                    'exports
                       (element 2 (element 2 chunks)))))
    (is-equal 8 (length exports))))

(deftest parse-net
  (let ((`#(ok ,result) (kanin-uri:parse "amqp://alice:secret@host:10000/vhost")))
    (is-equal 'amqp_params_network (element 1 result))
    (is-equal "alice" (binary_to_list (element 2 result)))
    (is-equal "secret" (binary_to_list (element 3 result)))
    (is-equal "vhost" (binary_to_list (element 4 result)))
    (is-equal "host" (element 5 result))
    (is-equal 10000 (element 6 result))))

;; XXX The following test isn't passing anymore. It now failes due to a bad
;; arg. The manner in which amqp_params_direct is used (has been updated?)
;; needs to be looked into.
(deftestskip parse-direct
  (let ((`#(ok ,result) (kanin-uri:parse "amqp://dave:secret@")))
    (is-equal 'amqp_params_direct (element 1 result))
    (is-equal "dave" (binary_to_list (element 2 result)))
    (is-equal "secret" (binary_to_list (element 3 result)))
    (is-equal "/" (binary_to_list (element 4 result)))
    (is-equal 'nonode@nohost (element 5 result))))
