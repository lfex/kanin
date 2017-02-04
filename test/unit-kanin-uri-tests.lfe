(defmodule unit-kanin-uri-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")
(include-lib "kanin/include/kanin-uri-macros.lfe")
(include-lib "kanin/include/amqp-client.lfe")

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
    (is-equal 42 (length exports))))

(deftest parse-net
  (let ((`#(ok ,result) (kanin-uri:parse "amqp://alice:secret@host:10000/vhost")))
    (is-equal 'amqp_params_network (element 1 result))
    (is-equal #"alice" (amqp_params_network-username result))
    (is-equal #"secret" (amqp_params_network-password result))
    (is-equal #"vhost" (amqp_params_network-virtual_host result))
    (is-equal "host" (amqp_params_network-host result))
    (is-equal 10000 (amqp_params_network-port result))))

(deftest parse-direct
  (let ((`#(ok ,result) (kanin-uri:parse "amqp://dave:secret@")))
    (is (is_record result 'amqp_params_direct))
    (is-equal #"dave" (amqp_params_direct-username result))
    (is-equal #"secret" (amqp_params_direct-password result))
    (is-equal #"/" (amqp_params_direct-virtual_host result))
    (is-equal 'nonode@nohost (amqp_params_direct-node result))))
