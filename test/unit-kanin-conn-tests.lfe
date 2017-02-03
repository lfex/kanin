(defmodule unit-kanin-conn-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(defun get-test-file ()
  "./_build/test/lib/kanin/ebin/kanin-conn.beam")

(deftest function-checks
  (is (is_function #'kanin-conn:open-channel/1))
  (is (is_function #'kanin-conn:open-channel/3))
  (is (is_function #'kanin-conn:start/1))
  (is (is_function #'kanin-conn:error-atom/1))
  (is (is_function #'kanin-conn:info-keys/1))
  (is (is_function #'kanin-conn:socket-adapter-info/2)))

(deftest export-count
  (let* ((chunks (beam_lib:chunks (get-test-file) '(exports)))
         (exports (proplists:get_value
                    'exports
                       (element 2 (element 2 chunks)))))
    (is-equal 18 (length exports))))
