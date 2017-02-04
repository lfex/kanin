(defmodule unit-kanin-chan-tests
  (behaviour ltest-unit))

(include-lib "ltest/include/ltest-macros.lfe")

(defun get-test-file ()
  "./_build/test/lib/kanin/ebin/kanin-chan.beam")

(deftest function-checks
  (is (is_function #'kanin-chan:call/2))
  (is (is_function #'kanin-chan:cast/3))
  (is (is_function #'kanin-chan:wait-for-confirms-or-die/3))
  (is (is_function #'kanin-chan:open/1))
  (is (is_function #'kanin-chan:code-change/3))
  (is (is_function #'kanin-chan:handle-info/2)))

(deftest export-count
  (let* ((chunks (beam_lib:chunks (get-test-file) '(exports)))
         (exports (proplists:get_value
                    'exports
                       (element 2 (element 2 chunks)))))
    (is-equal 36 (length exports))))
