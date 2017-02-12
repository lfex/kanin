;;;; This is the top-level include for all of kanin.
;;;;
;;;; Note that this include should be prefered over all others, in that in
;;;; entails them without overlap or conflict. If a developer picks and
;;;; choses which to incldue and/or changes the order in which they are done
;;;; so, there will quite likely arise errors indicating duplicate inclusion.

(include-lib "kanin/include/amqp-client.lfe")
(include-lib "kanin/include/kanin-conn-macros.lfe")
(include-lib "kanin/include/kanin-chan-macros.lfe")
(include-lib "kanin/include/kanin-uri-macros.lfe")
