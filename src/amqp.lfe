(defmodule amqp
  (export-macro connection.start
                connection.secure
                connection.tune
                connection.open
                connection.close
                connection.blocked
                connection.unblocked
                connection.redirect
                channel.open
                channel.flow
                channel.close
                channel.alert
                access.request
                exchange.declare
                exchange.delete
                exchange.bind
                exchange.unbind
                queue.declare
                queue.bind
                queue.purge
                queue.delete
                queue.unbind
                basic.qos
                basic.consume
                basic.cancel
                basic.publish
                basic.return
                basic.deliver
                basic.get
                basic.get_empty
                basic.ack
                basic.reject
                basic.recover_async
                basic.recover
                basic.nack
                basic.credit
                basic.credit_drained
                tx.select
                tx.commit
                tx.rollback
                confirm.select
                file.qos
                file.consume
                file.cancel
                file.open
                file.stage
                file.publish
                file.return
                file.deliver
                file.ack
                file.reject
                stream.qos
                stream.consume
                stream.cancel
                stream.publish
                stream.return
                stream.deliver
                dtx.select
                dtx.start
                tunnel.request
                test.integer
                test.string
                test.table
                test.content))

(include-lib "kanin/include/amqp-commands.lfe")

; (defmacro exchange.declare body
;   `(kanin:call
;      (make-exchange.declare ,@body)))

; (defmacro queue.declare body
;   `(kanin:call
;      (make-queue.declare ,@body)))

; (defmacro queue.bind body
;   `(kanin:call
;      (make-queue.bind ,@body)))

(defun noop () 'noop)
