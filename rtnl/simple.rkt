#lang racket/base
;
; Simplified Bindings
;
; These bindings are to be used together with sysfs/net.
; That way you get quite good framework for managing Linux
; network configuration.
;

(require racket/contract)

(require "main.rkt")

(provide (except-out (all-defined-out)
                     s))


(define s (nl-socket-alloc))
(nl-connect s 0)


(define (interface-vlan? name)
  (


; vim:set ts=2 sw=2 et:
