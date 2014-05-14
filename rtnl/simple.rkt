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
                     socket))


(define socket (nl-socket))


(define (interface-vlan? name)
  (with-handlers ((exn:fail:nl? not))
    (rtnl-link-is-vlan? (rtnl-link-get/kernel socket -1 name))))


(define (remove-interface name)
  (let ((link (rtnl-link-alloc)))
    (rtnl-link-set-name! link name)
    (rtnl-link-delete! socket link)))

(define remove-bridge remove-interface)
(define remove-vlan remove-interface)


(define (add-bridge name)
  (let ((link (rtnl-link-bridge-alloc)))
    (rtnl-link-set-name! link name)
    (rtnl-link-set-family! link 'bridge)
    (rtnl-link-add! socket link)))


(define (bridge-port-add bridge-name port-name)
  (let ((bridge (rtnl-link-get/kernel socket -1 bridge-name))
        (port   (rtnl-link-get/kernel socket -1 port-name))
        (change (rtnl-link-alloc)))
    (rtnl-link-set-master! change (rtnl-link-get-ifindex bridge))
    (rtnl-link-change! socket port change)))


(define (bridge-port-remove port-name)
  (let ((port   (rtnl-link-get/kernel socket -1 port-name))
        (change (rtnl-link-alloc)))
    (rtnl-link-set-master! change 0)
    (rtnl-link-change! socket port change)))


(define (add-vlan name parent id)
  (let ((parent (rtnl-link-get/kernel socket -1 parent))
        (link (rtnl-link-vlan-alloc)))
    (rtnl-link-set-name! link name)
    (rtnl-link-set-link! link (rtnl-link-get-ifindex parent))
    (rtnl-link-vlan-set-id! link id)
    (rtnl-link-add! socket link)))


(define (get-vlan-id name)
  (let ((link (rtnl-link-get/kernel socket -1 name)))
    (if (rtnl-link-is-vlan? link)
      (rtnl-link-vlan-get-id link)
      #f)))


(define (set-interface-up! name up)
  (let* ((link (rtnl-link-get/kernel socket -1 name))
         (change (rtnl-link-alloc)))
    (rtnl-link-set-flags! change (rtnl-link-get-flags link))
    ((if up rtnl-link-set-flags! rtnl-link-unset-flags!) change '(up))
    (rtnl-link-change! socket link change)))


(define (interface-up? name)
  (let ((link (rtnl-link-get/kernel socket -1 name)))
    (and (memq 'up (rtnl-link-get-flags link)) #t)))


(define (set-interface-addr! name addr)
  (let* ((link (rtnl-link-get/kernel socket -1 name))
         (change (rtnl-link-alloc)))
    (rtnl-link-set-addr! change (nl-addr-parse addr 'llc))
    (rtnl-link-change! socket link change)))


(define (get-interface-ipaddrs name)
  (let ((ifindex (rtnl-link-get-ifindex
                   (rtnl-link-get/kernel socket -1 name))))
    (map (compose nl-addr->string
                  rtnl-addr-get-local)
         (filter (lambda (addr)
                   (= ifindex (rtnl-addr-get-ifindex addr)))
                 (nl-cache->list (rtnl-addr-alloc-cache socket))))))


(define (add-interface-ipaddr name ip)
  (let ((link (rtnl-link-get/kernel socket -1 name))
        (addr (rtnl-addr-alloc)))
    (rtnl-addr-set-ifindex! addr (rtnl-link-get-ifindex link))
    (rtnl-addr-set-local! addr (nl-addr-parse ip 'unspec))
    (rtnl-addr-add! socket addr)))


(define (remove-interface-ipaddr name ip)
  (let* ((link (rtnl-link-get/kernel socket -1 name))
         (addr (rtnl-addr-get (rtnl-addr-alloc-cache socket)
                              (rtnl-link-get-ifindex link)
                              (nl-addr-parse ip 'unspec))))
    (rtnl-addr-delete! socket addr)))


(define (remove-interface-ipaddr/safe name ip)
  (let ((ips (remove ip (get-interface-ipaddrs name))))
    (remove-interface-ipaddr name ip)
    (for ((ip (in-list ips)))
      (unless (member ip (get-interface-ipaddrs name))
        (add-interface-ipaddr name ip)))))


; vim:set ts=2 sw=2 et:
