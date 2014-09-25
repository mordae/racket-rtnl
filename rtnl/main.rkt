#lang racket/base
;
; Racket FFI Bindings for Route-Netlink Library
;

(require
  (for-syntax racket/base))

(require
  (rename-in ffi/unsafe (-> -->)))

(require ffi/unsafe/define
         ffi/unsafe/atomic
         ffi/unsafe/alloc
         racket/contract
         racket/string
         racket/match)

(require misc1/syntax
         misc1/throw)

(provide
  (contract-out
    (exn:fail:nl? predicate/c)
    (nl-object-pointer? predicate/c)
    (nl-cache-pointer? predicate/c)
    (nl-socket-pointer? predicate/c)
    (nl-addr-pointer? predicate/c)
    (rtnl-link-pointer? predicate/c)
    (rtnl-route-pointer? predicate/c)
    (rtnl-nexthop-pointer? predicate/c)
    (rtnl-addr-pointer? predicate/c)
    (nl-addr->string (-> nl-addr-pointer? string?))
    (nl-socket (-> nl-socket-pointer?))
    (nl-cache->list (-> nl-cache-pointer? (listof nl-object-pointer?)))
    (rtnl-route-nexthops (-> rtnl-route-pointer? (listof rtnl-nexthop-pointer?)))))


(define-struct (exn:fail:nl exn:fail) ())


(define-ffi-definer define-libnl/raw
                    (ffi-lib "libnl-3" '("200" "")))

(define-ffi-definer define-libnl-route/raw
                    (ffi-lib "libnl-route-3" '("200" "")))


(define-for-syntax (name->symbol name)
  (string->symbol
    (regexp-replace* #rx"[?!]"
      (regexp-replace* #rx"[-/]"
        (symbol->string (syntax-e name)) "_") "")))

(define-syntax (define-nl stx)
  (syntax-case stx ()
    ((_ name _type opt ...)
     (with-syntax ((symbol (name->symbol #'name)))
       #'(begin
           (provide name)
           (define-libnl/raw name _type #:c-id symbol opt ...))))))

(define-syntax (define-rtnl stx)
  (syntax-case stx ()
    ((_ name _type opt ...)
     (with-syntax ((symbol (name->symbol #'name)))
       #'(begin
           (provide name)
           (define-libnl-route/raw name _type #:c-id symbol opt ...))))))

(define-syntax define-getter
  (syntax-rules ()
    ((_ name _object-type _value-type)
     (define-rtnl name (_fun _object-type --> _value-type)))

    ((_ name _object-type _value-type wrapper)
     (define-rtnl name (_fun _object-type --> _value-type) #:wrap wrapper))))

(define-syntax-rule (define-setter name _object-type _value-type)
  (define-rtnl name (_fun _object-type _value-type --> _void)))

(define-syntax define-accessors
  (syntax-rules ()
    ((_ _object-type (getter-name _getter-value-type)
                     (setter-name _setter-value-type))
     (begin
       (define-getter getter-name _object-type _getter-value-type)
       (define-setter setter-name _object-type _setter-value-type)))

    ((_ _object-type (getter-name _getter-value-type wrapper)
                     (setter-name _setter-value-type))
     (begin
       (define-getter getter-name _object-type _getter-value-type wrapper)
       (define-setter setter-name _object-type _setter-value-type)))))

(define-syntax-rule (define-simple-accessors _object-type
                      (getter-name setter-name _value-type) ...)
  (begin
    (define-accessors _object-type
                      (getter-name _value-type)
                      (setter-name _value-type))
    ...))


(define (check-result result)
  (unless (= 0 result)
    (throw exn:fail:nl 'rtnl (nl-geterror (abs result)))))

(define (check-int-result result)
  (when (negative? result)
    (throw exn:fail:nl 'rtnl (nl-geterror (abs result))))
  result)

(define (check-buffer->string/utf-8 buffer)
  (if buffer
      (cast buffer _bytes _string/utf-8)
      (throw exn:fail:nl 'rtnl "invalid value")))

(define ((lender incref) proc)
  (λ args
    (call-as-atomic
      (λ_
        (producing ((result (apply proc args)))
          (when result
            (incref result)))))))


(define-cpointer-type _nl-object-pointer)
(define-cpointer-type _nl-cache-pointer)
(define-cpointer-type _nl-socket-pointer)
(define-cpointer-type _nl-addr-pointer)
(define-cpointer-type _rtnl-link-pointer _nl-object-pointer)
(define-cpointer-type _rtnl-route-pointer _nl-object-pointer)
(define-cpointer-type _rtnl-nexthop-pointer _nl-object-pointer)
(define-cpointer-type _rtnl-addr-pointer _nl-object-pointer)
;(define-cpointer-type _rtnl-neigh-pointer _nl-object-pointer)
;(define-cpointer-type _rtnl-neightbl-pointer _nl-object-pointer)
;(define-cpointer-type _rtnl-qdisc-pointer _nl-object-pointer)
;(define-cpointer-type _rtnl-rule-pointer _nl-object-pointer)
;(define-cpointer-type _rtnl-tc-pointer _nl-object-pointer)


;
; General
;

(define _nl-addr-family (_enum '(unspec        = 0
                                 local         = 1
                                 inet          = 2
                                 ax25          = 3
                                 ipx           = 4
                                 appletalk     = 5
                                 netrom        = 6
                                 bridge        = 7
                                 atmpvc        = 8
                                 x25           = 9
                                 inet6         = 10
                                 rose          = 11
                                 decnet        = 12
                                 netbeui       = 13
                                 security      = 14
                                 key           = 15
                                 netlink       = 16
                                 packet        = 17
                                 ash           = 18
                                 econet        = 19
                                 atmsvc        = 20
                                 rds           = 21
                                 sna           = 22
                                 irda          = 23
                                 pppox         = 24
                                 wanpipe       = 25
                                 llc           = 26
                                 can           = 29
                                 tipc          = 30
                                 bluetooth     = 31
                                 iucv          = 32
                                 rxrpc         = 33
                                 isdn          = 34
                                 phonet        = 35
                                 ieee802154    = 36
                                 caif          = 37
                                 alg           = 38
                                 nfc           = 39)))


;
; Error Handling
;

(define-nl nl-geterror
           (_fun _int --> _string/utf-8))


;
; Network Addresses
;

(define-nl nl-addr-put!
           (_fun _nl-addr-pointer --> _void)
           #:wrap (releaser))

(define-nl nl-addr-get!
           (_fun _nl-addr-pointer --> _nl-addr-pointer)
           #:wrap (retainer nl-addr-put!))

(define-nl nl-addr-parse
           (_fun _string/utf-8
                 _nl-addr-family
                 (addr : (_ptr o _nl-addr-pointer/null))
                 --> (result : _int)
                 --> (begin
                       (check-result result) addr))
           #:wrap (allocator nl-addr-put!))

(define-nl nl-addr-cmp
           (_fun _nl-addr-pointer
                 _nl-addr-pointer
                 --> (result : _int)
                 --> (check-int-result result)))

(define-nl nl-addr-iszero?
           (_fun _nl-addr-pointer --> _bool))

(define-nl nl-addr-valid?
           (_fun _string/utf-8 _nl-addr-family --> _bool))

(define-nl nl-addr-set-family!
           (_fun _nl-addr-pointer
                 _nl-addr-family
                 --> _void))

(define-nl nl-addr-get-family
           (_fun _nl-addr-pointer --> _nl-addr-family))

(define-nl nl-addr-get-len
           (_fun _nl-addr-pointer --> _uint))

(define-nl nl-addr-set-prefixlen!
           (_fun _nl-addr-pointer _int --> _void))

(define-nl nl-addr-get-prefixlen
           (_fun _nl-addr-pointer --> _uint))

(define-nl nl-addr2str
           (_fun (addr : _nl-addr-pointer)
                 (buffer : _bytes
                         = (make-bytes (+ 6 (* 4 (nl-addr-get-len addr)))))
                 (size : _size = (bytes-length buffer))
                 --> (result : _bytes)
                 --> (check-buffer->string/utf-8 buffer)))

(define (nl-addr->string addr)
  (nl-addr2str addr))


;
; Main Netlink Socket
;

(define-nl nl-socket-free!
           (_fun _nl-socket-pointer --> _void)
           #:wrap (releaser))

(define-nl nl-socket-alloc
           (_fun --> _nl-socket-pointer)
           #:wrap (allocator nl-socket-free!))

(define-nl nl-connect!
           (_fun _nl-socket-pointer
                 (_int = 0)
                 --> (result : _int)
                 --> (check-result result)))

(define-nl nl-close!
           (_fun _nl-socket-pointer --> _void))

(define-nl nl-socket-disable-seq-check!
           (_fun _nl-socket-pointer --> _void))

(define (nl-socket)
  (let ((s (nl-socket-alloc)))
    (nl-socket-disable-seq-check! s)
    (nl-connect! s)
    s))


;
; Generic Objects
;

(define-nl nl-object-put!
           (_fun _nl-object-pointer --> _void)
           #:wrap (releaser))

(define-nl nl-object-get!
           (_fun _nl-object-pointer --> _void)
           #:wrap (retainer nl-object-put!))

(define-nl nl-object-clone
           (_fun _nl-object-pointer
                 --> (result : _nl-object-pointer/null)
                 --> (nl-object-upcast result)))

(define-nl nl-object-identical?
           (_fun _nl-object-pointer
                 _nl-object-pointer
                 --> _bool))

(define-nl nl-object-get-type
           (_fun _nl-object-pointer
                 --> (result : _string/utf-8)
                 --> (string->symbol result)))

(define (nl-object-upcast object)
  (and object
       (producing ((new-object
                     (match (nl-object-get-type object)
                       ('route/link
                        (cast object _nl-object-pointer _rtnl-link-pointer))
                       ('route/route
                        (cast object _nl-object-pointer _rtnl-route-pointer))
                       ('route/nexthop
                        (cast object _nl-object-pointer _rtnl-nexthop-pointer))
                       ('route/addr
                        (cast object _nl-object-pointer _rtnl-addr-pointer))
                       (else object))))
         (nl-object-get! new-object))))


;
; Generic Cache
;

(define-nl nl-cache-put!
           (_fun _nl-cache-pointer --> _void)
           #:wrap (releaser))

(define-nl nl-cache-nitems
           (_fun _nl-cache-pointer
                 --> (result : _int)
                 --> (check-int-result result)))

(define-nl nl-cache-nitems/filter
           (_fun _nl-cache-pointer
                 _nl-object-pointer
                 --> (result : _int)
                 --> (check-int-result result)))

(define-nl nl-cache-get-first
           (_fun _nl-cache-pointer
                 --> (result : _nl-object-pointer/null)
                 --> (nl-object-upcast result)))

(define-nl nl-cache-get-last
           (_fun _nl-cache-pointer
                 --> (result : _nl-object-pointer/null)
                 --> (nl-object-upcast result)))

(define-nl nl-cache-get-next
           (_fun _nl-object-pointer
                 --> (result : _nl-object-pointer/null)
                 --> (nl-object-upcast result)))

(define-nl nl-cache-get-prev
           (_fun _nl-object-pointer
                 --> (result : _nl-object-pointer/null)
                 --> (nl-object-upcast result)))

(define (nl-cache->list cache)
  (let loop ((item (nl-cache-get-first cache)))
    (if item
      (cons item (loop (nl-cache-get-next item)))
      (list))))

(define-nl nl-cache-find
           (_fun _nl-cache-pointer
                 _nl-object-pointer
                 --> (result : _nl-object-pointer/null)
                 --> (nl-object-upcast result)))

(define-nl nl-object-get-cache
           (_fun _nl-object-pointer
                 --> _nl-cache-pointer/null)
           #:wrap (allocator nl-cache-put!))


;
; Links
;

(define-rtnl rtnl-link-alloc
             (_fun --> _rtnl-link-pointer)
             #:wrap (allocator nl-object-put!))

(define-rtnl rtnl-link-add!
             (_fun _nl-socket-pointer
                   _rtnl-link-pointer
                   (_int = #x600)
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-change!
             (_fun _nl-socket-pointer
                   _rtnl-link-pointer
                   _rtnl-link-pointer
                   (_int = #x500)
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-delete!
             (_fun _nl-socket-pointer
                   _rtnl-link-pointer
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-get/kernel
             (_fun _nl-socket-pointer
                   _int
                   _string/utf-8
                   (link : (_ptr o _rtnl-link-pointer/null))
                   --> (result : _int)
                   --> (begin (check-result result) link))
             #:wrap (allocator nl-object-put!))

(define-simple-accessors _rtnl-link-pointer
  (rtnl-link-get-qdisc rtnl-link-set-qdisc! _string/utf-8)
  (rtnl-link-get-name rtnl-link-set-name! _string/utf-8)
  (rtnl-link-get-group rtnl-link-set-group! _uint32)
  (rtnl-link-get-mtu rtnl-link-set-mtu! _uint)
  (rtnl-link-get-txqlen rtnl-link-set-txqlen! _uint)
  (rtnl-link-get-ifindex rtnl-link-set-ifindex! _int)
  (rtnl-link-get-family rtnl-link-set-family! _nl-addr-family)
  (rtnl-link-get-link rtnl-link-set-link! _int)
  (rtnl-link-get-master rtnl-link-set-master! _int)
  (rtnl-link-get-promiscuity rtnl-link-set-promiscuity! _uint32)
  (rtnl-link-get-num-tx-queues rtnl-link-set-num-tx-queues! _uint32)
  (rtnl-link-get-num-rx-queues rtnl-link-set-num-rx-queues! _uint32)
  (rtnl-link-get-ifalias rtnl-link-set-ifalias! _string/utf-8))

(define-accessors _rtnl-link-pointer
                  (rtnl-link-get-addr _nl-addr-pointer/null
                                      (lender nl-addr-get!))
                  (rtnl-link-set-addr! _nl-addr-pointer/null))

(define-accessors _rtnl-link-pointer
                  (rtnl-link-get-broadcast _nl-addr-pointer/null
                                           (lender nl-addr-get!))
                  (rtnl-link-set-broadcast! _nl-addr-pointer/null))

(define-rtnl rtnl-link-enslave/ifindex!
             (_fun _nl-socket-pointer
                   _int
                   _int
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-enslave!
             (_fun _nl-socket-pointer
                   _rtnl-link-pointer
                   _rtnl-link-pointer
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-release/ifindex!
             (_fun _nl-socket-pointer
                   _int
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-release!
             (_fun _nl-socket-pointer
                   _rtnl-link-pointer
                   --> (result : _int)
                   --> (check-result result)))


;
; Bonding Links
;

(define-rtnl rtnl-link-bond-alloc
             (_fun --> _rtnl-link-pointer)
             #:wrap (allocator nl-object-put!))

(define-rtnl rtnl-link-bond-add!
             (_fun _nl-socket-pointer
                   _string/utf-8
                   _rtnl-link-pointer
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-bond-enslave/ifindex!
             (_fun _nl-socket-pointer
                   _int
                   _int
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-bond-enslave!
             (_fun _nl-socket-pointer
                   _rtnl-link-pointer
                   _rtnl-link-pointer
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-bond-release/ifindex!
             (_fun _nl-socket-pointer
                   _int
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-bond-release!
             (_fun _nl-socket-pointer
                   _rtnl-link-pointer
                   --> (result : _int)
                   --> (check-result result)))


;
; Bridge Links
;

(define-rtnl rtnl-link-bridge-alloc
             (_fun --> _rtnl-link-pointer)
             #:wrap (allocator nl-object-put!))

(define-rtnl rtnl-link-bridge-add!
             (_fun _nl-socket-pointer
                   _string/utf-8
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-is-bridge?
             (_fun _rtnl-link-pointer --> _bool))

(define-rtnl rtnl-link-bridge-set-port-state!
             (_fun _rtnl-link-pointer
                   _uint8
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-bridge-get-port-state
             (_fun _rtnl-link-pointer
                   --> (result : _int)
                   --> (check-int-result result)))

(define-rtnl rtnl-link-bridge-set-priority!
             (_fun _rtnl-link-pointer
                   _uint16
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-bridge-get-priority
             (_fun _rtnl-link-pointer
                   --> (result : _int)
                   --> (check-int-result result)))

(define-rtnl rtnl-link-bridge-set-cost!
             (_fun _rtnl-link-pointer
                   _uint32
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-bridge-get-cost
             (_fun _rtnl-link-pointer
                   (cost : (_ptr o _uint32))
                   --> (result : _int)
                   --> (begin
                         (check-result result)
                         cost)))

(define _rtnl-link-bridge-flags (_bitmask '(hairpin-mode = 1
                                            bpdu-guard   = 2
                                            root-block   = 4
                                            fast-leave   = 8)))

(define-rtnl rtnl-link-bridge-unset-flags!
             (_fun _rtnl-link-pointer
                   _rtnl-link-bridge-flags
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-bridge-set-flags!
             (_fun _rtnl-link-pointer
                   _rtnl-link-bridge-flags
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-bridge-get-flags
             (_fun _rtnl-link-pointer
                   --> _rtnl-link-bridge-flags))


;
; VLAN Links
;

(define-rtnl rtnl-link-vlan-alloc
             (_fun --> _rtnl-link-pointer)
             #:wrap (allocator nl-object-put!))

(define-rtnl rtnl-link-is-vlan?
             (_fun _rtnl-link-pointer --> _bool))

(define-rtnl rtnl-link-vlan-set-id!
             (_fun _rtnl-link-pointer
                   _uint16
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-vlan-get-id
             (_fun _rtnl-link-pointer
                   --> (result : _int)
                   --> (check-int-result result)))

(define-rtnl rtnl-link-vlan-flags2str
             (_fun _int
                   (buffer : _bytes = (make-bytes 512))
                   (size : _size = (bytes-length buffer))
                   --> (result : _bytes)
                   --> (check-buffer->string/utf-8 buffer)))

(define-rtnl rtnl-link-vlan-str2flags
             (_fun _string/utf-8
                   --> (result : _int)
                   --> (check-int-result result)))

(define-rtnl rtnl-link-vlan-get-flags
             (_fun _rtnl-link-pointer
                   --> (result : _uint)
                   --> (map string->symbol
                            (string-split (rtnl-link-vlan-flags2str result) ","))))

(define-rtnl rtnl-link-vlan-set-flags!
             (_fun _rtnl-link-pointer
                   (type: _uint
                    pre: (scheme => (rtnl-link-vlan-str2flags
                                      (string-join
                                        (map symbol->string scheme) ","))))
                   --> _void))

(define-rtnl rtnl-link-vlan-unset-flags!
             (_fun _rtnl-link-pointer
                   (type: _uint
                    pre: (scheme => (rtnl-link-vlan-str2flags
                                      (string-join
                                        (map symbol->string scheme) ","))))
                   --> _void))


;
; Link Cache
;

(define-rtnl rtnl-link-alloc-cache
             (_fun _nl-socket-pointer
                   (_int = 0)
                   (cache : (_ptr o _nl-cache-pointer))
                   --> (result : _int)
                   --> (begin (check-result result) cache))
             #:wrap (allocator nl-cache-put!))

(define-rtnl rtnl-link-get
             (_fun _nl-cache-pointer
                   _int
                   --> _rtnl-link-pointer/null)
             #:wrap (allocator nl-object-put!))

(define-rtnl rtnl-link-get-by-name
             (_fun _nl-cache-pointer
                   _string/utf-8
                   --> _rtnl-link-pointer/null)
             #:wrap (allocator nl-object-put!))

(define-rtnl rtnl-link-i2name
             (_fun _nl-cache-pointer
                   _int
                   (buffer : _bytes = (make-bytes 16))
                   (size : _size = (bytes-length buffer))
                   --> (result : _bytes)
                   --> (check-buffer->string/utf-8 buffer)))

(define-rtnl rtnl-link-name2i
             (_fun _nl-cache-pointer
                   _string/utf-8
                   --> (result : _int)
                   --> (check-int-result result)))

(define-rtnl rtnl-link-flags2str
             (_fun _int
                   (buffer : _bytes = (make-bytes 1024))
                   (size : _size = (bytes-length buffer))
                   --> (result : _bytes)
                   --> (check-buffer->string/utf-8 buffer)))

(define-rtnl rtnl-link-str2flags
             (_fun _string/utf-8
                   --> (result : _int)
                   --> (check-int-result result)))

(define-rtnl rtnl-link-operstate2str
             (_fun _uint8
                   (buffer : _bytes = (make-bytes 32))
                   (size : _size = (bytes-length buffer))
                   --> (result : _bytes)
                   --> (check-buffer->string/utf-8 buffer)))

(define-rtnl rtnl-link-str2operstate
             (_fun _string/utf-8
                   --> (result : _int)
                   --> (check-int-result result)))

(define-rtnl rtnl-link-mode2str
             (_fun _uint8
                   (buffer : _bytes = (make-bytes 32))
                   (size : _size = (bytes-length buffer))
                   --> (result : _bytes)
                   --> (check-buffer->string/utf-8 buffer)))

(define-rtnl rtnl-link-str2mode
             (_fun _string/utf-8
                   --> (result : _int)
                   --> (check-int-result result)))

(define-rtnl rtnl-link-carrier2str
             (_fun _uint8
                   (buffer : _bytes = (make-bytes 32))
                   (size : _size = (bytes-length buffer))
                   --> (result : _bytes)
                   --> (check-buffer->string/utf-8 buffer)))

(define-rtnl rtnl-link-str2carrier
             (_fun _string/utf-8
                   --> (result : _int)
                   --> (check-int-result result)))


(define-rtnl rtnl-link-set-type!
             (_fun _rtnl-link-pointer
                   _symbol
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-get-type
             (_fun _rtnl-link-pointer --> _symbol))

(define-rtnl rtnl-link-get-flags
             (_fun _rtnl-link-pointer
                   --> (result : _uint)
                   --> (map string->symbol
                            (string-split (rtnl-link-flags2str result) ","))))

(define-rtnl rtnl-link-set-flags!
             (_fun _rtnl-link-pointer
                   (type: _uint
                    pre: (scheme => (rtnl-link-str2flags
                                      (string-join
                                        (map symbol->string scheme) ","))))
                   --> _void))

(define-rtnl rtnl-link-unset-flags!
             (_fun _rtnl-link-pointer
                   (type: _uint
                    pre: (scheme => (rtnl-link-str2flags
                                      (string-join
                                        (map symbol->string scheme) ","))))
                   --> _void))

(define-rtnl rtnl-link-get-carrier
             (_fun _rtnl-link-pointer
                   --> (result : _uint)
                   --> (string->symbol (rtnl-link-carrier2str result))))

(define-rtnl rtnl-link-set-carrier!
             (_fun _rtnl-link-pointer
                   (type: _uint
                    pre: (scheme => (rtnl-link-str2carrier
                                      (symbol->string scheme))))
                   --> _void))

(define-rtnl rtnl-link-get-operstate
             (_fun _rtnl-link-pointer
                   --> (result : _uint)
                   --> (string->symbol (rtnl-link-operstate2str result))))

(define-rtnl rtnl-link-set-operstate!
             (_fun _rtnl-link-pointer
                   (type: _uint
                    pre: (scheme => (rtnl-link-str2operstate
                                      (symbol->string scheme))))
                   --> _void))

(define-rtnl rtnl-link-get-linkmode
             (_fun _rtnl-link-pointer
                   --> (result : _uint)
                   --> (string->symbol (rtnl-link-mode2str result))))

(define-rtnl rtnl-link-set-linkmode!
             (_fun _rtnl-link-pointer
                   (type: _uint
                    pre: (scheme => (rtnl-link-str2mode
                                      (symbol->string scheme))))
                   --> _void))


;
; Addresses
;

(define-rtnl rtnl-addr-alloc
             (_fun --> _rtnl-addr-pointer)
             #:wrap (allocator nl-object-put!))

(define-rtnl rtnl-addr-alloc-cache
             (_fun _nl-socket-pointer
                   (cache : (_ptr o _nl-cache-pointer))
                   --> (result : _int)
                   --> (begin (check-result result) cache))
             #:wrap (allocator nl-cache-put!))

(define-rtnl rtnl-addr-get
             (_fun _nl-cache-pointer
                   _int
                   _nl-addr-pointer
                   --> _rtnl-addr-pointer/null)
             #:wrap (allocator nl-object-put!))

(define-rtnl rtnl-addr-add!
             (_fun _nl-socket-pointer
                   _rtnl-addr-pointer
                   (_int = 0)
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-addr-delete!
             (_fun _nl-socket-pointer
                   _rtnl-addr-pointer
                   (_int = 0)
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-scope2str
             (_fun _uint8
                   (buffer : _bytes = (make-bytes 32))
                   (size : _size = (bytes-length buffer))
                   --> (result : _bytes)
                   --> (check-buffer->string/utf-8 buffer)))

(define-rtnl rtnl-str2scope
             (_fun _string/utf-8
                   --> (result : _int)
                   --> (check-int-result result)))

(define-simple-accessors _rtnl-addr-pointer
  (rtnl-addr-get-ifindex rtnl-addr-set-ifindex! _int)
  (rtnl-addr-get-family rtnl-addr-set-family! _nl-addr-family)
  (rtnl-addr-get-prefixlen rtnl-addr-set-prefixlen! _int)
  (rtnl-addr-get-scope rtnl-addr-set-scope! _int)
  (rtnl-addr-get-valid-lifetime rtnl-addr-set-valid-lifetime! _uint32)
  (rtnl-addr-get-preferred-lifetime rtnl-addr-set-preferred-lifetime! _uint32))

(define-accessors _rtnl-addr-pointer
                  (rtnl-addr-get-link _rtnl-link-pointer/null
                                      (allocator nl-object-put!))
                  (rtnl-addr-set-link! _rtnl-link-pointer/null))

(define-accessors _rtnl-addr-pointer
                  (rtnl-addr-get-local _nl-addr-pointer/null
                                       (lender nl-addr-get!))
                  (rtnl-addr-set-local! _nl-addr-pointer/null))

(define-accessors _rtnl-addr-pointer
                  (rtnl-addr-get-peer _nl-addr-pointer/null
                                      (lender nl-addr-get!))
                  (rtnl-addr-set-peer! _nl-addr-pointer/null))

(define-accessors _rtnl-addr-pointer
                  (rtnl-addr-get-broadcast _nl-addr-pointer/null
                                           (lender nl-addr-get!))
                  (rtnl-addr-set-broadcast! _nl-addr-pointer/null))

(define-accessors _rtnl-addr-pointer
                  (rtnl-addr-get-multicast _nl-addr-pointer/null
                                           (lender nl-addr-get!))
                  (rtnl-addr-set-multicast! _nl-addr-pointer/null))

(define-accessors _rtnl-addr-pointer
                  (rtnl-addr-get-anycast _nl-addr-pointer/null
                                         (lender nl-addr-get!))
                  (rtnl-addr-set-anycast! _nl-addr-pointer/null))

(define-rtnl rtnl-addr-get-create-time
             (_fun _rtnl-addr-pointer --> _uint32))

(define-rtnl rtnl-addr-get-last-update-time
             (_fun _rtnl-addr-pointer --> _uint32))

(define-rtnl rtnl-addr-flags2str
             (_fun _int
                   (buffer : _bytes = (make-bytes 512))
                   (size : _size = (bytes-length buffer))
                   --> (result : _bytes)
                   --> (check-buffer->string/utf-8 buffer)))

(define-rtnl rtnl-addr-str2flags
             (_fun _string/utf-8
                   --> (result : _int)
                   --> (check-int-result result)))

(define-rtnl rtnl-addr-get-flags
             (_fun _rtnl-addr-pointer
                   --> (result : _uint)
                   --> (map string->symbol
                            (string-split (rtnl-addr-flags2str result) ","))))

(define-rtnl rtnl-addr-set-flags!
             (_fun _rtnl-addr-pointer
                   (type: _uint
                    pre: (scheme => (rtnl-addr-str2flags
                                      (string-join
                                        (map symbol->string scheme) ","))))
                   --> _void))

(define-rtnl rtnl-addr-unset-flags!
             (_fun _rtnl-addr-pointer
                   (type: _uint
                    pre: (scheme => (rtnl-addr-str2flags
                                      (string-join
                                        (map symbol->string scheme) ","))))
                   --> _void))


;
; Routes
;

(define-rtnl rtnl-route-alloc
             (_fun --> _rtnl-route-pointer)
             #:wrap (allocator nl-object-put!))

(define-rtnl rtnl-route-alloc-cache
             (_fun _nl-socket-pointer
                   _nl-addr-family
                   (_enum '(route = 0
                            cache = 1))
                   (cache : (_ptr o _nl-cache-pointer))
                   --> (result : _int)
                   --> (begin (check-result result) cache))
             #:wrap (allocator nl-cache-put!))

(define-rtnl rtnl-route-add!
             (_fun _nl-socket-pointer
                   _rtnl-route-pointer
                   (_int = 0)
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-route-delete!
             (_fun _nl-socket-pointer
                   _rtnl-route-pointer
                   (_int = 0)
                   --> (result : _int)
                   --> (check-result result)))

(define-simple-accessors _rtnl-route-pointer
  (rtnl-route-get-table rtnl-route-set-table! _uint32)
  (rtnl-route-get-scope rtnl-route-set-scope! _uint8)
  (rtnl-route-get-tos rtnl-route-set-tos! _uint8)
  (rtnl-route-get-protocol rtnl-route-set-protocol! _nl-addr-family)
  (rtnl-route-get-priority rtnl-route-set-priority! _uint32)
  (rtnl-route-get-family rtnl-route-set-family! _nl-addr-family)
  (rtnl-route-get-type rtnl-route-set-type! _uint8)
  (rtnl-route-get-iif rtnl-route-set-iif! _int))

(define-accessors _rtnl-route-pointer
                  (rtnl-route-get-dst _nl-addr-pointer/null
                                      (allocator nl-object-put!))
                  (rtnl-route-set-dst! _nl-addr-pointer/null))

(define-accessors _rtnl-route-pointer
                  (rtnl-route-get-src _nl-addr-pointer/null
                                      (allocator nl-object-put!))
                  (rtnl-route-set-src! _nl-addr-pointer/null))

(define-accessors _rtnl-route-pointer
                  (rtnl-route-get-pref-src _nl-addr-pointer/null
                                           (allocator nl-object-put!))
                  (rtnl-route-set-pref-src! _nl-addr-pointer/null))

(define-rtnl rtnl-route-add-nexthop!
             (_fun _rtnl-route-pointer
                   _rtnl-nexthop-pointer
                   --> _void))

(define-rtnl rtnl-route-remove-nexthop!
             (_fun _rtnl-route-pointer
                   _rtnl-nexthop-pointer
                   --> _void))

(define-rtnl rtnl-route-get-nnexthops
             (_fun _rtnl-route-pointer
                   --> (result : _int)
                   --> (check-int-result result)))

(define-rtnl rtnl-route-nexthop-n
             (_fun _rtnl-route-pointer
                   _int
                   --> _rtnl-nexthop-pointer/null)
             #:wrap (allocator nl-object-put!))

(define (rtnl-route-nexthops route)
  (for/list ((i (in-range (rtnl-route-get-nnexthops route))))
    (rtnl-route-nexthop-n route i)))

(define-rtnl rtnl-route-nh-flags2str
             (_fun _int
                   (buffer : _bytes = (make-bytes 512))
                   (size : _size = (bytes-length buffer))
                   --> (result : _bytes)
                   --> (check-buffer->string/utf-8 buffer)))

(define-rtnl rtnl-route-nh-str2flags
             (_fun _string/utf-8
                   --> (result : _int)
                   --> (check-int-result result)))

(define-rtnl rtnl-route-nh-get-flags
             (_fun _rtnl-link-pointer
                   --> (result : _uint)
                   --> (map string->symbol
                            (string-split (rtnl-route-nh-flags2str result) ","))))

(define-rtnl rtnl-route-nh-set-flags!
             (_fun _rtnl-link-pointer
                   (type: _uint
                    pre: (scheme => (rtnl-route-nh-str2flags
                                      (string-join
                                        (map symbol->string scheme) ","))))
                   --> _void))

(define-rtnl rtnl-route-nh-unset-flags!
             (_fun _rtnl-link-pointer
                   (type: _uint
                    pre: (scheme => (rtnl-route-nh-str2flags
                                      (string-join
                                        (map symbol->string scheme) ","))))
                   --> _void))


;
; Next Hops
;

(define-rtnl rtnl-route-nh-alloc
             (_fun --> _rtnl-nexthop-pointer)
             #:wrap (allocator nl-object-put!))

(define-simple-accessors _rtnl-nexthop-pointer
  (rtnl-route-nh-get-weight rtnl-route-nh-set-weight! _uint8)
  (rtnl-route-nh-get-ifindex rtnl-route-nh-set-ifindex! _int)
  (rtnl-route-nh-get-realms rtnl-route-nh-set-realms! _uint32))

(define-accessors _rtnl-nexthop-pointer
                  (rtnl-route-nh-get-gateway _nl-addr-pointer/null
                                             (lender nl-addr-get!))
                  (rtnl-route-nh-set-gateway! _nl-addr-pointer/null))


; vim:set ts=2 sw=2 et:
