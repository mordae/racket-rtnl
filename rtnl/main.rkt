#lang racket/base
;
; Racket FFI Bindings for Route-Netlink Library
;

(require (rename-in ffi/unsafe (-> -->))
         (for-syntax racket/base)
         (for-syntax racket/syntax)
         racket/contract
         racket/match
         racket/string
         racket/provide
         throw)

(provide (filtered-out (lambda (name)
                         (and (regexp-match? #rx"^[^_]" name)
                              (not (regexp-match? #rx"-tag$" name))
                              (not (regexp-match? #rx"-get!$" name))
                              (not (regexp-match? #rx"-put!$" name))
                              (not (regexp-match? #rx"str2" name))
                              (not (regexp-match? #rx"2str" name))
                              (regexp-replace #rx"-pointer\\?$" name "?")))
           (except-out (all-defined-out)
                       nl-socket-free!
                       define-rtnl-link-accessors
                       define-rtnl-addr-accessors
                       define-rtnl-route-accessors
                       define-rtnl-nexthop-accessors
                       rtnl-route-get-nnexthops
                       rtnl-route-nexthop-n
                       libnl
                       libnl-route
                       define-nl
                       define-rtnl
                       with-finalizer
                       check-result
                       nl-socket-alloc
                       nl-socket-free!
                       nl-socket-disable-seq-check!
                       nl-connect!
                       nl-close!)))


(define libnl       (ffi-lib "libnl-3" '("200" "")))
(define libnl-route (ffi-lib "libnl-route-3" '("200" "")))


(define-struct (exn:fail:nl exn:fail) ())


(define-syntax (define-nl stx)
  (syntax-case stx ()
    ((_ name type)
     (with-syntax ((symbol (regexp-replace* #rx"[?!]"
                             (regexp-replace* #rx"[-/]"
                               (symbol->string (syntax-e #'name)) "_") "")))
       #'(define name (get-ffi-obj symbol libnl type))))))


(define-syntax (define-rtnl stx)
  (syntax-case stx ()
    ((_ name type)
     (with-syntax ((symbol (regexp-replace* #rx"[?!]"
                             (regexp-replace* #rx"[-/]"
                               (symbol->string (syntax-e #'name)) "_") "")))
       #'(define name (get-ffi-obj symbol libnl-route type))))))


(define (with-finalizer result finalizer)
  (when result
    (register-finalizer result finalizer))
  result)


(define (check-result result)
  (unless (= 0 result)
    (throw exn:fail:nl
           'rtnl (nl-geterror (abs result)))))


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
           (_fun _nl-addr-pointer --> _void))

(define-nl nl-addr-parse
           (_fun _string/utf-8
                 _nl-addr-family
                 (addr : (_ptr o _nl-addr-pointer/null))
                 --> (result : _int)
                 --> (begin
                       (check-result result)
                       (with-finalizer addr nl-addr-put!))))

(define-nl nl-addr-cmp
           (_fun _nl-addr-pointer
                 _nl-addr-pointer
                 --> _int))

(define-nl nl-addr-iszero?
           (_fun _nl-addr-pointer --> _bool))

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
                         = (make-bytes (+ 5 (* 4 (nl-addr-get-len addr)))))
                 (size : _size = (bytes-length buffer))
                 --> (result : _bytes)
                 --> (and result
                          (string-trim
                            (bytes->string/utf-8 buffer) "\0" #:repeat? #t))))

(define (nl-addr->string addr)
  (nl-addr2str addr))


;
; Main Netlink Socket
;

(define-nl nl-socket-alloc
           (_fun --> (result : _nl-socket-pointer)
                 --> (with-finalizer result nl-socket-free!)))

(define-nl nl-socket-free!
           (_fun _nl-socket-pointer --> _void))

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

(define-nl nl-object-get!
           (_fun _nl-object-pointer --> _void))

(define-nl nl-object-put!
           (_fun _nl-object-pointer --> _void))

(define-nl nl-object-clone
           (_fun _nl-object-pointer
                 --> (result : _nl-object-pointer/null)
                 --> (with-finalizer (nl-object-upcast result)
                                     nl-object-put!)))

(define-nl nl-object-identical?
           (_fun _nl-object-pointer
                 _nl-object-pointer
                 --> _bool))

(define-nl nl-object-get-type
           (_fun _nl-object-pointer
                 --> (result : _string/utf-8)
                 --> (string->symbol result)))

(define-nl nl-object-get-cache
           (_fun _nl-object-pointer
                 --> (result : _nl-cache-pointer/null)
                 --> (with-finalizer result nl-cache-put!)))

(define (nl-object-upcast object)
  (if object
    (let ((new-object
            (match (nl-object-get-type object)
                   ('route/link    (cast object _nl-object-pointer
                                                _rtnl-link-pointer))
                   ('route/route   (cast object _nl-object-pointer
                                                _rtnl-route-pointer))
                   ('route/nexthop (cast object _nl-object-pointer
                                                _rtnl-nexthop-pointer))
                   ('route/addr    (cast object _nl-object-pointer
                                                _rtnl-addr-pointer))
                   (_              object))))
      (nl-object-get! new-object)
      (with-finalizer new-object nl-object-put!))
    #f))


;
; Generic Cache
;

(define-nl nl-cache-put!
           (_fun _nl-cache-pointer --> _void))

(define-nl nl-cache-nitems
           (_fun _nl-cache-pointer --> _int))

(define-nl nl-cache-nitems/filter
           (_fun _nl-cache-pointer
                 _nl-object-pointer
                 --> _int))

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
                 --> (with-finalizer (nl-object-upcast result)
                                     nl-object-put!)))


;
; Links
;

(define-rtnl rtnl-link-alloc
             (_fun --> (result : _rtnl-link-pointer)
                   --> (with-finalizer result nl-object-put!)))

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
                   --> (begin
                         (check-result result)
                         (with-finalizer link nl-object-put!))))

(define-syntax-rule (define-rtnl-link-accessors (setter getter type) ...)
  (begin
    (begin
      (define-rtnl setter (_fun _rtnl-link-pointer type --> _void))
      (define-rtnl getter (_fun _rtnl-link-pointer --> type)))
    ...))

(define-rtnl-link-accessors
  (rtnl-link-set-qdisc!     rtnl-link-get-qdisc     _string/utf-8)
  (rtnl-link-set-name!      rtnl-link-get-name      _string/utf-8)
  (rtnl-link-set-group!     rtnl-link-get-group     _uint32)
  (rtnl-link-set-mtu!       rtnl-link-get-mtu       _uint)
  (rtnl-link-set-txqlen!    rtnl-link-get-txqlen    _uint)
  (rtnl-link-set-ifindex!   rtnl-link-get-ifindex   _int)
  (rtnl-link-set-family!    rtnl-link-get-family    _nl-addr-family)
  ;(rtnl-link-set-arptype!   rtnl-link-get-arptype   _uint)
  (rtnl-link-set-addr!      rtnl-link-get-addr      _nl-addr-pointer/null)
  (rtnl-link-set-broadcast! rtnl-link-get-broadcast _nl-addr-pointer/null)
  (rtnl-link-set-link!      rtnl-link-get-link      _int)
  (rtnl-link-set-master!    rtnl-link-get-master    _int)
  (rtnl-link-set-ifalias!   rtnl-link-get-ifalias   _string/utf-8)
  (rtnl-link-set-type!      rtnl-link-get-type      _symbol))

(define-rtnl-link-accessors
  (rtnl-link-set-promiscuity!   rtnl-link-get-promiscuity   _uint32)
  (rtnl-link-set-num-tx-queues! rtnl-link-get-num-tx-queues _uint32)
  (rtnl-link-set-num-rx-queues! rtnl-link-get-num-rx-queues _uint32))

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
             (_fun --> (result : _rtnl-link-pointer)
                   --> (with-finalizer result nl-object-put!)))

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
             (_fun --> (result : _rtnl-link-pointer)
                   --> (with-finalizer result nl-object-put!)))

(define-rtnl rtnl-link-is-bridge?
             (_fun _rtnl-link-pointer --> _bool))

(define-rtnl rtnl-link-bridge-set-port-state!
             (_fun _rtnl-link-pointer
                   _uint8
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-bridge-get-port-state
             (_fun _rtnl-link-pointer --> _int))

(define-rtnl rtnl-link-bridge-set-priority!
             (_fun _rtnl-link-pointer
                   _uint16
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-bridge-get-priority
             (_fun _rtnl-link-pointer --> _int))

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
             (_fun --> (result : _rtnl-link-pointer)
                   --> (with-finalizer result nl-object-put!)))

(define-rtnl rtnl-link-is-vlan?
             (_fun _rtnl-link-pointer --> _bool))

(define-rtnl rtnl-link-vlan-set-id!
             (_fun _rtnl-link-pointer
                   _uint16
                   --> (result : _int)
                   --> (check-result result)))

(define-rtnl rtnl-link-vlan-get-id
             (_fun _rtnl-link-pointer --> _int))

(define-rtnl rtnl-link-vlan-flags2str
             (_fun _int
                   (buf : _bytes = (make-bytes 512))
                   (size : _size = (bytes-length buf))
                   --> (result : _bytes)
                   --> (and result
                            (string-trim
                              (bytes->string/utf-8 buf) "\0" #:repeat? #t))))

(define-rtnl rtnl-link-vlan-str2flags
             (_fun _string/utf-8 --> _int))

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
                   --> (begin
                         (check-result result)
                         (with-finalizer cache nl-cache-put!))))

(define-rtnl rtnl-link-get
             (_fun _nl-cache-pointer
                   _int
                   --> (result : _rtnl-link-pointer/null)
                   --> (with-finalizer result nl-object-put!)))

(define-rtnl rtnl-link-get-by-name
             (_fun _nl-cache-pointer
                   _string/utf-8
                   --> (result : _rtnl-link-pointer/null)
                   --> (with-finalizer result nl-object-put!)))

(define-rtnl rtnl-link-i2name
             (_fun _nl-cache-pointer
                   _int
                   (buf : _bytes = (make-bytes 16))
                   (size : _size = (bytes-length buf))
                   --> (result : _bytes)
                   --> (and result
                            (string-trim
                              (bytes->string/utf-8 buf) "\0" #:repeat? #t))))

(define-rtnl rtnl-link-name2i
             (_fun _nl-cache-pointer
                   _string/utf-8
                   --> _int))

(define-rtnl rtnl-link-flags2str
             (_fun _int
                   (buf : _bytes = (make-bytes 1024))
                   (size : _size = (bytes-length buf))
                   --> (result : _bytes)
                   --> (and result
                            (string-trim
                              (bytes->string/utf-8 buf) "\0" #:repeat? #t))))

(define-rtnl rtnl-link-str2flags
             (_fun _string/utf-8 --> _int))

(define-rtnl rtnl-link-operstate2str
             (_fun _uint8
                   (buf : _bytes = (make-bytes 32))
                   (size : _size = (bytes-length buf))
                   --> (result : _bytes)
                   --> (and result
                            (string-trim
                              (bytes->string/utf-8 buf) "\0" #:repeat? #t))))

(define-rtnl rtnl-link-str2operstate
             (_fun _string/utf-8 --> _int))

(define-rtnl rtnl-link-mode2str
             (_fun _uint8
                   (buf : _bytes = (make-bytes 32))
                   (size : _size = (bytes-length buf))
                   --> (result : _bytes)
                   --> (and result
                            (string-trim
                              (bytes->string/utf-8 buf) "\0" #:repeat? #t))))

(define-rtnl rtnl-link-str2mode
             (_fun _string/utf-8 --> _int))

(define-rtnl rtnl-link-carrier2str
             (_fun _uint8
                   (buf : _bytes = (make-bytes 32))
                   (size : _size = (bytes-length buf))
                   --> (result : _bytes)
                   --> (and result
                            (string-trim
                              (bytes->string/utf-8 buf) "\0" #:repeat? #t))))

(define-rtnl rtnl-link-str2carrier
             (_fun _string/utf-8 --> _int))

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
             (_fun --> (result : _rtnl-addr-pointer)
                   --> (with-finalizer result nl-object-put!)))

(define-rtnl rtnl-addr-alloc-cache
             (_fun _nl-socket-pointer
                   (cache : (_ptr o _nl-cache-pointer))
                   --> (result : _int)
                   --> (begin
                         (check-result result)
                         (with-finalizer cache nl-cache-put!))))

(define-rtnl rtnl-addr-get
             (_fun _nl-cache-pointer
                   _int
                   _nl-addr-pointer
                   --> (result : _rtnl-addr-pointer/null)
                   --> (with-finalizer result nl-object-put!)))

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

(define-syntax-rule (define-rtnl-addr-accessors (setter getter type) ...)
  (begin
    (begin
      (define-rtnl setter (_fun _rtnl-addr-pointer type --> _void))
      (define-rtnl getter (_fun _rtnl-addr-pointer --> type)))
    ...))

(define-rtnl-addr-accessors
  (rtnl-addr-set-ifindex!   rtnl-addr-get-ifindex    _int)
  (rtnl-addr-set-link!      rtnl-addr-get-link       _rtnl-link-pointer/null)
  (rtnl-addr-set-family!    rtnl-addr-get-family     _nl-addr-family)
  (rtnl-addr-set-prefixlen! rtnl-addr-get-prefixlen  _int)
  (rtnl-addr-set-scope!     rtnl-addr-get-scope      _int)
  (rtnl-addr-set-local!     rtnl-addr-get-local      _nl-addr-pointer/null)
  (rtnl-addr-set-peer!      rtnl-addr-get-peer       _nl-addr-pointer/null)
  (rtnl-addr-set-broadcast! rtnl-addr-get-broadcast  _nl-addr-pointer/null)
  (rtnl-addr-set-multicast! rtnl-addr-get-multicast  _nl-addr-pointer/null)
  (rtnl-addr-set-anycast!   rtnl-addr-get-anycast    _nl-addr-pointer/null))

(define-rtnl-addr-accessors
  (rtnl-addr-set-valid-lifetime!     rtnl-addr-get-valid-lifetime     _uint32)
  (rtnl-addr-set-preferred-lifetime! rtnl-addr-get-preferred-lifetime _uint32))

(define-rtnl rtnl-addr-get-create-time
             (_fun _rtnl-addr-pointer --> _uint32))

(define-rtnl rtnl-addr-get-last-update-time
             (_fun _rtnl-addr-pointer --> _uint32))

(define-rtnl rtnl-addr-flags2str
             (_fun _int
                   (buf : _bytes = (make-bytes 512))
                   (size : _size = (bytes-length buf))
                   --> (result : _bytes)
                   --> (and result
                            (string-trim
                              (bytes->string/utf-8 buf) "\0" #:repeat? #t))))

(define-rtnl rtnl-addr-str2flags
             (_fun _string/utf-8 --> _int))

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
             (_fun --> (result : _rtnl-route-pointer)
                   --> (with-finalizer result nl-object-put!)))

(define-rtnl rtnl-route-alloc-cache
             (_fun _nl-socket-pointer
                   _nl-addr-family
                   (_enum '(route = 0
                            cache = 1))
                   (cache : (_ptr o _nl-cache-pointer))
                   --> (result : _int)
                   --> (begin
                         (check-result result)
                         (with-finalizer cache nl-cache-put!))))

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

(define-syntax-rule (define-rtnl-route-accessors (setter getter type) ...)
  (begin
    (begin
      (define-rtnl setter (_fun _rtnl-route-pointer type --> _void))
      (define-rtnl getter (_fun _rtnl-route-pointer --> type)))
    ...))

(define-rtnl-route-accessors
  (rtnl-route-set-table!    rtnl-route-get-table    _uint32)
  (rtnl-route-set-scope!    rtnl-route-get-scope    _uint8)
  (rtnl-route-set-tos!      rtnl-route-get-tos      _uint8)
  (rtnl-route-set-protocol! rtnl-route-get-protocol _nl-addr-family)
  (rtnl-route-set-priority! rtnl-route-get-priority _uint32)
  (rtnl-route-set-family!   rtnl-route-get-family   _nl-addr-family)
  (rtnl-route-set-type!     rtnl-route-get-type     _uint8)
  (rtnl-route-set-dst!      rtnl-route-get-dst      _nl-addr-pointer/null)
  (rtnl-route-set-src!      rtnl-route-get-src      _nl-addr-pointer/null)
  (rtnl-route-set-pref-src! rtnl-route-get-pref-src _nl-addr-pointer/null)
  (rtnl-route-set-iif!      rtnl-route-get-iif      _int))

(define-rtnl rtnl-route-add-nexthop!
             (_fun _rtnl-route-pointer
                   _rtnl-nexthop-pointer
                   --> _void))

(define-rtnl rtnl-route-remove-nexthop!
             (_fun _rtnl-route-pointer
                   _rtnl-nexthop-pointer
                   --> _void))

(define-rtnl rtnl-route-get-nnexthops
             (_fun _rtnl-route-pointer --> _int))

(define-rtnl rtnl-route-nexthop-n
             (_fun _rtnl-route-pointer
                   _int
                   --> (result : _rtnl-nexthop-pointer/null)
                   --> (with-finalizer result nl-object-put!)))

(define (rtnl-route-nexthops route)
  (for/list ((i (in-range (rtnl-route-get-nnexthops route))))
    (rtnl-route-nexthop-n route i)))

(define-rtnl rtnl-route-nh-flags2str
             (_fun _int
                   (buf : _bytes = (make-bytes 512))
                   (size : _size = (bytes-length buf))
                   --> (result : _bytes)
                   --> (and result
                            (string-trim
                              (bytes->string/utf-8 buf) "\0" #:repeat? #t))))

(define-rtnl rtnl-route-nh-str2flags
             (_fun _string/utf-8 --> _int))

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
             (_fun --> (result : _rtnl-nexthop-pointer)
                   --> (with-finalizer result nl-object-put!)))

(define-syntax-rule (define-rtnl-nexthop-accessors (setter getter type) ...)
  (begin
    (begin
      (define-rtnl setter (_fun _rtnl-nexthop-pointer type --> _void))
      (define-rtnl getter (_fun _rtnl-nexthop-pointer --> type)))
    ...))

(define-rtnl-nexthop-accessors
  (rtnl-route-nh-set-weight!  rtnl-route-nh-get-weight  _uint8)
  (rtnl-route-nh-set-ifindex! rtnl-route-nh-get-ifindex _int)
  (rtnl-route-nh-set-gateway! rtnl-route-nh-get-gateway _nl-addr-pointer/null)
  (rtnl-route-nh-set-realms!  rtnl-route-nh-get-realms  _uint32))


; vim:set ts=2 sw=2 et:
