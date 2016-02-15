#lang racket

(provide def def/ctc obj? call exn:fail:contract:no-such-method? opaque/c fresh/c
         (rename-out [_send send]
                     [_obj/c obj/c]))

(require syntax/parse
         syntax/parse/lib/function-header
         bounded
         (for-syntax syntax/parse
                     syntax/parse/lib/function-header
                     racket))

(struct obj (name [methods #:mutable] [recvr #:mutable])
  #:methods gen:custom-write
  [(define (write-proc obj port mode)
     (write-string (format "#<obj: ~a>" (obj-name obj)) port))])

(struct	exn:fail:contract:no-such-method exn:fail:contract ()
    #:extra-constructor-name
    make-exn:fail:contract:no-such-method
    #:transparent)

(define (send obj method . args)
  (unless (obj? obj)
    (raise-argument-error 'send "obj?" 0 obj))
  (let ([fn (hash-ref (obj-methods obj) method (λ () #f))]
        [r  (obj-recvr obj)])
    (cond
      [fn (apply fn args)]
      [r  (apply (r method) args)]
      [else (raise (make-exn:fail:contract:no-such-method
                    (format "no such method: ~a~n" method)
                    (current-continuation-marks)))])))

(define (call obj method . args)
  (unless (obj? obj)
    (raise-argument-error 'call "obj?" 0 obj))
  (let ([fn (hash-ref (obj-methods obj) method (λ () #f))]
        [r  (obj-recvr obj)])
    (cond
      [fn (apply fn args)]
      [r  (apply (r method) args)]
      [else (raise (make-exn:fail:contract:no-such-method
                    (format "no such method: ~a~n" method)
                    (current-continuation-marks)))])))

(define-values (obj/c-prop obj/c? obj/c-accessor)
  (make-impersonator-property 'obj/c))

(define (obj/c method-map recvr-fn)
  (make-contract
   #:name 'obj/c
   #:first-order obj?
   #:projection
   (λ (blame)
     (λ (obj)
       (if (obj? obj)
           (impersonate-struct
            obj
            set-obj-methods!
            #f
            obj-methods
            (λ (obj methods)
              (impersonate-hash
               methods
               (λ (hash method)
                 (values method
                         (λ (hash method fn)
                           (let ([ctc (hash-ref method-map method (λ () #f))])
                             (cond
                               [ctc
                                (((contract-projection ctc) blame) fn)]
                               [recvr-fn
                                (((contract-projection
                                   (let ([ctc (recvr-fn method)])
                                     (unless ctc
                                       (raise-blame-error
                                        (blame-swap blame) obj
                                        "cannot call hidden method: ~e" method))
                                     ctc)) blame) fn)]
                               [else (raise-blame-error
                                      (blame-swap blame) obj
                                      "cannot call hidden method: ~e" method)])))))
               (λ (hash key value) (values key value))
               (λ (hash key) key)
               (λ (hash key) key)))
            set-obj-recvr!
            #f
            obj-recvr
            (λ (obj recvr)
              (if recvr
                  (((contract-projection
                     (if recvr-fn
                         (->i ([method symbol?]) [result (method)
                                                         (let ([ctc (recvr-fn method)])
                                                           (unless ctc
                                                             (raise-blame-error
                                                              (blame-swap blame) obj
                                                              "cannot call hidden method: ~e" method))
                                                           ctc)])
                         (->i ([method symbol?]) [result (method)
                                                         (make-contract
                                                          #:name 'hidden
                                                          #:first-order procedure?
                                                          #:projection
                                                          (λ (blame)
                                                            (λ (val)
                                                              (raise-blame-error
                                                               (blame-swap blame) obj
                                                               "cannot call hidden method: ~e" method))))])))
                    blame) recvr)
                  #f))
            obj/c-prop #t)
           (raise-blame-error
            blame obj
            '(expected "an object" given: "~e")
            obj))))))

(begin-for-syntax
  (define-syntax-class method
    #:datum-literals (to)
    (pattern (to header:function-header body ...+)
             #:attr defn #'(define header body ...)
             #:attr name #'header.name))
  (define-syntax-class recv
    #:datum-literals (recv)
    (pattern (recv (n args) body ...+)
             #:attr defn #'(define (recvr n) (λ args body ...))))
  (define-syntax-class quantifier
    #:datum-literals (∀ ∃ :>)
    (pattern (∀ v:id :> bound:expr)
             #:attr defn #'(define v (new-bounded-∀/c (quote v) bound)))
    (pattern (∃ v:id :> bound:expr)
             #:attr defn #'(define v (new-bounded-∃/c (quote v) bound)))))

(define-syntax (_send stx)
  (syntax-parse stx
    [(_ obj:expr name:id arg:expr ...)
     #'(send obj (quote name) arg ...)]))

(define-syntax (def stx)
  (syntax-parse stx
    [(_ name:id m:method ...
        (~optional r:recv
                   #:defaults ([r.defn #'(define recvr #f)])))
     #`(define name
         (let ()
           m.defn ...
           (define methods
             (let ([hash (make-hash)])
               (for ([k (list (quote m.name) ...)]
                     [v (list m.name ...)])
                 (hash-set! hash k v))
               hash))
           r.defn
           (obj (quote name) methods recvr)))]
    [(_ header:function-header body ...+)
     #`(def header.name (to (run #,@#'header.args) body ...))]))

(define-syntax (def/ctc stx)
  (syntax-parse stx
    [(_ name:id (ctc ...)
        body ...)
     #'(define/contract name
         (fresh/c (_obj/c ctc ...))
         (let ()
           (def name body ...)
           name))]
    [(_ header:function-header ctc:expr body ...+)
     #'(def/ctc header.name
         ([run ctc])
         (to (run #,@#'header.args) body ...))]))

(define-syntax (_obj/c stx)
  (syntax-parse stx
    #:datum-literals (recv)
    [(_ q:quantifier ... [name:id ctc:expr] ... [recv rctc:expr])
     #'(let ([hash (make-hash)])
         q.defn ...
         (recursive-contract
          (let ()
            (for ([k (list (quote name) ...)]
                  [v (list ctc ...)])
              (hash-set! hash k v))
            (obj/c hash rctc))))]
    [(_ q:quantifier ... [name:id ctc:expr] ...)
     #'(let ([hash (make-hash)])
         q.defn ...
         (recursive-contract
          (let ()
            (for ([k (list (quote name) ...)]
                  [v (list ctc ...)])
              (hash-set! hash k v))
            (obj/c hash #f))))]))

(define opaque/c (_obj/c))

(define-syntax (fresh/c stx)
  (syntax-parse stx
    [(_ ctc:expr)
     #`(make-contract
        #:name 'fresh/c
        #:projection
        (λ (blame)
          (λ (value)
            (((contract-projection ctc) blame) value))))]))

(module* test racket
  (require rackunit)
  (require (submod ".."))
  
  (def obj1
    (to (hello) 0)
    (to (world) 1)
    (to (add n m) (+ n m)))
  
  (def obj2
    (to (hello) 0)
    (to (world) 1)
    (to (add n m) (+ n m))
    (recv (name args)
          (cond
            [(eq? name 'test) 0]
            [(eq? name 'foo) 'done]
            [else (error "bad method")])))

  (define/contract obj1-def/c
    (obj/c
     [hello (-> integer?)]
     [world (-> symbol?)])
    (let ()
      (def obj
        (to (hello) 0)
        (to (world) 1)
        (to (add n m) (+ n m)))
      obj))

  (define/contract obj2-def/c
    (obj/c
     [hello (-> integer?)]
     [world (-> integer?)]
     [add   (-> integer? integer? integer?)]
     [recv  (λ (method)
              (cond
                [(eq? method 'test) #f]
                [else (-> symbol?)]))])
    (let ()
      (def obj
        (to (hello) 0)
        (to (world) 1)
        (to (add n m) (+ n m))
        (recv (name args)
              (cond
                [(eq? name 'test) 0]
                [(eq? name 'foo) 'done]
                [else (error "bad method")])))
      obj))
  
  (def/ctc obj1-c
    ([hello (-> integer?)]
     [world (-> symbol?)])
    (to (hello) 0)
    (to (world) 1)
    (to (add n m) (+ n m)))
  
  (def/ctc obj2-c
    ([hello (-> integer?)]
     [world (-> integer?)]
     [add   (-> integer? integer? integer?)]
     [recv  (λ (method)
              (cond
                [(eq? method 'test) #f]
                [else (-> symbol?)]))])
    (to (hello) 0)
    (to (world) 1)
    (to (add n m) (+ n m))
    (recv (name args)
          (cond
            [(eq? name 'test) 0]
            [(eq? name 'foo) 'done]
            [else (error "bad method")])))
  
  (check-not-exn (λ () (send obj1 hello)))
  (check-not-exn (λ () (send obj2 add 1 2)))
  (check-exn exn:fail:contract:no-such-method? (λ () (send obj1 test)))
  (check-not-exn (λ () (send obj2 test)))
  (check-exn exn:fail? (λ () (send obj2 bar)))
  (check-exn exn:fail? (λ () (send 3 hello)))
  
  (check-not-exn (λ () (call obj1 'hello)))
  (check-not-exn (λ () (call obj2 'add 1 2)))
  (check-exn exn:fail:contract:no-such-method? (λ () (call obj1 'test)))
  (check-not-exn (λ () (call obj2 'test)))
  (check-exn exn:fail? (λ () (call obj2 'bar)))
  (check-exn exn:fail? (λ () (call 3 'hello)))

  (check-not-exn (λ () (send obj1-c hello)))
  (check-exn exn:fail:contract? (λ () (send obj1-c world)))
  (check-exn exn:fail:contract? (λ () (send obj1-c add 1 2)))
  (check-not-exn (λ () (send obj2-c hello)))
  (check-not-exn (λ () (send obj2-c world)))
  (check-not-exn (λ () (send obj2-c add 1 2)))
  (check-exn exn:fail:contract? (λ () (send obj2-c test)))
  (check-not-exn (λ () (send obj2-c foo)))

  (check-not-exn (λ () (call obj1-c 'hello)))
  (check-exn exn:fail:contract? (λ () (call obj1-c 'world)))
  (check-exn exn:fail:contract? (λ () (call obj1-c 'add 1 2)))
  (check-not-exn (λ () (call obj2-c 'hello)))
  (check-not-exn (λ () (call obj2-c 'world)))
  (check-not-exn (λ () (call obj2-c 'add 1 2)))
  (check-exn exn:fail:contract? (λ () (call obj2-c 'test)))
  (check-not-exn (λ () (call obj2-c 'foo)))

  (check-not-exn (λ () (send obj1-def/c hello)))
  (check-exn exn:fail:contract? (λ () (send obj1-def/c world)))
  (check-exn exn:fail:contract? (λ () (send obj1-def/c add 1 2)))
  (check-not-exn (λ () (send obj2-def/c hello)))
  (check-not-exn (λ () (send obj2-def/c world)))
  (check-not-exn (λ () (send obj2-def/c add 1 2)))
  (check-exn exn:fail:contract? (λ () (send obj2-def/c test)))
  (check-not-exn (λ () (send obj2-def/c foo)))

  (check-not-exn (λ () (call obj1-def/c 'hello)))
  (check-exn exn:fail:contract? (λ () (call obj1-def/c 'world)))
  (check-exn exn:fail:contract? (λ () (call obj1-def/c 'add 1 2)))
  (check-not-exn (λ () (call obj2-def/c 'hello)))
  (check-not-exn (λ () (call obj2-def/c 'world)))
  (check-not-exn (λ () (call obj2-def/c 'add 1 2)))
  (check-exn exn:fail:contract? (λ () (call obj2-def/c 'test)))
  (check-not-exn (λ () (call obj2-def/c 'foo))))
