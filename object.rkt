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

(struct obj ([methods #:mutable] [recvr #:mutable])
  #:property prop:procedure
  (λ (obj method . args)
    (let ([fn (hash-ref (obj-methods obj) method (λ () #f))]
          [r  (obj-recvr obj)])
      (cond
        [fn (apply fn args)]
        [r  (apply r method args)]
        [else (raise (make-exn:fail:contract:no-such-method
                      (format "no such method: ~a~n" method)
                      (current-continuation-marks)))]))))

(struct	exn:fail:contract:no-such-method exn:fail:contract ()
    #:extra-constructor-name
    make-exn:fail:contract:no-such-method
    #:transparent)

(define (send obj method . args)
  (unless (obj? obj)
    (raise-argument-error 'send "obj?" 0 obj))
  (apply obj method args))

(define (call obj method . args)
  (unless (obj? obj)
    (raise-argument-error 'call "obj?" 0 obj))
  (apply obj method args))

(define (obj/c method-map recvr-ctc)
  (make-contract
   #:name 'obj/c
   #:first-order obj?
   #:projection
   (λ (blame)
     (λ (obj)
       (if (obj? obj)
           (impersonate-struct
            obj
            obj-methods
            (λ (obj methods)
              (impersonate-hash
               methods
               (λ (hash method)
                 (values method
                         (λ (hash method fn)
                           (let ([ctc (hash-ref method-map method (λ () #f))])
                             (unless ctc
                               (raise-blame-error
                                (blame-swap blame) obj
                                "cannot call hidden method: ~e" method))
                             (((contract-projection ctc) blame) fn)))))
               (λ (hash key value) (values key value))
               (λ (hash key) key)
               (λ (hash key) key)))
            set-obj-methods!
            #f
            obj-recvr
            (λ (obj recvr)
              (if recvr
                  (((contract-projection recvr-ctc) blame) recvr)
                  #f))
           set-obj-recvr!
           #f)
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
             #:attr defn #'(define (recvr n . args) body ...)))
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
           (obj methods recvr)))]))

(define-syntax (def/ctc stx)
  (syntax-parse stx
    [(_ name:id (ctc ...)
        body ...)
     #'(define/contract name
         (fresh/c (_obj/c ctc ...))
         (let ()
           (def name body ...)
           name))]))

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
     [recv  (->i ([method (λ (n) (not (eq? n 'test)))]) #:rest [args any/c] [result () symbol?])])
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
  (check-not-exn (λ () (call obj2-c 'foo))))

(define opaque/c (_obj/c))

(define-syntax (fresh/c stx)
  (syntax-parse stx
    [(_ ctc:expr)
     #`(make-contract
        #:name 'fresh/c
        #:projection
        (λ (blame)
          (λ (value)
            (((contract-projection (begin (printf "creating contract ~e~n" ctc) ctc)) blame) value))))]))

(define/contract test
  (_obj/c
   (∃ X :> opaque/c)
   [make (-> integer? X)]
   [read (-> X integer?)])
  (let ()
    (def obj
      (to (make i)
          (def int
            (to (get) i))
          int)
      (to (read i)
          (_send i get)))
    obj))

(define test2
  (let ()
    (def obj
      (to (make i)
          (def int
            (to (get) i))
          int)
      (to (read i)
          (_send i get)))
    obj))
