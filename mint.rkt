#lang racket

(require bounded)

(define (makeBrandPair)
  (let ()
    (define-member-name unseal-envelope (generate-member-key))
    (define envelope%
      (class object%
        (init value)
        (define enclosed value)
        (define/public (unseal-envelope)
          enclosed)
        (super-new)))
    (define sealer%
      (class object%
        (define/public (seal value)
          (new envelope% [value value]))
        (super-new)))
    (define unsealer%
      (class object%
        (define/public (unseal env)
          (send env unseal-envelope))
        (super-new)))
    (values (new sealer%) (new unsealer%))))

(define (amount? v)
  (and (integer? v)
       (not (negative? v))))

(define mint%
  (class object%
    (init)
    (define-values (sealer unsealer) (makeBrandPair))
    (define purse%
      (class object%
        (init amt mint)
        (define parent mint)
        (define balance amt)
        (define/public (getBalance)
          balance)
        (define/public (sprout)
          (send parent makePurse 0))
        (define/public (getDecr)
          (send sealer seal
                (lambda (amt)
                  (set! balance (- balance amt)))))
        (define/public (deposit amt src)
          ((send unsealer unseal (send src getDecr)) amt)
          (set! balance (+ balance amt)))
        (super-new)))
    (define/public (makePurse balance)
      (new purse% [amt balance] [mint this]))
    (super-new)))

(define carolMint (new mint%))
(define aliceMainPurse (send carolMint makePurse 1000))
(define bobMainPurse (send carolMint makePurse 0))
(define paymentForBob (send aliceMainPurse sprout))

(define simplepurse%
  (class object%
    (init mint initial)
    (define balance initial)
    (define parent mint)
    (define/public (getBalance)
      balance)
    (define/public (deposit amount src)
      (send src deduct amount)
      (set! balance (+ balance amount)))
    (define/public (sprout)
      (send parent makePurse 0))
    (define/public (deduct amount)
      (set! balance (- balance amount)))
    (super-new)))


(define simplemint%
  (class object%
    (init)
    (define/public (makePurse balance)
      (new simplepurse% [initial balance] [mint this]))
    (super-new)))

(define/contract (make-simplemint)
  (->i ()
       [result ()
               (letrec ([purse (object/c-opaque
                                [getBalance (->m amount?)]
                                [deposit (->m amount? (recursive-contract bound) void)]
                                [sprout (->m (recursive-contract purse))])]
                        [bound (new-bounded-∃/c 'P purse)])
                 (object/c-opaque
                  [makePurse (->m amount? bound)]))])
  (new simplemint%))

(define simpleCarolMint (make-simplemint))
(define simpleAliceMainPurse (send simpleCarolMint makePurse 1000))
(define simpleBobMainPurse (send simpleCarolMint makePurse 0))
(define simplePaymentForBob (send simpleAliceMainPurse sprout))
(send simplePaymentForBob deposit 100 simpleAliceMainPurse)

(define simpleAliceMint (make-simplemint))
(define simpleAliceFakePurse (send simpleAliceMint makePurse 1000))
;(send simpleAliceMainPurse deposit 1000 simpleAliceFakePurse)


;; def makeMint(name) :any {
;;     def [sealer, unsealer] := makeBrandPair(name)
;;     def mint {
;;         to __printOn(out) :void { out.print(`<$name's mint>`) }
 
;;         to makePurse(var balance :(int >= 0)) :any {
;;             def decr(amount :(0..balance)) :void {
;;                 balance -= amount
;;             }
;;             def purse {
;;                 to __printOn(out) :void {
;;                     out.print(`<has $balance $name bucks>`)
;;                 }
;;                 to getBalance() :int { return balance }
;;                 to sprout()     :any { return mint.makePurse(0) }
;;                 to getDecr()    :any { return sealer.seal(decr) }

;;                 to deposit(amount :int, src) :void {
;;                     unsealer.unseal(src.getDecr())(amount)
;;                     balance += amount
;;                 }
;;             }
;;             return purse
;;         }
;;     }
;;     return mint
;; }
