#lang racket

(require "object.rkt")

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

(define (amount? v)
  (and (integer? v)
       (not (negative? v))))

(define/contract (make-mint)
  (-> (fresh/c (obj/c
                (∃ P :> (obj/c
                         [getBalance (-> amount?)]
                         [deposit    (-> amount? P void)]
                         [sprout     (-> P)]))
                [makePurse (-> amount? P)])))
  (def mint
    (to (makePurse balance)
        (def purse
          (to (getBalance) balance)
          (to (deposit amount src)
              (send src deduct amount)
              (set! balance (+ balance amount)))
          (to (sprout)
              (send mint makePurse 0))
          (to (deduct amount)
              (unless (<= amount balance)
                (error "insufficient funds"))
              (set! balance (- balance amount))))
        purse))
  mint)

(define carolMint (make-mint))
(define aliceMainPurse (send carolMint makePurse 1000))
(define bobMainPurse (send carolMint makePurse 0))
(define paymentForBob (send aliceMainPurse sprout))
(send paymentForBob deposit 100 aliceMainPurse)

(define aliceMint (make-mint))
(define aliceFakePurse (send aliceMint makePurse 1000))
(send aliceMainPurse deposit 1000 aliceFakePurse)