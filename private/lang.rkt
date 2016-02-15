#lang racket

(require E-on-Racket/private/object
         racket/base
         racket/contract)

(provide (all-from-out E-on-Racket/private/object)
         (except-out (all-from-out racket/base) #%app)
         (all-from-out racket/contract)
         (rename-out [%app #%app]))

(define-syntax (%app stx)
  (syntax-case stx ()
    [(_ obj args ...)
     #'(if (#%app obj? obj)
           (#%app call obj 'run args ...)
           (#%app obj args ...))]))
