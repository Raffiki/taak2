#lang r6rs

;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*                                                                 *-*-
;-*-*                Hash Tables (External Chaining)                  *-*-
;-*-*                                                                 *-*-
;-*-*                       Wolfgang De Meuter                        *-*-
;-*-*                 2008 Programming Technology Lab                 *-*-
;-*-*                   Vrije Universiteit Brussel                    *-*-
;-*-*                                                                 *-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
;-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-

(library 
 (dictionary)
 (export new dictionary? insert! delete! find print)
 (import (rnrs base)
         (srfi :9)
         (rnrs mutable-pairs)
         (only (rnrs io simple) display newline))

 
 (define make-assoc cons)
 (define assoc-key car)
 (define assoc-value cdr)
 
 (define-record-type external-chaining
   (make s h e)
   dictionary?
   (s storage)
   (h hash-function)
   (e equality))

(define (print dict)
  (display (storage dict)))
 
 (define (new ==? M)
   (make (make-vector M ()) (lambda (x) x) ==?))
 
 (define (insert! table key val)
   (define vector (storage table))
   (define h (hash-function table))
   (define ==? (equality table))
   (define home-address (h key))
   (define assoc (make-assoc key val))
   (vector-set! vector home-address (cons assoc (vector-ref vector home-address))))
 
 (define (find table key)
   (define vector (storage table))
   (define h (hash-function table))
   (define ==? (equality table))
   (define home-address (h key))
   (vector-ref vector home-address))
 
 (define (delete! table key)
   (define vector (storage table))
   (define h (hash-function table))
   (define ==? (equality table))
   (define home-address (h key))
   (let delete-from-bucket 
     ((prev '())
      (next! (lambda (ignore next) (vector-set! vector home-address next)))
      (next (vector-ref vector home-address)))
     (cond 
       ((null? next)
        #f)
       ((==? (assoc-key (car next)) key)
        (next! prev (cdr next))
        table)
       (else
        (delete-from-bucket next set-cdr! (cdr next)))))
   table))