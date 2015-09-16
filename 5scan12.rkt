#lang racket
(require "1auto.rkt")
(require "2match1.rkt")

(provide scan-init
         scan
         scan-lm
	scan-2-types
         rank
         top
         contest)

;; SCAN
(define (scan-init population)
  (foldl
   (lambda (au h)
     (hash-update h
                  (current-claim au)
                  add1 0))
   (hash)
   population))

(define (scan population)
  (foldl
   (lambda (au h)
     (hash-update h au add1 0))
   (hash)
   population))

(define (scan-flattened population)
  (let ([flattened (map flatten-auto population)])
    (foldl
     (lambda (au h)
       (hash-update h au add1 0))
   (hash)
   flattened)))

(define (scan-2-types population)
  (let ([scanned (scan-flattened population)])
    (list
     (hash-ref* scanned (list 1 1 1 1 1 1 1 1 1 1))
     (hash-ref* scanned (list 2 2 2 2 2 2 2 2 2 2)))))

(define (rank population)
  (let ([ranking (hash->list (scan-flattened population))])
    (sort ranking > #:key cdr)))

(define (hash-ref* a-hash a-key)
  (if (hash-has-key? a-hash a-key)
      (hash-ref a-hash a-key)
      0))
(define (scan-lm population)
  (let ([scanned (scan-init population)])
    (list
     (hash-ref* scanned 0)
     (hash-ref* scanned 1))))


;; TOP
(define (top t population)
  (let* ([top-autos (map car (rank population))]
         [automaton (map make-auto (take top-autos t))])
    (for/list ([i t])
      (eval
       (list 'define (x->ax i)
             (list-ref automaton i))))))

(define (x->ax x)
  (string->symbol (string-append "a" (number->string x))))

(define (generate-ax a-list)
  (map x->ax a-list))

(define (contest an-auto a-list)
  (for/list ([n (length a-list)])
    (match-pair (list an-auto (list-ref a-list n)) 10)))
