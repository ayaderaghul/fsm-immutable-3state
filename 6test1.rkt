#lang racket

(require "1auto.rkt")

(provide create-test-population
         create-test-4-types)

;; create test population

(define (create-test-population l m h)
  (shuffle
   (append
    (for/list
        ([n l])
      (generate-lows))
    (for/list
        ([n m])
      (generate-mediums))
    (for/list
        ([n h])
      (generate-highs)))))

(define (generate-lows)
  (make-auto
   (list 0
         (random 3) (random 3) (random 3)
         (random 3) (random 3) (random 3)
         (random 3) (random 3) (random 3))))

(define (generate-mediums)
  (make-auto
   (list 1
         (random 3) (random 3) (random 3)
         (random 3) (random 3) (random 3)
         (random 3) (random 3) (random 3))))

(define (generate-highs)
  (make-auto
   (list 2
         (random 3) (random 3) (random 3)
         (random 3) (random 3) (random 3)
         (random 3) (random 3) (random 3))))

(define (create-test-4-types h m l a)
  (shuffle
   (append
    (for/list
        ([x h]) all-highs)
    (for/list
        ([y m]) all-mediums)
    (for/list
        ([z l]) all-lows)
    (for/list
        ([t a]) accommodator))))
