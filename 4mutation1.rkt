#lang racket
(require "1auto.rkt")

(provide
 set-immutable
 mutate
 mutate-populations
	mutate-random)

(define (set-immutable a-list posn new-value)
  (append
   (take a-list posn)
   (list new-value)
   (drop a-list (add1 posn))))

(define (mutate an-auto)
  (let ([flattened (flatten-auto an-auto)])
    (make-auto (set-immutable flattened
                              (random 10)
                              (random 3)))))

(define (mutate-populations countdown population)
  (let* ([l (length population)]
         [r (random l)]
         [mutated (mutate (list-ref population r))]
         [new-population (set-immutable population r mutated)])
    (if (zero? countdown)
        population
        (mutate-populations (sub1 countdown) new-population))))

(define (mutate-random countdown population)
  (let* ([l (length population)]
         [r (random l)]
         [mutated (generate-auto)]
         [new-population (set-immutable population r mutated)])
    (if (zero? countdown)
        population
        (mutate-random (sub1 countdown) new-population))))
