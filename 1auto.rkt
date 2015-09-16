#lang racket
(provide struct-out automaton
         struct-out state

         state-result0
         state-result1
         state-result2
         automaton-states
         automaton-current-state

         current-state
         current-claim
         react
         update

         generate-auto
         flatten-auto
         make-auto

	accommodator
	all-highs
	all-mediums
	all-lows
         )

;; AUTOMATON
;; if automaton has only 3 states -> state name = state position
(struct state (result2 result1 result0) #:transparent)
;; a state: name and many transition rules
(struct automaton (current-state states) #:transparent)
;; the machine itself: current state + states

(define (current-state an-auto)
  (list-ref
   (automaton-states an-auto)
   (automaton-current-state an-auto)))
(define (current-claim an-auto)
  (automaton-current-state an-auto))

;; an event happens
;; output: claim, not state
(define (react an-event an-auto)
  (let ([current (current-state an-auto)])
    (cond [(= an-event 0) (state-result0 current)]
          [(= an-event 1) (state-result1 current)]
          [(= an-event 2) (state-result2 current)])))

(define (update old-auto new-state)
  (struct-copy automaton old-auto [current-state new-state]))

; generate random automaton (random current state, random result-state
; after each event
(define (generate-auto)
  (automaton (random 3)
             (list (state (random 3) (random 3) (random 3))
                   (state (random 3) (random 3) (random 3))
                   (state (random 3) (random 3) (random 3)))))

(define (flatten-state a-state)
  (map (lambda (f) (f a-state))
       (list
        state-result0
        state-result1
        state-result2)))

(define (flatten-auto an-auto)
  (append*
   (list (automaton-current-state an-auto))
   (map flatten-state (automaton-states an-auto))))

(define (make-auto a-list)
  (automaton (first a-list)
             (list (apply state (take (drop a-list 1) 3))
                   (apply state (take (drop a-list 4) 3))
                   (apply state (take-right a-list 3)))))

;; example

(define accommodator
  (make-auto (list 1 0 1 2 0 1 2 0 1 2)))
(define all-highs
  (make-auto (list 2 2 2 2 2 2 2 2 2 2)))
(define all-mediums
  (make-auto (list 1 1 1 1 1 1 1 1 1 1)))
(define all-lows
  (make-auto (list 0 0 0 0 0 0 0 0 0 0)))
