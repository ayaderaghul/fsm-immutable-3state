(require "1auto.rkt"
         "2match1.rkt"
         "3fit2.rkt"
         "4mutation1.rkt"
         "5scan12.rkt"
	"6test1.rkt"
         "7tv.rkt"
	"8out15.rkt")

;; generate population
(define (create-population N)
  (for/list
      ([n N])
    (generate-auto)))

(define A (create-population 100))



(define population-mean '())
(define demographic '())
;; evolve the population over cycles
;; N=100
(define (evolve population cycles speed mutation rounds-per-match)
  (let* ([N (length population)]
         ;;[demo (scan-2-types population)]
         [round-results (match-population population rounds-per-match)]
         [average-payoff (exact->inexact (/ (apply + (flatten round-results))
                                            (* rounds-per-match N)))]
         [accum-fitness (accumulate (payoff-percentages (flatten round-results)))]
         [survivors (drop population speed)]
         [successors
          (randomise-over-fitness accum-fitness population speed)]
         [before-mutation
          (shuffle (append survivors successors))]
         [new-population
          (mutate-populations mutation before-mutation)]
         )
    (set! population-mean
          (append population-mean (list average-payoff)))
    ;;(out-rank cycles population)
    ;;  (set! demographic
    ;;        (append demographic (list demo)))
    (if (zero? cycles)
        (begin
      ;;    (plot-dynamic demographic N)
          (plot-mean population-mean)
          population
          )
        (evolve new-population (sub1 cycles) speed mutation rounds-per-match))))
