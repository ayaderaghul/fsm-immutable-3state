#lang racket
(require "1auto.rkt")
(require "csv.rkt")
(require "5scan12.rkt")
(provide out-data
         out-mean
         out-rank
         chop
         make-body
         
         generate-codes
         export-codes
         )

;; data:
;; '((1 2..)
;;   (2 3..))

;; if needed, map list data..
(define (out-data filename data)
  (define out (open-output-file filename #:mode 'text #:exists 'append))
  (write-table data out)
  (close-output-port out))

(define (out-mean data)
  (out-data "mean" (map list data)))

(define (out-rank day population)
  (out-data "rank" (append (list (list day))
                           (map list (rank population)))))


(define (chop automaton)
  (let ([states (automaton-states automaton)])
    (map (lambda (a-state)
           (drop (vector->list (struct->vector a-state)) 2))
         states)))

(define (convert-end-points state-ends)
  (let ([a (first state-ends)]
        [b (second state-ends)]
        [c (third state-ends)])
    (cond
     [(= a b c) (list "L,M,H" "L,M,H" "L,M,H")]
     [(= a b) (list "L,M" "L,M" "H")]
     [(= a c) (list "L,H" "M" "L,H")]
     [(= b c) (list "L" "M,H" "M,H")]
     [else (list "L" "M" "H")])))


(define (collect-end-points automaton)
  (flatten
   (for/list ([i 3])
     (convert-end-points (list-ref (chop automaton) i)))))

(define (make-body automaton)
  (let ([end-points (flatten (chop automaton))]
        [end-labels (collect-end-points automaton)])
    (list*
     "Graph[{-1 -> ~a"
     (remove-duplicates
      (for/list ([i 9])
        (format
         (list-ref trajectories i)
         (list-ref end-points i)
         (list-ref end-labels i))))
     )))

;; export matha code

(define trajectories
  (list
   "Labeled[0 -> ~a, ~s]" "Labeled[0 -> ~a, ~s]" "Labeled[0 -> ~a, ~s]"
   "Labeled[1 -> ~a, ~s]" "Labeled[1 -> ~a, ~s]" "Labeled[1 -> ~a, ~s]"
   "Labeled[2 -> ~a, ~s]" "Labeled[2 -> ~a, ~s]" "Labeled[2 -> ~a, ~s]"))

(define (generate-code a-list posn x)
  (let ([automaton (list-ref a-list posn)])
    (format
     (string-append*
      (append
       (list "VertexCircle[{xc_, yc_}, name_, {w_, h_}] := Disk[{xc, yc}, .1];
")
       (list "~aGraph =
")
       (cdr
        (append*
         (map (lambda (x) (list ", " x))
              (make-body automaton))))
       (list "},
   EdgeShapeFunction -> GraphElementData[\"EdgeShapeFunction\", \"FilledArrow\"],
   VertexStyle -> LightGray,
   VertexShapeFunction -> VertexCircle,
   VertexLabels -> { 0 -> Placed[~s, Center],
   1 -> Placed[~s, Center],
   2 -> Placed[~s, Center] }
  ];
")
       (list
        "Export[\"~a.png\", ~aGraph];
"
        )))
     (name x posn)
     (current-claim automaton)
     (name x posn)
     (name x posn))))


(define (generate-codes a-list x)
  (for/list ([i (length a-list)])
    (generate-code a-list i x)))

(define (export-codes a-list x)
  (for ([i (length a-list)])
    (with-output-to-file "auto-code"
      (lambda () (printf (generate-code a-list i x)))
      #:exists 'append)))

(define (name x n)
  (string->symbol (string-append x (number->string n))))
