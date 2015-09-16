#lang racket
(provide plot-mean
	plot-dynamic)

(require plot)
(plot-new-window? #t)

;; TV
(define (plot-mean data)
  (let* ([l (length data)]
         [coors (map list (build-list l values)
                     data)])
    (plot (lines coors))))

(define (plot-dynamic data N)
  (plot (lines data)
        #:x-min 0 #:x-max N
        #:y-min 0 #:y-max N))
#|
(define dynamic-frame (new frame%
                           [label "dynamic"]
                           [width 400]
                           [height 400]))
(define dynamic-canvas (new canvas%
                            [parent dynamic-frame]))
(define dynamic-dc (send dynamic-canvas get-dc))
|#
