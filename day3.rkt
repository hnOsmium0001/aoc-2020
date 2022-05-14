#lang racket

(require racket/file)

(define input (file->lines "inputs/day3.txt"))
(define map-width (string-length (car input)))
(define map-height (length input))

(define (thing-at input-map x y)
  (let ([actual-x (modulo x map-width)]
        [actual-y y])
    (string-ref (list-ref input-map actual-y) actual-x)))

;; Part 1
(define (solve-part1 x y)
  (cond
    [(>= y map-height) 0]
    [else (let ([thing (thing-at input x y)]
                [subcase-solution (solve-part1 (+ x 3) (+ y 1))])
            (cond
              [(eq? thing #\.) subcase-solution]
              [(eq? thing #\#) (+ subcase-solution 1)]))]))
(define (part1)
  (solve-part1 0 0))
