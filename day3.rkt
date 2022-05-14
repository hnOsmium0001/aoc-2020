#lang racket

(require racket/file)

(define input (file->lines "inputs/day3.txt"))
(define map-width (string-length (car input)))
(define map-height (length input))

(define (thing-at input-map x y)
  (let ([actual-x (modulo x map-width)]
        [actual-y y])
    (string-ref (list-ref input-map actual-y) actual-x)))

(define (count-things-along-path x y delta-x delta-y)
  (cond
    [(>= y map-height) 0]
    [else (let ([thing (thing-at input x y)]
                [subcase-solution (count-things-along-path (+ x delta-x) (+ y delta-y) delta-x delta-y)])
            (cond
              [(eq? thing #\.) subcase-solution]
              [(eq? thing #\#) (+ subcase-solution 1)]))]))

;; Part 1
(define (part1)
  ;; Each time move right 3, down 1
  (count-things-along-path 0 0 3 1))

;; Part 2
(define (part2)
  (* (count-things-along-path 0 0 1 1)
     (count-things-along-path 0 0 3 1)
     (count-things-along-path 0 0 5 1)
     (count-things-along-path 0 0 7 1)
     (count-things-along-path 0 0 1 2)))
