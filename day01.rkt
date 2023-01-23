#lang racket

(require racket/file)

(define input (map string->number (file->lines "inputs/day1.txt")))

;; Part 1
#|
(for* ([a input]
       [b input])
  (cond
    [(= (+ a b) 2020) (printf "~a * ~a = ~a\n" a b (* a b))]))
|#
(define (part1)
  (for*/first ([a input]
               [b input]
               #:when (= (+ a b) 2020))
    (printf "~a * ~a = ~a\n" a b (* a b))))

;; Part 2
(define (part2)
  (for*/first ([a input]
               [b input]
               [c input]
               #:when (= (+ a b c) 2020))
    (printf "~a * ~a * ~a = ~a\n" a b c (* a b c))))
