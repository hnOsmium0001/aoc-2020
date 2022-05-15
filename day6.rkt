#lang racket

(require racket/file
         data/bit-vector)

(define (bit-vector-and vec1 vec2)
  (for/bit-vector ([a vec1]
                   [b vec2])
    (and a b)))

(define (bit-vector-or vec1 vec2)
  (for/bit-vector ([a vec1]
                   [b vec2])
    (or a b)))

(define (parse-response txt)
  (define res (make-bit-vector 27))
  (for ([c txt])
    (define idx (- (char->integer c)
                   (char->integer #\a)))
    (bit-vector-set! res idx #t))
  res)

(define input
  (map (lambda (responses)
         (map (lambda (response) (parse-response response))
              (string-split responses)))
       (string-split (file->string "inputs/day6.txt") "\n\n")))

;; Part 1
(define (part1)
  (for/sum ([form input])
    (bit-vector-popcount (foldl bit-vector-or (make-bit-vector 27 #f) form))))

;; Part 2
(define (part2)
  (for/sum ([form input])
    (bit-vector-popcount (foldl bit-vector-and (make-bit-vector 27 #t) form))))
