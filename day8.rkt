#lang racket

(require racket/file
         data/monad
         data/applicative
         megaparsack
         megaparsack/text)

(define instruction/p
  (do [ins <- (repeat/p 3 letter/p)]
      space/p
      [sign <- (or/p (char/p #\+)
                     (char/p #\-))]
      [num <- integer/p]
      eof/p
      (pure (cons (list->string ins)
                  (* num
                     (match sign
                       [#\+ +1]
                       [#\- -1]))))))

;; Read the input file line by line, parse each line as an instruction and record as a vector
;; (vector is for O(1) lookup by index)
(define input
  (for/vector ([line (file->lines "inputs/day8.txt")])
    (parse-result! (parse-string instruction/p line))))

(struct machine-state ([accumulator #:mutable]))
(define (make-machine-state) (machine-state 0))

;; Part 1

;; Execute the instruction at the `pc` location. If this location is visited (has an entry in `visited`),
;; return `pc` directly. Otherwise recurse with the updated `pc` value after executing the instruction
(define (exec-no-repeat visited state pc)
  (if (vector-ref visited pc)
      pc
      (let* ([ins (vector-ref input pc)]
             [op (car ins)]
             [arg (cdr ins)])
        (vector-set! visited pc #t)
        (match op
          ["acc"
           (define acc (machine-state-accumulator state))
           (set-machine-state-accumulator! state (+ acc arg))
           (exec-no-repeat visited state (+ pc 1))]
          ["jmp"
           (exec-no-repeat visited state (+ pc arg))]
          ["nop"
           (exec-no-repeat visited state (+ pc 1))]))))

(define (part1)
  (let ([visited (make-vector (vector-length input) #f)]
        [state (make-machine-state)])
    (let ([pc-on-repeat (exec-no-repeat visited state 0)])
      (cons pc-on-repeat
            (machine-state-accumulator state)))))
