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
;; If `pc` is set to one-past-the-end of the array of instructions after execution (i.e. =(vector-length input)=)
;; `pc` is returned which will be equal to =(vector-length input)=.
(define (exec-no-repeat visited state pc [instructions input])
  (cond
    [(= pc (vector-length instructions)) pc]
    [(vector-ref visited pc) pc]
    [else
     (let* ([ins (vector-ref instructions pc)]
            [op (car ins)]
            [arg (cdr ins)])
       (vector-set! visited pc #t)
       (match op
         ["acc"
          (define acc (machine-state-accumulator state))
          (set-machine-state-accumulator! state (+ acc arg))
          (exec-no-repeat visited state (+ pc 1) instructions)]
         ["jmp"
          (exec-no-repeat visited state (+ pc arg) instructions)]
         ["nop"
          (exec-no-repeat visited state (+ pc 1) instructions)]))]))

(define (part1)
  (let ([visited (make-vector (vector-length input) #f)]
        [state (make-machine-state)])
    (let ([pc-on-repeat (exec-no-repeat visited state 0)])
      (cons pc-on-repeat
            (machine-state-accumulator state)))))

;; Part 2
(define (part2)
  (define instructions (vector-copy input))
  (define result null)
  (for ([i (in-range 0 (vector-length input))])
    (let* ([ins (vector-ref instructions i)]
           [op (car ins)]
           [arg (cdr ins)])
      (if (equal? op "jmp")
          (let ([visited (make-vector (vector-length instructions) #f)]
                [state (make-machine-state)])
            (display i)
            (display "\n")
            (vector-set! instructions i (cons "nop" arg))
            (if (= (vector-length instructions)
                   (exec-no-repeat visited state 0 instructions))
                (set! result (cons i
                                   (machine-state-accumulator state)))
                null)
            (vector-set! instructions i (cons "jmp" arg)))
          null)))
  result)
