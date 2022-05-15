#lang racket

(require racket/file)

(define (parse-binary-partition str)
  (define res 0)
  (for ([char str])
    (define prev-res (arithmetic-shift res 1))
    (match char
      [(or #\F #\L) (set! res (bitwise-ior prev-res 0))]
      [(or #\B #\R) (set! res (bitwise-ior prev-res 1))]))
  res)

(define (parse-boarding-pass bp)
  (let ([row-str (substring bp 0 7)]
        [col-str (substring bp 7 10)])
    (cons (parse-binary-partition row-str)
          (parse-binary-partition col-str))))

(define input (map parse-boarding-pass (file->lines "inputs/day5.txt")))

(define (make-seat-id bp)
  (match bp
    [(cons row col) (+ (* row 8) col)]))

;; Part 1
(define (part1)
  (let* ([seat-ids (map make-seat-id input)]
         [max-seat-id (argmax identity seat-ids)])
    max-seat-id))
