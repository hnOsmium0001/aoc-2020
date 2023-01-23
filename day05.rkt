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

;; Part 2
(define (consecutive-id? a b)
  (eq? a (- b 2)))

(define (part2)
  (let* ([seat-ids (sort (map make-seat-id input) <)]
         [from-i 0]
         [to-i (- (length input) 2)])
    (for/first ([i (in-inclusive-range from-i to-i)]
                #:when (consecutive-id? (list-ref seat-ids i)
                                        (list-ref seat-ids (add1 i))))
      (+ (list-ref seat-ids i) 1))))

;; Part 2 (using alternate sliding window method)
(define (list-fits-window? lst window-size)
  (cond
    [(= window-size 0) #t]
    [(empty? lst) #f]
    [else (list-fits-window? (cdr lst) (sub1 window-size))]))

(define (sliding-window lst window-size predicate)
  (cond
    [(not (list-fits-window? lst window-size)) '()]
    [else (let ([window (take lst window-size)])
            (if (predicate window)
                window
                (sliding-window (drop lst window-size) window-size predicate)))]))

(define (part2-alt)
  (let ([ids (sliding-window (sort (map make-seat-id input) <)
                             2
                             (lambda (ids) (consecutive-id? (car ids) (cadr ids))))])
    (+ (car ids) 1)))
