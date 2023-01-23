#lang racket

(require racket/file)

(define input (map string->number (file->lines "inputs/day9.txt")))

(define (rotate! buffer)
  (let aux ([i 1])
    (cond
      ;; At end of buffer
      [(= i (vector-length buffer)) null]
      [else
       (begin
         (vector-set! buffer (- i 1) (vector-ref buffer i))
         (aux (+ i 1)))])))

(define (push-into-buffer! buffer num)
  (rotate! buffer)
  (vector-set! buffer (sub1 (vector-length buffer)) num))

(define (permutable-from-buffer? buffer num)
  (for*/or ([a buffer]
            [b buffer])
    (= (+ a b) num)))

(define (list->vector/sized lst size)
  (for/vector #:length size ([elm lst])
    elm))

(define (find-invalid-num entries premable-size)
  (let ([buffer (list->vector/sized entries premable-size)])
    (for/first ([n (drop input premable-size)]
                #:when (let ([valid (permutable-from-buffer? buffer n)])
                         (push-into-buffer! buffer n)
                         (not valid)))
      n)))

(define (part1) (find-invalid-num input 25))

(define (vector-set-last! vec n)
  (vector-set! vec (- (vector-length vec) 1) n))

;; NOTE: unused, written as a reference for the list version
(define (vector->prefix-sum vec)
  (define result (make-vector (add1 (vector-length vec))))
  (define acc 0)
  (for ([i (in-range 0 (vector-length vec))])
    (vector-set! result i acc)
    (set! acc (+ acc
                 (vector-ref vec i))))
  ;; Put in the last element (which is the sum of all elements in =vec=)
  (vector-set-last! result acc)
  result)

(define (list->prefix-sum lst)
  (define acc 0)
  (define result (for/vector #:length (+ (length lst) 1)
                             ([n lst])
                   (let ([res acc])
                     (set! acc (+ acc n))
                     res)))
  (vector-set-last! result acc)
  result)

(define (query-prefix-sum ps beg end)
  (- (vector-ref ps end)
     (vector-ref ps beg)))

(define (search-min-max lst)
  (if (null? (cdr lst))
      (let ([n (car lst)])
        (cons n n))
      (let ([n (car lst)]
            [subcase (search-min-max (cdr lst))])
        (cons (min n (car subcase))
              (max n (cdr subcase))))))

(define (for-range-pair callback arr-len)
  (let impl ([beg 0]
             [end arr-len])
    (callback beg end)
    (when (>= (- end beg) 2)
        ;; If the range length is >= 2...
        (begin
          ;; Process subsequences starting at the same place, with shorter lengths
          (for ([subseq-end (in-range end beg -1)])
            (callback beg subseq-end))
          ;; Process subsequences starting right after current =beg=
          (impl (+ beg 1) end)))))

(define (slice lst beg end)
  (take (drop lst beg) (- end beg)))

(define (part2)
  (let ([i.n. (find-invalid-num input 25)]
        [p.s. (list->prefix-sum input)])
    (for-range-pair (lambda (beg end)
                      (when (and (>= (- end beg) 2)
                                 (= (query-prefix-sum p.s. beg end) i.n.))
                            (let* ([c (search-min-max (slice input beg end))]
                                   [min (car c)]
                                   [max (cdr c)])
                              ;; NOTE: racket also supports the more traditiona ~% for line breaks
                              (printf "min: ~a, max: ~a, min+max: ~a\n" min max (+ min max)))))
                    (length input))))
