#lang racket

(require racket/file)

;; Problem constants
(define input-file "inputs/day9.txt")
(define premable-size 25)

(define input (map string->number (file->lines input-file)))

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

(define (make-premable-buffer all)
  (for/vector #:length premable-size ([elm all])
    elm))

(define (find-invalid-num)
  (let* ([buffer (make-premable-buffer input)])
    (for/first ([n (drop input premable-size)]
                #:when (let ([valid (permutable-from-buffer? buffer n)])
                         (push-into-buffer! buffer n)
                         (not valid)))
      n)))

(define (part1) (find-invalid-num))

;; NOTE: unused, untested
(define (list/for-subsequence search-space callback)
  ;; This operation is O(n) to length of search-space
  (define search-space-len (length search-space))
  (let impl ([lst search-space]
             [len 1])
    ;; Send current subsequence to callback
    (callback lst len)
    ;; Process subsequences starting at the same place, with shorter lengths
    (let loop ([sublen (sub1 len)])
      (if (> len 0)
          (begin
            (impl lst sublen)
            (loop (sub1 sublen)))
          null))
    ;; Process subsequences starting after the current location
    (impl (cdr lst) (sub1 len))))

;; NOTE: unused, untested
(define (list/search-subsequence search-space predicate)
  (define result (cons '() 0))
  (list/for-subsequence search-space
                   (lambda (lst len)
                     (if (predicate lst len)
                         (set! result (cons lst len))
                         null)))
  result)

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
    (if (>= (- end beg) 2)
        ;; If the range length is >= 2...
        (begin
          ;; Process subsequences starting at the same place, with shorter lengths
          (let loop ([subseq-end (sub1 end)])
            (if (> (- subseq-end beg) 0)
                (begin
                  (callback beg subseq-end)
                  (loop (sub1 subseq-end)))
                null))
          ;; Process subsequences starting right after current =beg=
          (impl (+ beg 1) end))
        ;; Else, the range is too small to have any subsequences
        null)))

(define (slice lst beg end)
  (take (drop lst beg) (- end beg)))

(define (part2)
  (let ([i.n. (find-invalid-num)]
        [p.s. (list->prefix-sum input)])
    (for-range-pair (lambda (beg end)
                      (if (and (>= (- end beg) 2)
                               (= (query-prefix-sum p.s. beg end) i.n.))
                          (let* ([c (search-min-max (slice input beg end))]
                                 [min (car c)]
                                 [max (cdr c)])
                            ;; NOTE: racket also supports the more traditiona ~% for line breaks
                            (printf "min: ~a, max: ~a, min+max: ~a\n" min max (+ min max)))
                          null))
                    (length input))))
