#lang racket

(require racket/file
         data/monad
         data/maybe
         megaparsack
         megaparsack/text)

(define input
  ;; pp stands for passport
  (map (lambda (pp-text)
         (map (lambda (pp-field)
                (string-split pp-field ":"))
              ;; (string-split) uses whitespaces like " ", "\n" as limiters by default
              (string-split pp-text)))
       (string-split (file->string "inputs/day4.txt") "\n\n")))

;; Part 1

;; We assume that there is no duplicate fields
(define (passport-valid? passport)
  (cond
    [(eq? (length passport) 8) #t]
    [(eq? (length passport) 7)
     (not (for/first ([pp-field passport]
                      #:when (equal? (list-ref pp-field 0) "cid"))
            #t))]
    [else #f]))

(define (part1)
  (count passport-valid? input))
