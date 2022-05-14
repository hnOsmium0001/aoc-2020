#lang racket

(require racket/file
         data/monad
         data/applicative
         megaparsack
         megaparsack/text)

(define input (file->lines "inputs/day2.txt"))

;; Part 1

;; Define a struct just to see how to works
;; We can technically just use a list and match the same way in usages
(struct password-entry (num1 num2 char letters))

(define password-entry/p
  (do [num1 <- integer/p]
      (char/p #\-)
      [num2 <- integer/p]
      space/p
      [char <- letter/p]
      (char/p #\:)
      space/p
      [letters <- (many/p letter/p)]
      (pure (password-entry num1 num2 char letters))))

#|
(define (password-valid-p1? password)
  (match password
    [(password-entry min-count max-count char letters)
     (let ([occurences (count (lambda (c) (eq? c char)) letters)])
       (<= min-count occurences max-count))]))
|#
(define (password-valid-p1? password)
  (let ([min-count (password-entry-num1 password)]
        [max-count (password-entry-num2 password)]
        [char (password-entry-char password)]
        [letters (password-entry-letters password)])
    (<= min-count (count (lambda (c) (eq? c char)) letters) max-count)))

(define (part1)
  (count (lambda (line) (password-valid-p1? (parse-result! (parse-string password-entry/p line)))) input))

;; Part 2
(define (password-valid-p2? password)
  (let ([1st (password-entry-num1 password)]
        [2nd (password-entry-num2 password)]
        [char (password-entry-char password)]
        [letters (password-entry-letters password)])
    (xor (eq? (list-ref letters (- 1st 1)) char)
         (eq? (list-ref letters (- 2nd 1)) char))))

(define (part2)
  (count (lambda (line) (password-valid-p2? (parse-result! (parse-string password-entry/p line)))) input))
