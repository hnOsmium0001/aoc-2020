#lang racket

(require racket/file
         data/monad
         data/applicative
         data/either
         megaparsack
         megaparsack/text)

;; Birth Year
(define passport-byr/p
  (do (byr <- (guard/p integer/p (lambda (n) (<= 1920 n 2002))
                       "Birth year not in range of [1920, 2002]"))
      eof/p
      (pure byr)))

;; Issue Year
(define passport-iyr/p
  (do (isr <- (guard/p integer/p (lambda (n) (<= 2010 n 2020))
                       "Issue year not in range of [2010, 2020]"))
      eof/p
      (pure isr)))

;; Expiration Year
(define passport-eyr/p
  (do (eyr <- (guard/p integer/p (lambda (n) (<= 2020 n 2030))
                       "Expiration year not in range of [2020, 2030]"))
      eof/p
      (pure eyr)))

;; Height
(define passport-hgt/p
  (do (scalar <- integer/p)
      ;; NOTE: megaparsack has a 1 char lookahead, and since 'c' and 'i' are different there is no need for backtracking here
      (dim <- (guard/p (or/p (string/p "cm")
                             (string/p "in"))
                       (lambda (dim)
                         (match dim
                           ["cm" (<= 150 scalar 193)]
                           ["in" (<= 59 scalar 76)]))
                       "Height is invalid"))
      eof/p
      (pure (cons scalar dim))))

;; Hair color
(define passport-hcl/p
  (do (char/p #\#)
      ;; 6 hexidecimal digits
      (repeat/p 6 (or/p digit/p
                        (char-between/p #\a #\f)))
      eof/p))

;; Eye Color
(define passport-ecl/p
  #| NOTE: simply doing this will not work as multiple options have the same initial char, so we need backtracking
  (or/p (string/p "amb")
        (string/p "blu")
        (string/p "brn")
        (string/p "gry")
        (string/p "grn")
        (string/p "hzl")
        (string/p "oth"))
  |#
  (do (or/p (try/p (string/p "amb"))
            (try/p (string/p "blu"))
            (try/p (string/p "brn"))
            (try/p (string/p "gry"))
            (try/p (string/p "grn"))
            (try/p (string/p "hzl"))
            (string/p "oth"))
      eof/p))

;; Passport ID
(define passport-pid/p
  (do (repeat/p 9 digit/p)
      ;; This EOF check is especially important:
      ;; Ensure the pid is exactly 9 digits long (otherwise it will match 10 or more digits)
      eof/p))

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
(define (passport-has-necessary-fields? passport)
  (cond
    [(eq? (length passport) 8) #t]
    [(eq? (length passport) 7)
     (not (for/first ([pp-field passport]
                      #:when (equal? (list-ref pp-field 0) "cid"))
            #t))]
    [else #f]))

(define (part1)
  (count passport-has-necessary-fields? input))

;; Part 2
(define (passport-field-valid? pp-field)
  ;; ppf stands for passport field
  (let* ([ppf-type (list-ref pp-field 0)]
         [ppf-value-text (list-ref pp-field 1)]
         [ppf-value-pres (match ppf-type
                           ["byr" (parse-string passport-byr/p ppf-value-text)]
                           ["iyr" (parse-string passport-iyr/p ppf-value-text)]
                           ["eyr" (parse-string passport-eyr/p ppf-value-text)]
                           ["hgt" (parse-string passport-hgt/p ppf-value-text)]
                           ["hcl" (parse-string passport-hcl/p ppf-value-text)]
                           ["ecl" (parse-string passport-ecl/p ppf-value-text)]
                           ["pid" (parse-string passport-pid/p ppf-value-text)]
                           ["cid" (success '())])])
    (success? ppf-value-pres)))

(define (passport-fields-valid? pp)
  (andmap passport-field-valid? pp))

(define (part2)
  (count (lambda (inp)
           (and (passport-has-necessary-fields? inp)
                (passport-fields-valid? inp)))
         input))
