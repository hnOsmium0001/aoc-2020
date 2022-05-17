#lang racket

(require racket/file
         data/monad
         data/applicative
         megaparsack
         megaparsack/text)

(define input-content/p
  (do [cnt <- integer/p]
      space/p
      [subject-1st <- (many/p letter/p)]
      space/p
      [subject-2nd <- (many/p letter/p)]
      (or/p (try/p (string/p " bags"))
            (string/p " bag"))
      (pure (cons cnt
                  (string-append (list->string subject-1st) " " (list->string subject-2nd))))))

(define input-line/p
  (do [subject-1st <- (many/p letter/p)]
      space/p
      [subject-2nd <- (many/p letter/p)]
      (string/p " bags contain ")
      [contents <- (or/p (string/p "no other bags")
                         (many/p input-content/p
                                 #:sep (string/p ", ")))]
      (string/p ".")
      eof/p
      (pure (cons (string-append (list->string subject-1st) " " (list->string subject-2nd))
                  (if (equal? contents "no other bags")
                      null
                      contents)))))

(struct node ([parents #:mutable]
              [children #:mutable]))
(define (make-node-default) (node '() '()))

;; "cc" stands for "cons cell"
(define (make-input)
  (define nodes (make-hash))
  (for ([line (file->lines "inputs/day7.txt")])
  ;;(for ([line (file->lines "t.txt")])
    (let* ([rulecc (parse-result! (parse-string input-line/p line))]
           [rule-subject (car rulecc)]
           [rule-children (cdr rulecc)]
           [node (hash-ref! nodes rule-subject (thunk (make-node-default)))])
      (for ([childcc rule-children])
        (let* ([child-cnt (car childcc)]
               [child-subject (cdr childcc)]
               [child (hash-ref! nodes child-subject (thunk (make-node-default)))])
          ;; Prepend 'rule-subject' to the child's list of parents
          (set-node-parents! child (cons rule-subject
                                         (node-parents child)))))
      (set-node-children! node rule-children)))
  nodes)
(define input (make-input))

;; Part 1
;; traverse-parents doesn't visit repeated nodes
(define (traverse-parents-impl visited func node)
  (for ([parent-subject (node-parents node)])
    (if (not (hash-has-key? visited parent-subject))
        (let ([parent (hash-ref input parent-subject)])
          (hash-set! visited parent-subject #t)
          (func parent-subject parent)
          (traverse-parents-impl visited func parent))
        null)))
(define (traverse-parents func node)
  (define visited (make-hash))
  (traverse-parents-impl visited func node))

(define (part1)
  (define cnt 0)
  (traverse-parents (lambda (subject node)
                      ;;(printf "~a\n" subject)
                      (set! cnt (+ cnt 1)))
                    (hash-ref input "shiny gold"))
  cnt)
