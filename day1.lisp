;; Playground
(defun read-file-demo ()
  (let ((in (open "inputs/day1.txt")))
    (loop for line = (read-line in nil)
          while line do (format t "~a~%" line))
    (close in)))

;; Actual day 1 problem

;; https://stackoverflow.com/a/3814098
(defparameter *input*
  (with-open-file (file "inputs/day1.txt")
    (loop for line = (read-line file nil)
          while line
          collect (parse-integer line))))

(defun part1 ()
  (loop for a in *input*
        append (loop for b in *input*
                     when (= (+ a b) 2020)
                     return (format t "~a * ~a = ~a~%" a b (* a b)))))

(defun part2 ()
  (loop for a in *input*
        append (loop for b in *input*
                     append (loop for c in *input*
                                  when (= (+ a b c) 2020)
                                  return (format t "~a * ~a * ~a = ~a~%" a b c (* a b c))))))
