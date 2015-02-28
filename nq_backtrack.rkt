#lang racket

(define (safe? test-row solutions)
  (not
   (or  (clash-row? test-row solutions)
        (clash-diag-right-up? test-row solutions)
        (clash-diag-right-down? test-row solutions))))

(define (place-queen test-row solutions N)
    (cond [(> test-row N) (backtrack solutions N)]
          [(safe? test-row solutions)(bump-counter) (get-solution (cons test-row solutions) N)]
          [else (bump-counter) (place-queen (+ test-row 1) solutions N)]))

(define (full-solution? solutions N)
  (= (length solutions) N))

(define get-solution
  (lambda (solutions N)
    (cond
      [(full-solution? solutions N) solutions]
      [else (place-queen 1 solutions N)])))

(define (clash-row? test-row solutions)
  (cond [(null? solutions) #f]
        [(= (car solutions) test-row) #t]
        [else
         (clash-row? test-row (cdr solutions))]))

(define (clash-diag-right-up? test-row solutions)
  (cond [(null? solutions) #f]
        [(= (car solutions) (- test-row 1)) #t]
        [else
         (clash-diag-right-up? (- test-row 1) (cdr solutions))]))

(define (clash-diag-right-down? test-row solutions)
  (cond [(null? solutions) #f]
        [(= (car solutions) (+ test-row 1)) #t]
        [else
         (clash-diag-right-down? (+ test-row 1) (cdr solutions))]))

(define (nqueen-backtrack N) 
  (get-solution '() N))

(define (nq-bt N) 
  [begin
    (set! counter 0)
    (define start (current-inexact-milliseconds))
    (define solution (nqueen-backtrack N))
    (define end (current-inexact-milliseconds))
    (define time-taken (/ (- end start) 1000))
    (matrix-format solution)
    (display "Size: ")
    (display N)
    (newline)
    (display "Time taken in seconds: ")
    (display time-taken)
    (newline)
    (display "No. of steps taken: ")
    (display counter)
    (newline)
    solution])

(define (backtrack solutions N)
  (if (null? solutions)
      '()
      (place-queen (+ (car solutions) 1) (cdr solutions) N)))

; format solutions
(define (matrix-format solutions) (format-helper solutions (length solutions) 1 1 solutions))

(define (format-helper solutions N row col partial_list)
  (cond [(> row N) (newline)]
        [(<= col N) (if (= (car partial_list) row)
                        (begin (display "1 ")
                               (format-helper solutions N row (add1 col) (cdr partial_list)))
                        (begin (display "0 ")
                               (format-helper solutions N row (add1 col) (cdr partial_list))))]
        [(null? partial_list) (newline) (format-helper solutions N (add1 row) 1 solutions)]
        [else (newline)]))

; Counter idea referenced from: http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-7.html#node_sec_5.1
(define counter 0)

(define bump-counter
  (lambda ()
    (set! counter (+ counter 1))
    counter))

(display (nq-bt 5))

