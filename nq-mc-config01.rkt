#lang racket
(require data/heap)

(define maxsteps 30000)

(define (get-solution solutions N) 
  (cond [(equal? #() solutions)  (move-queen N (build-start-vector N) N)]
        [(full-solution? N solutions N) solutions]
        [(= counter maxsteps) (display "Either no solution possible or exceeded maxsteps.")]
        [else (move-queen N solutions N)])) 

(define (build-start-vector N)
  (do ((vec (make-vector (+ N 1)))
       (i 0 (+ i 1)))
    ((> i N) vec)
    (vector-set! vec i i)))

(define (full-solution? col solutions N)
  (or (= col 0)
      (and (= 0 (conflict-sum (vector-ref solutions col) col solutions N))
           (full-solution? (- col 1) solutions N))))

(define (move-queen col solutions N)
  (let [(row (vector-ref solutions col))]
    (cond [(= 0 col) (get-solution solutions N)]
          [(= 0 (conflict-sum row col solutions N)) (move-queen (- col 1) solutions N)]
          [else (let [(binheap (make-heap node<=?))]
                  (do [(i N (- i 1))]
                    [(< i 1) (let [(node1 (delete-min! binheap))
                                   (node2 (delete-min! binheap))]
                               (if [and (equal? (node-tryrow node1) row) (equal? (node-conflict node1) (node-conflict node1))]
                                   [begin (vector-set! solutions col (node-tryrow node2)) 
                                          (bump-counter)
                                          (move-queen (- col 1) solutions N)]
                                   [begin (vector-set! solutions col (node-tryrow node1)) 
                                          (bump-counter)
                                          (move-queen (- col 1) solutions N)]
                                   ))]
                    (add! binheap i (conflict-sum i col solutions N))))])))

#|
;To see text printed
(define (move-queen col solutions N)
  (let [(row (vector-ref solutions col))]
    (cond [(= 0 col) (get-solution solutions N)]
          [(= 0 (conflict-sum row col solutions N)) (move-queen (- col 1) solutions N)]
          [else (let [(binheap (make-heap node<=?))]
                  (do [(i N (- i 1))]
                    [(< i 1) (let [(node1 (delete-min! binheap))
                                   (node2 (delete-min! binheap))]
                               (if [and (equal? (node-tryrow node1) row) (equal? (node-conflict node1) (node-conflict node1))]
                                   [begin (vector-set! solutions col (node-tryrow node2)) 
                                    (display "minheap is row ")
                                    (display (node-tryrow node2))
                                    (display ". Vector is: ")
                                    (display solutions) 
                                    (newline)
                                    (bump-counter)
                                    (move-queen (- col 1) solutions N)]
                                   [begin (vector-set! solutions col (node-tryrow node1)) 
                                    (display "minheap is row ")
                                    (display (node-tryrow node1))
                                    (display ". Vector is: ")
                                    (display solutions) 
                                    (newline)
                                    (bump-counter)
                                    (move-queen (- col 1) solutions N)]
                                   ))]
                    (add! binheap i (conflict-sum i col solutions N))
                    (display i)
                    (display ": ")
                    (display (conflict-sum i col solutions N))
                    (display "...")))])))
|#

(define (conflict-sum test-row test-col solutions N)
  (do [(sol-col 1 (add1 sol-col))
       (sum 0 (if (check-all-conflict test-row test-col (vector-ref solutions sol-col) sol-col) 
                  (+ sum 1) 
                  sum))]
    [(> sol-col N)
     sum]))

;alternative 
(define (frequency? test-num solutions)
  (for/sum ([value (in-vector solutions)])
    (if (equal? value test-num) 1 0)))


;(length (filter (lambda (x) (equal? x test-num)) solutions))

(define (check-all-conflict test-row test-col sol-row sol-col)
  (and (not (equal? test-col sol-col))
       (not (equal? 0 sol-row))
       (or (clashrow? test-row test-col sol-row sol-col)
           (clashdiag1? test-row test-col sol-row sol-col)
           (clashdiag2? test-row test-col sol-row sol-col))))

(define (clashrow? test-row test-col sol-row sol-col)
  (= test-row sol-row))

(define (clashdiag1? test-row test-col sol-row sol-col)
  (= sol-row (+ test-row (- sol-col test-col))))

(define (clashdiag2? test-row test-col sol-row sol-col)
  (= sol-row (- test-row (- sol-col test-col))))

; Counter idea is referenced from: 
; http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-7.html#node_sec_5.1
(define counter 0)

(define bump-counter
  (lambda ()
    (set! counter (+ counter 1))
    counter))


; Slower method to check if queen position is safe.
(define (clash-row? test-row solutions N)
  (do ([count  1 (+ count 1)]
       [result 0 (if (equal? (vector-ref solutions count) test-row)
                     (+ 1 result)
                     result)])
    [(> count N) 
     result]))

(define (clash-diag-right-up? test-row test-col solutions N)
  (do ([count  (+ test-col 1) (+ count 1)]
       [index 1 (+ index 1)]
       [result 0 (if (equal? (vector-ref solutions count) (- test-row index))
                     (+ 1 result)
                     result)])
    [(> count N) 
     result]))

(define (clash-diag-right-down? test-row test-col solutions N)
  (do ([count  (+ test-col 1) (+ count 1)]
       [index 1 (+ index 1)]
       [result 0 (if (equal? (vector-ref solutions count) (+ test-row index))
                     (+ 1 result)
                     result)])
    [(> count N)
     result]))

(define (clash-diag-left-up? test-row test-col solutions N)
  (do ([count  (- test-col 1) (- count 1)]
       [index 1 (+ index 1)]
       [result 0 (if (equal? (vector-ref solutions count) (- test-row index))
                     (+ 1 result)
                     result)])
    [(< count 1)
     result]))

(define (clash-diag-left-down? test-row test-col solutions N)
  (do ([count  (- test-col 1) (- count 1)]
       [index 1 (+ index 1)]
       [result 0 (if (equal? (vector-ref solutions count) (+ test-row index))
                     (+ 1 result)
                     result)])
    [(< count 1)
     result]))
; end of slower method

; Creating min-heaps
(struct node (tryrow conflict))
(define (node<=? x y)
  (<= (node-conflict x) (node-conflict y)))

(define (add! heap try-row conflict) 
  (heap-add! heap (node try-row conflict)))

(define (delete-min! heap)
  (begin0 
    (let [(min (heap-min heap))]
      (heap-remove-min! heap)
      min)))

; How to use heap
(define heap2 (make-heap node<=?))
(add! heap2 1 3)
(add! heap2  2 2)
(add! heap2  3 9)
(add! heap2  4 6)

; format solutions
(define (matrix-format solutions) (format-helper solutions (- (vector-length solutions) 1) 1 1))

(define (format-helper solutions N row col)
  (cond [(> row N) (newline)]
        [(<= col N) (if (= (vector-ref solutions col) row)
                        (begin (display "1 ")
                               (format-helper solutions N row (add1 col)))
                        (begin (display "0 ")
                               (format-helper solutions N row (add1 col))))]
        [(> col N) (newline) (format-helper solutions N (add1 row) 1)]
        [else (newline)]))

(define (nqueen-minconflict N) 
  (get-solution #() N))

(define (nq-mc N) 
  [begin
    (set! counter 0)
    (define start (current-inexact-milliseconds))
    (define solution (nqueen-minconflict N))
    (define end (current-inexact-milliseconds))
    (define time-taken (/ (- end start) 1000))
    (matrix-format solution)
    (display "Size: ")
    (display N)
    (newline)
    (display "Min-conflict: Columns are selected from N to 1.")
    (newline)
    (display "Time taken in seconds: ")
    (display time-taken)
    (newline)
    (display "No. of steps taken: ")
    (display counter)
    (newline)
    (display "*******Ignore the index 0 item in vector***********")
    (newline)
    solution])

(display (nq-mc 20))










