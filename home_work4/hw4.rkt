
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;; sequence 
(define (sequence low hight strid)
  (if (> low hight)
      null
      (cons low (sequence (+ low strid) hight strid))))

;; string-append-map
(define (string-append-map xs suffix)
  (map (lambda (str)(string-append str suffix)) xs))

;; list-nth-mod 
(define (list-nth-mod xs num)
  (cond [(< num 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [(= num 0) (car xs)]
        [#t (list-ref xs (remainder num (length xs)))]))

;; stream example
(define s (lambda () (cons 1 s)))

;; stream-for-n-steps
(define (stream-for-n-steps s n)
  (define (helper stream count)    
    (let ([pr (stream)])
      (if (= count 0)
          null
          (cons (car pr) (helper (cdr pr) (- count 1))))))

  (helper s n))
          
;; funny-number-stream
(define funny-number-stream 
  (letrec ([f (lambda(x)
                (cons 
                (if (= (remainder x 5) 0)
                    (* x -1)
                    x) (lambda() (f (+ x 1)))))])
  (lambda () (f 1))))

;; dan-then-dog
(define dan-then-dog
  (letrec ([f (lambda(x)
                (cons x (lambda() (f (if (equal? x "dan.jpg") "dog.jpg" "dan.jpg") ))))]) 
  (lambda () (f "dan.jpg"))))

;; stream-add-zero
(define (stream-add-zero s)
  (letrec ([f (lambda(x)
                (cons (cons 0 (car (x)))
                 (lambda () (f (cdr (x))))))])
  (lambda () (f s))))

;; cycle-lists
;; the code is awfull but it works and does not use outer function
;; just for fun
#|
(define (cycle-lists xs ys)
  (letrec ([f (lambda(x y) 
                (cons 
                 (cons (car x) (car y))
                 (lambda() (f (if (= (length x) 1) xs (cdr x)) (if (= (length y) 1) ys (cdr y))))))])
    (lambda () (f xs ys))))
|#

;; cycle-lists
;; implementation using list-nth-mod
(define (cycle-lists xs ys)
  (letrec ([f (lambda(acc) 
                (cons 
                 (cons (list-nth-mod xs acc) (list-nth-mod ys acc))
                 (lambda() (f (+ acc 1)))))])
    (lambda () (f 0))))

;; vector-assoc
(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]           
           [f (lambda (index)
                (cond [(= index len) false]
                      [( not (pair? (vector-ref vec index))) (f (+ index 1))]
                      [#t (if (equal? (car(vector-ref vec index)) v)
                              (vector-ref vec index)
                              (f (+ index 1)))]))])
    (f 0)))

;; cached-assoc
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [index 0])
  (lambda (v)
    (let ([ans (vector-assoc v memo)])
      (if ans
          ans
          (let ([new-ans(assoc v xs)])
            (begin
              (vector-set! memo index new-ans)
              (set! index (if (= index (- n 1)) 0 (+ index 1))))
            new-ans))))))
              