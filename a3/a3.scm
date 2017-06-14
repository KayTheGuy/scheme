;;================================================================================
;;    Kayhan Dehghani Mohammadi
;;    301243781
;;    kdehghan@sfu.ca
;;    Scheme Problem Sets
;;================================================================================

;;================================================================================
;; Q1:
;;      Write a function called (my-last lst) that returns the last element of lst
;;      Calling my-last on an empty prints the error message “empty list”
;;================================================================================
(define my-last
    (lambda (lst)
        (cond 
            ((null? lst)  
                (error "empty list")) ;; report error 
            ((null? (cdr lst)) 
                (car lst))  ;; base case
            (else
                (my-last (cdr lst)) ;; recursive call
            )
        )
    )
)

;;================================================================================
;; Q2:
;;      Write a function called (snoc x lst) that returns a list that is the same 
;;      as lst except x has been added to the right end of the list
;;================================================================================
(define snoc
    (lambda (x lst)
        (cond
            ((null? lst)
                (cons x lst)) ;; base case
            ((null? (cdr lst))
                ;; append x before the only element of x
                (cons (car lst) (cons x (cdr lst)))) 
            (else 
                ;; recursive call
                (cons (car lst) (snoc (cdr lst))))
        )
    )
)

;;================================================================================
;; Q3:
;;      Write a function called (range n) that returns the list (0 1 2 ... n-1)
;;      Assume n is an integer, and if it is 0, or less, return the empty list
;;================================================================================
(define range
    (lambda (n)
        (cond 
            ((or (= n 0) (< n 0)) ;; base case
                '())
            (else
                (snoc (- n 1) (range (- n 1)))) ;; use snoc defined above as helper
        )
    )
)

;;================================================================================
;; Q4:
;;      Write the function called (deep-sum lst) that returns the sum of all the 
;;      numbers in lst, including numbers within lists. 
;;      Non-numbers should be ignored
;;================================================================================
(define deep-sum
    (lambda (lst)
        (cond 
            ((null? lst) ;; base case
                0)
            ((list? (car lst)) 
                (+ (deep-sum (car lst)) (deep-sum (cdr lst)))) ;; recursive deep sum
            ((number? (car lst))
                (+ (car lst) (deep-sum (cdr lst)))) ;; recursive shallow sum
            (else
                (deep-sum (cdr lst))) ;; recursive call
        )
    )
)

;;================================================================================
;; Q5:
;;      Write a function called (count-primes n) that returns the number of primes 
;;      less than, or equal to, n
;;================================================================================
(define count-primes
    (lambda (n)
        (cond
            ((< n 2)
                0) ;; base case
            (else
                (+ (if (is-prime? n) 1 0)
                    (count-primes (- n 1)))
                ) ;; recursive call
        )
    )
)

(define is-prime? 
    (lambda (n)
        (cond
            ((< n 2)
                #f)
            ((and 
                (not(= n 2))
                (zero? (remainder n 2))
                )
                    #f) ;; skip basic cases for efficiency
            ((and 
                (not(= n 3))
                (zero? (remainder n 3))
                )
                    #f) ;; skip basic cases for efficiency
            ((and 
                (not(= n 5))
                (zero? (remainder n 5))
                )
                    #f) ;; skip basic cases for efficiency
            (else
                (is-prime-helper? n 2))
        )
    )
)

(define is-prime-helper?
    (lambda (a b)
        (cond   
            ((< a (* b b))
                #t)
            ((zero? (remainder a b)) 
                #f) ;; a is not prime
            (else
                (is-prime-helper? a (+ b 1))) ;; loop from 2 up to b^2
        )
    )
)

;;================================================================================
;; Q6:
;;      Write a function called (is-bit? x) that returns #t when x is the number 
;;      0 or 1, and #f otherwise.
;;================================================================================
(define is-bit?
    (lambda (x)
        (cond 
            ((and 
                  (integer? x) 
                  (or (= x 0) (= x 1))
                )
                #t)
            (else
                #f)
        )
    )
)

;;================================================================================
;; Q7:
;;      Write a function called (is-bit-seq? lst) that returns true if lst is the 
;;      empty list, or if it contains only bits (as defined by is-bit?).
;;================================================================================
(define is-bit-seq?
    (lambda (lst)
        (cond
            ((null? lst) ;; base case
                #t)
            (else
                (and 
                    (is-bit? (car lst))
                    (is-bit-seq? (cdr lst)) ;; recursive call
                )
            )
        )
    )
)