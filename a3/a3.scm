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
;;      Write the function called (deep-sum lst) that returns the sum of all the 
;;      numbers in lst, including numbers within lists. 
;;      Non-numbers should be ignored
;;================================================================================