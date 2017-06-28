;; ========================================================================================================
;; Kayhan Dehghani Mohammadi
;; 301243781
;; kdehghan@sfu.ca
;; Assignment 4
;; ========================================================================================================

(load "env1.scm")

;; ========================================================================================================
;; Helper: returns true if the argument follows the following grammar:
;; expr =  "(" expr "+" expr ")"
;;         | "(" expr "-" expr ")"
;;         | "(" expr "*" expr ")"
;;         | "(" expr "/" expr ")"
;;         | "(" expr "**" expr ")"  ;; e.g. (2 ** 3) is 8, (3 ** 3) is 27
;;         | "(" "inc" expr ")"      ;; adds 1 to expr
;;         | "(" "dec" expr ")"      ;; subtracts 1 from expr
;;         | var
;;         | number
;; number = a Scheme number
;; var    = a Scheme symbol
;; ========================================================================================================
(define is-expr?
    (lambda (e)
        (cond
            ((or    (number? e)    ;; return true if e is number or symbol
                    (symbol? e))
                #t
            )
            ((is-add? e)        ;; (e1 + e2)
                (and
                    (is-expr? (first e))
                    (is-expr? (third e))
                )
            )
            ((is-sub? e)        ;; (e1 - e2)
                (and
                    (is-expr? (first e))
                    (is-expr? (third e))
                )
            )
            ((is-mul? e)        ;; (e1 * e2)
                (and
                    (is-expr? (first e))
                    (is-expr? (third e))
                )
            )
            ((is-div? e)        ;; (e1 / e2)
                (and
                    (is-expr? (first e))
                    (is-expr? (third e))
                )
            )
            ((is-pow? e)        ;; (e1 ** e2)
                (and
                    (is-expr? (first e))
                    (is-expr? (third e))
                )
            )
            ((is-inc? e)        ;; (inc e2)
                (is-expr? (second e))
            )
            ((is-dec? e)        ;; (dec e2)
                (is-expr? (second e))
            )
            (else
                #f)
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: "(" "inc" expr ")"         example: (inc e)
;; ========================================================================================================
(define is-inc?
    (lambda (e)
        (and 
            (nlist? 2 e)
            (equal? 'inc (car e))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: "(" "dec" expr ")"         example: (dec e)
;; ========================================================================================================
(define is-dec?
    (lambda (e)
        (and 
            (nlist? 2 e)
            (equal? 'dec (car e))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: "(" expr "+" expr ")"        example: (e1 + e2)
;; ========================================================================================================
(define is-add?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (equal? '+ (second e))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: "(" expr "-" expr ")"        example: (e1 - e2)
;; ========================================================================================================
(define is-sub?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (equal? '- (second e))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: "(" expr "*" expr ")"         example: (e1 * e2)
;; ========================================================================================================
(define is-mul?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (equal? '* (second e))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: "(" expr "/" expr ")"         example: (e1 / e2)
;; ========================================================================================================
(define is-div?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (equal? '/ (second e))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: "(" expr "**" expr ")"        example: (e1 ** e2)
;; ========================================================================================================
(define is-pow?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (equal? '** (second e))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if lst is a list with size n, else false
;; ========================================================================================================
(define nlist?
    (lambda (n lst)
        (and 
            (list? lst)
            (= n (length lst))
        )
    )
)
