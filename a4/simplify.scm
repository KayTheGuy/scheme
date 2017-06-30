;; ========================================================================================================
;; Kayhan Dehghani Mohammadi
;; 301243781
;; kdehghan@sfu.ca
;; Assignment 4
;; ========================================================================================================

;; ========================================================================================================
;; Main function: 
;; ========================================================================================================
(define simplify
    (lambda (expr)
        (cond
            ((or                                ;; base cases for recursion
                (number? expr) 
                (symbol? expr))   
                expr
            )
            ((is-one-element? expr)            ;; expression is a list of one number or one symbol
                expr
            )
            ((or                               ;; (e + 0) or (e * 1) or (e - 0) or (e ** 1)
                (is-exp-plus-zero? expr)
                (is-exp-times-one? expr)
                (is-exp-minus-zero? expr)
                (is-power-one? expr))

                (simplify (first expr))        ;; return e (first element)
            )
            ((or                               ;; (0 + e) or (1 * e) 
                (is-zero-plus-exp? expr)
                (is-one-times-exp? expr))

                (simplify (third expr))        ;; return e (third element)
            )
            ((or                               ;; (e * 0) or (0 * e) or (e - e)
                (is-times-zero? expr)
                (is-exp-minus-itself? expr))          
                0
            )
            ((or                               ;; (e ** 0) or (1 ** e)
                (is-power-zero? expr)
                (is-one-power-expr? expr))
                1
            )
            ((is-inc-num? expr)               ;; (inc 8)
                (+ 1 (second expr))
            )
            ((is-dec-num? expr)               ;; (dec 9)
                (- (second expr) 1)
            )
            ((is-add? expr)                    ;; (e1 + e2) and nonzero expression
                (simplify-further expr '+)
            )
            ((is-sub? expr)                     ;; (e1 - e2) and not (e - 0) or (e - e)
                (simplify-further expr '-)
            )
            ((is-mul? expr)                    ;; (e1 * e2) and not ((0 * e), (e * 0), 
                (simplify-further expr '*)                         ;; (1 * e), (e * 1)
            )
            ((is-div? expr)                    ;; (e1 / e2) 
                (simplify-further expr '/) 
            )
            ((is-power? expr)                    ;; (e1 ** e2) and not (e ** 0),(e ** 1),or (1 ** e) 
                (simplify-further expr '**) 
            )
            ((is-inc? expr)                    ;; (inc expression) and not (inc number)
                (simplify-further-inc-dec expr 'inc)
            )
            ((is-dec? expr)                    ;; (dec expression) and not (dec number)
                (simplify-further-inc-dec expr 'dec)
            )
            (else 
                (error "simplify: invalid expression"))
        )
    )
)
;; ========================================================================================================
;; Helper: simplifies further if the subexpression change after simplification (for arithmetic operators)
;; ========================================================================================================
(define simplify-further
    (lambda (expr op)           ;; op is '+, or '-, ...
        (let* ((first-expr (first expr))
                (third-expr (third expr))
                (first-expr-simplified (simplify first-expr))
                (third-expr-simplified (simplify third-expr))
                )
            (cond   ;; check if sun-expressions changed after simplification
                ((and 
                    (equal? first-expr first-expr-simplified)
                    (equal? third-expr third-expr-simplified)
                    )
                    (list           
                        first-expr-simplified
                        op
                        third-expr-simplified)
                )
                (else    ;; simplify further if sub-expressoins changed after simplification
                    (simplify (list                
                        first-expr-simplified
                        op
                        third-expr-simplified))
                )
            )
        )
    )
) 
;; ========================================================================================================
;; Helper: simplifies further if the subexpression change after simplification (for inc/dec operators)
;; ========================================================================================================
(define simplify-further-inc-dec
    (lambda (expr op)           ;; op is dec or inc
        (let* ((second-expr (second expr))
                (second-expr-simplified (simplify second-expr))
                )
            (if   ;; check if sun-expressions changed after simplification
                (equal? second-expr second-expr-simplified)
                (list           
                    op (simplify second-expr))
                (simplify 
                    (list           
                    op (simplify second-expr)))
            )
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is one number or one symbole: (2) or (a)
;; ========================================================================================================
(define is-one-element?
    (lambda (e)
        (and 
            (nlist? 1 e)
            (or 
                (number? (car e))
                (symbol? (car e))
            )
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (e + 0)
;; ========================================================================================================
(define is-exp-plus-zero?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (equal? '+ (second e))
            (equal? 0 (third e))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (0 + e)
;; ========================================================================================================
(define is-zero-plus-exp?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (equal? 0 (first e))
            (equal? '+ (second e))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (e * 1)
;; ========================================================================================================
(define is-exp-times-one?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (equal? '* (second e))
            (equal? 1 (third e))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (1 * e)
;; ========================================================================================================
(define is-one-times-exp?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (equal? 1 (first e))
            (equal? '* (second e))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (0 * e) or (e * 0)
;; ========================================================================================================
(define is-times-zero?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (equal? '* (second e))
            (or
                (equal? 0 (first e))
                (equal? 0 (third e))
            )
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (e - 0)
;; ========================================================================================================
(define is-exp-minus-zero?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (equal? '- (second e))
            (equal? 0 (third e))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (e - e)
;; ========================================================================================================
(define is-exp-minus-itself?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (equal? '- (second e))
            (equal? (first e) (third e))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (e ** 0)
;; ========================================================================================================
(define is-power-zero?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (equal? '** (second e))
            (equal? 0 (third e))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (e ** 1)
;; ========================================================================================================
(define is-power-one?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (equal? '** (second e))
            (equal? 1 (third e))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (1 ** e)
;; ========================================================================================================
(define is-one-power-expr?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (equal? 1 (first e))
            (equal? '** (second e))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (e1 + e2) and none of e1 and e2 are zero
;; ========================================================================================================
(define is-add?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (not (equal? 0 (first e)))
            (equal? '+ (second e))
            (not (equal? 0 (third e)))
        )
    )
)
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (e1 - e2), not including (e - 0) or (e - e)
;; ========================================================================================================
(define is-sub?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (equal? '- (second e))
            (not (equal? 0 (third e)))
            (not (equal? (first e) (third e)))
        )
    )
)
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (e1 * e2) and
;; not including (0 * e), (e * 0), (1 * e), (e * 1)
;; ========================================================================================================
(define is-mul?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (not (equal? 0 (first e)))
            (not (equal? 1 (first e)))
            (equal? '* (second e))
            (not (equal? 0 (third e)))
            (not (equal? 1 (third e)))
        )
    )
)
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (e1 / e2) 
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
;; Helper: returns true if the expression is of the form: (e1 ** e2) and not (e ** 0), (e ** 1),or (1 ** e)
;; ========================================================================================================
(define is-power?
    (lambda (e)
        (and 
            (nlist? 3 e)
            (not (equal? 1 (first e)))
            (equal? '** (second e))
            (not (equal? 0 (third e)))
            (not (equal? 1 (third e)))
        )
    )
)
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (inc 8)
;; ========================================================================================================
(define is-inc-num?
    (lambda (e)
        (and 
            (nlist? 2 e)
            (equal? 'inc (first e))
            (number? (second e))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (dec 5)
;; ========================================================================================================
(define is-dec-num?
    (lambda (e)
        (and 
            (nlist? 2 e)
            (equal? 'dec (first e))
            (number? (second e))
        )
    )
)  
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (inc e) and e is not a number
;; ========================================================================================================
(define is-inc?
    (lambda (e)
        (and 
            (nlist? 2 e)
            (equal? 'inc (first e))
            (not (number? (second e)))
        )
    )
) 
;; ========================================================================================================
;; Helper: returns true if the expression is of the form: (dec e) and e is not a number
;; ========================================================================================================
(define is-dec?
    (lambda (e)
        (and 
            (nlist? 2 e)
            (equal? 'dec (first e))
            (not (number? (second e)))
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