;; ========================================================================================================
;; Kayhan Dehghani Mohammadi
;; 301243781
;; kdehghan@sfu.ca
;; Assignment 4
;; ========================================================================================================

;; ''''''''''''''''''''''''''''''''''''''''''''''''''''
;; Data structure used is a list of pairs:
;; For example: env ---> ((b.1) (c.2) (d.3))
;; equivalents JSON {"b": 1, "c":2, "d": 3}
;; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

;; ========================================================================================================
;; Returns a new empty environment
;; ========================================================================================================
(define (make-empty-env) '()) 
;; ========================================================================================================
;; Returns the value of variable v in environment env. If v is not in env, then raise an error message
;; ========================================================================================================
(define apply-env 
    (lambda (env v)
        (cond
            ((null? env)
                (error "apply-env: empty environment")
            )
            ((equal?                        ;; key of the pair same as v ?
                (car (car env)) v)     
                (cdr (car env))             ;; return value of the pair
            )
            (else
                (apply-env (cdr env) v)     ;; search recursively
            )
        )
    )
)
;; ========================================================================================================
;; Returns a new environment that is the same as env except that the value of v in it is val.
;; If v already has a value in env, then in the newly returned environment this value will be shadowed
;; ========================================================================================================
(define extend-env
    (lambda (v val env)
        (cond
            ((null? env)
                (cons           ;; add pair of (v.val) to empty env
                    (cons v val) 
                    env
                ) 
            )
            ((equal?            ;; key of the pair same as v ?
                (car (car env)) v) 
                (cons           ;; add new pair to env (shadow old value of pair)
                    (cons v val) 
                    (cdr env)
                ) 
            )
            (else
                (cons           ;; recursive call
                    (car env) 
                    (extend-env v val (cdr env))
                ) 
            )
        )
    )
)