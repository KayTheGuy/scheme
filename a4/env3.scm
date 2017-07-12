;; ========================================================================================================
;; Kayhan Dehghani Mohammadi
;; 301243781
;; kdehghan@sfu.ca
;; Assignment 4
;; ========================================================================================================

;; ''''''''''''''''''''''''''''''''''''''''''''''''''''
;; Data structure used is a list of two inner lists:
;; First inner list has the keys 
;; Second inner list has the values
;; The position of keys and their corresponding values
;; are the same
;; For example: env ---> ((b c d) (1 2 3))
;; equivalents JSON {"b": 1, "c":2, "d": 3}
;; ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

(define empty-env '(() ()))

;; ========================================================================================================
;; Returns a new empty environment
;; ========================================================================================================
(define (make-empty-env) empty-env) 
;; ========================================================================================================
;; Returns the value of variable v in environment env. If v is not in env, then raise an error message
;; ========================================================================================================
(define apply-env 
    (lambda (env v)
        (cond
            ((equal? env empty-env) 
                (error "apply-env: empty environment")
            )
            ((equal? 
                (car (first env)) v)            ;; key of the pair same as v ?
                (car (second env))              ;; return value of the pair
            )
            (else
                (apply-env                      ;; search recursively
                        (list 
                            (cdr (first env))   ;; rest of keys
                            (cdr (second env))  ;; rest of values
                        )
                        v
                )         
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
            ((equal? env empty-env)          ;; add first elements to inner lists
                (list 
                    (cons v (first env)) 
                    (cons val (second env)) 
                ) 
            )
            ((equal?                         ;; key of the pair same as v ?
                (first (first env)) v)  
                (list 
                    (first env)
                    (cons                    ;; update value in second inner list (shadow old value of pair)
                        val 
                        (cdr (second env))
                    )
                )  
            )
            (else                                ;; recursive call
                (let* ((rest-of-env 
                        (extend-env v val 
                            (list 
                                (cdr (first env))  ;; rest of keys
                                (cdr (second env)) ;; rest of values
                            )
                        )))                     ;; end of name-value of let
                    (list                       ;; put the first processed key/value back in their lists
                        (cons (car (first env)) (first rest-of-env ))
                        (cons (car (second env)) (second rest-of-env ))
                    )
                ) ;; end of let
            )
        )
    )
)