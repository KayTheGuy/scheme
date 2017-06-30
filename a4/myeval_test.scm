(load "myeval.scm")

(define env1
    (extend-env 'x -1
        (extend-env 'y 4
            (extend-env 'x 1
                (make-empty-env))))
)

(define env2
    (extend-env 'm -1
        (extend-env 'a 4
            (make-empty-env)))
)

(define env3
    (extend-env 'q -1
        (extend-env 'r 4
            (make-empty-env))))

(myeval '(2 + (3 * x))     ;; the expression
           env1               ;; the environment
   )
(myeval '(2 + (3 * 1))     ;; the expression
           env1               ;; the environment
   )
(myeval '((m * a) - 0.1)   ;; the expression
           env2               ;; the environment
   )

(myeval '(2)   ;; the expression
           env2               ;; the environment
   )
(myeval '(m)   ;; the expression
           env2               ;; the environment
   )
(myeval '(4 * (s * s))     ;; the expression
           env3  )             ;; the environment

(myeval '((2 ** (r ** (1 ** 0))) + (inc 9)) env3)
(myeval '(((r + r) - (r + r)) * (1 * (1 + 0))) env3)
(myeval '((1 * (r + 0)) + 0) env3)
(myeval '((1 * r) + (r * 0)) env3)
(myeval '(r ** (2 * (dec 1))) env3)