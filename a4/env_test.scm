(load "env1.scm")

(define test-env
    (extend-env 'a 1
        (extend-env 'b 2
            (extend-env 'c 3
                (extend-env 'b 4
                    (make-empty-env)))))
)


(apply-env test-env 'a)
(apply-env test-env 'b)
(apply-env test-env 'c)
(apply-env test-env 'd)