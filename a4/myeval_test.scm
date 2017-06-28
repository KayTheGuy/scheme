(load "myeval.scm")

(eval-expr '(1 + (2 + 3)))
(eval-expr '(1 / (2 + (3 + (10 * 10)))))
(eval-expr '(0 / (2 + (3 + (10 * 10)))))
(eval-expr '(8 / (dec (inc (0 + (100 - (10 * 10)))))))
(eval-expr '(8 / (2 + (dec (inc (0 + (100 - (10 * 10))))))))
(power 5 3)
(power 5 -3)
