(load "simplify.scm")

(simplify '(10))
(simplify '(a))
(simplify '(a + 0))
(simplify '(0 + a))
(simplify '(0 * a))
(simplify '((a * 1) + 1))
(simplify '(0 * (a + 0)))
(simplify '((a + 0) * 0))
(simplify '((1 * (a + 0)) - 0))
(simplify '((1 * (a - 0)) - 0))
(simplify '(((1 * (a - 0)) - 0) - ((1 * (a - 0)) - 0)))
(simplify '(a - a))
(simplify '((1 - 1) + (1 * 0)))
(simplify '((a - a) + (1 ** (dec 8))))
(simplify '(inc 8))
(simplify '((1 * (a + 0)) + 0))
(simplify '(((a + b) - (a + b)) * (1 * (1 + 0))))
(simplify '((1 * a) + (b * 0)))
(simplify '(z ** (b * (dec 1))))
(simplify '((1 * a) + (b * 1)))
(simplify '(((a - b) + (a - b)) * (1 * (1 - 0))))
(simplify '((dec (1 + 4)) + (inc (0 + 4))))
(simplify '(dec (inc (dec (1 ** 9)))))
(simplify '(dec (inc (dec (a ** (dec 8))))))