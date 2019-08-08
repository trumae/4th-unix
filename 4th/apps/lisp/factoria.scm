(define fac
  (lambda (n)
    (cond ((eq? n 1)
          1)
          (t
          (* n (fac (- n 1)))))))

(display (fac 5))
