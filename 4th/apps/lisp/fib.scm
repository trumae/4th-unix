(define fib
  (lambda (n)
      (cond ((eq? n 1) 1)
            ((eq? n 0) 0)
            (t (+ (fib (- n 2))
               (fib (- n 1)))))))

(display (fib 10))
