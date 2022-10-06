(display "Довгалюк Павло ІПЗ-41. Лаб 1. Варіант 5. Частина 1.\n")

(display "m: ")
(define m (read))
(display "n: ")
(define n (read))

(define (mod a b)
  (let ((res (remainder a b)))
    (if (< b 0)
        (if (<= res 0)
            res
            (+ res b))
        (if (>= res 0)
            res
            (+ res b))
     )
   )
  )

(define (_GCD a b depth)
  (cond ((= 0 (remainder b a)) (display a))
        ((= 0 (remainder a b)) (display b))
        ((>= b a) (_GCD a (mod b a) (+ 1 depth)))
        ((>= a b) (_GCD (mod a b) b (+ 1 depth)))
   )
  (display "\nDepth: ")
  (display depth)
 )


(display "\nMy GSD realisation: ")
(_GCD m n 1)

(display "\nInbuild GSD realisation: ")
(GCD m n)
