(display "Довгалюк Павло ІПЗ-41. Лаб 4. Варіант 5. Частина 2.\n")


; Сімпсона  
(define (Sipmsons x0 x02h f)
    (let ((h (/ ( - x02h x0) 2)))
    (* (/ h 3) ( + (+ (f x0) (* 4 (f (+ x0 h)))) (f (+ x0 (* 2 h)))))))

(define Simp (Sipmsons 0 1 (lambda(x)
                           (sin (+ x (+ 1 (expt x 2)))))))
(display Simp)



; Трапеції
(define (set_h a b n) (/ (- b a) n))
(define (set_S a b func) (+ (func a) (func b)))


(define eps 0.001)

(define (loop h S func i n)
  (if (= i (- n 1))
      (* S (/ h 2))
      (loop h (+ S (* 2 (func (* h i)))) func (+ i 1) n)))

(define (trap a b n S1 func)
  (let ((S (loop (set_h a b n) (set_S a b func) func 1 n)))
    (if (<= (abs (- S S1)) eps)
        (trap a b (* n 2) S func)
        S
     )
  )
)

(newline)
(display "Формула трапецій (n=1000) = ")
(define Trap (trap 0 1 1000 0 (lambda(x)
                           (sin (+ x (+ 1 (expt x 2)))))))
(display Trap)
; difference
(newline)
(display "Різниця між методами = ")
(display (- Trap Simp))