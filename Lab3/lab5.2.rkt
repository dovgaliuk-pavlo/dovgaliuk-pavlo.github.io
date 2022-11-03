(display "Довгалюк Павло ІПЗ-41. Лаб 3. Варіант 5. Частина 2.\n")


; Сімпсона  
(define (Sipmsons x0 x02h f)
    (let ((h (/ ( - x02h x0) 2)))
    (* (/ h 3) ( + (+ (f x0) (* 4 (f (+ x0 h)))) (f (+ x0 (* 2 h)))))))

(define Simp (Sipmsons 0 1 (lambda(x)
                           (sin (+ x (+ 1 (expt x 2)))))))
(display Simp)


;функція циклу
(define (loop min max func accur)
  (if (<= max min)
    (func min)
    (loop (+ min accur) max func accur)))

; Трапеції           
(define sum 0)              
(define (trap a b n func)
        (loop a b (lambda (x) (set! sum (+ sum (func x)))) (/ (- b a) n))
        (* (+ ( / (+ (func a) (func b)) 2) sum) (/ (- b a) n)))  

(newline)
(display "Формула трапецій (n=1000) = ")
(define Trap (trap 0 1 1000 (lambda(x)
                           (sin (+ x (+ 1 (expt x 2)))))))
(display Trap)
; difference
(newline)
(display "Різниця між методами = ")
(display (- Trap Simp))