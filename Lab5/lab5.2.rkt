(display "Довгалюк Павло ІПЗ-41. Лаб 5. Варіант 5. Частина 2.\n")

;==========селектори дійсної та уявної частин компл числа=====
(define (Myreal-part z) (car z))
(define (Myimag-part z) (cdr z))
;============ тригонометрична форма компл числа======== ==
(define (square x)
(* x x))
(define (magnitude1 z)
(sqrt (+ (square (Myreal-part z)) (square (Myimag-part z)))))  
(define (angle1 z)
(atan (Myimag-part z) (Myreal-part z)))
(define (make-from-real-imag x y)
(cons x y))
(define (make-from-mag-ang r a)
(cons (* r (cos a)) (* r (sin a))))

;======= додавання та віднімання компл чисел в алгебраїчній формі======
(define (add-complex z1 z2)
 (make-from-real-imag (+ (Myreal-part z1) (Myreal-part z2))
    (+ (Myimag-part z1) (Myimag-part z2))))

(define (sub-complex z1 z2)
    (make-from-real-imag (- (Myreal-part z1) (Myreal-part z2))
       (- (Myimag-part z1) (Myimag-part z2))))

(define complex1 (make-from-real-imag 2 3))      ;створення комплексного числа1
(define complex2 (make-from-real-imag -5 1))
;=====множення та ділення компл чисел в тригонометричній формі======
(define (mul-complex z1 z2)
        (make-from-mag-ang (* (magnitude1 z1) (magnitude1 z2))
                           (+ (angle1 z1) (angle1 z2))))

(define (div-complex z1 z2)
        (make-from-mag-ang (/ (magnitude1 z1) (magnitude1 z2))
                           (- (angle1 z1) (angle1 z2))))

;=====список комплексних чисел======
(define complex-list (list (make-from-real-imag 1 5)
                           (make-from-real-imag 4 2)
                           (make-from-real-imag 6 9)
                           (make-from-real-imag 7 3)))

;=====добуток комплексних чисел======
(define (complex-list-sum complex-list)
  (if (null? complex-list)
      (make-from-real-imag 0 0)
      (add-complex (car complex-list) (complex-list-sum (cdr complex-list))))
  )

(display "Список комплексних чисел: ") complex-list  
(display "Добуток комплексних чисел: ") (complex-list-sum complex-list)
