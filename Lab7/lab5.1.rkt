(display "Довгалюк Павло ІПЗ-41. Лаб 5. Варіант 5. Частина 1.\n")

(define (numer x) (car x))		; чисельник
(define (denom x) (cdr x))		; знаменник
(define (make-rat n d) (cons n d))	; створення пари
(define(print-rat x)			; друк пари 
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;===========додавання дробів===============
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y) )))
;===========віднімання дробів===============
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
;===============ділення дробів=======================
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
;===============множення дробів=======================
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
;===============порівняння дробів=======================
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
;===============створення та скорочення дробу==============
(define (make-rat n d)
  (let ((nod (gcd n d)))
    (cons (/ n nod) (/ d nod))))


(define all-way (make-rat 1 1))
(define now (make-rat 1 3))
(define center (make-rat 1 2))
(define diff (make-rat 2 1))

(define half-way (sub-rat center now))
(define way (div-rat half-way diff))
(display "Весь шлях: ") (cdr way)
(display "Залишилося йти: ") (cdr (div-rat way (sub-rat all-way now)))