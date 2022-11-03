(display "Довгалюк Павло ІПЗ-41. Лаб 3. Варіант 5. Частина 1.\n")

(define (display_formated . m_part)
  (for-each display m_part))

(define a 5)
(define b 10)
(define acc 0.0001)
(define root 0)

(define (average a b) (/ (+ a b) 2))
(define (isAccurate x) (< (abs x) acc))

(define (func x) (- (+ 5 (cos x)) x))
(define (derFunc x) (- -1 (sin x)))


(define (bisec a b)
  (let ((divideStep (average a b)))
    (if (isAccurate (func divideStep))
        (set! root divideStep)
        (cond ((or (and (negative? (func a)) (positive? (func divideStep)))
                  (and (positive? (func a)) (negative? (func divideStep)))) (bisec a divideStep))
              ((or(and (negative? (func divideStep)) (positive? (func b)))
                  (and (positive? (func divideStep)) (negative? (func b)))) (bisec divideStep b))))))

(define x1 0)
(define (newton x0)
        (set! x1 (- x0 (/ (func x0) (derFunc x0))))
            (if (isAccurate(- x1 x0))
            (set! root x0)
            (newton x1)))
  
(display "Bisec method: ")
(bisec a b)
root
(newline)

(display "Newton method: ")
(newton a)
root

