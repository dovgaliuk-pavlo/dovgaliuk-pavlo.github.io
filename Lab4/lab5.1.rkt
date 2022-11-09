(#%require racket/list)

(display "Довгалюк Павло ІПЗ-41. Лаб 4. Варіант 5. Частина 1.\n")

(define x 2)
(define n 8)

; creation
(define (getListPowers x i n)
  (cond ((= i n) '())
        ((< i n) (cons (expt x i) (getListPowers x (+ i 1) n)))))

(define (createListPowers x n)
  (let ((i 1) (n (+ n 1)))
    (getListPowers x i n)))

; task a
(define (returnByStep lst i n step)
  (cond ((> i ( - n 1)) '())
        ((= i ( - n 1)) (list (list-ref lst i)))
        ((< i ( - n 1)) (cons (list-ref lst i) (returnByStep lst (+ i step) n step)))))

(define (getOddElements lst n position)
  (returnByStep lst 0 n position))

(define (getEvenElements lst n position)
  (returnByStep lst 1 n position))

; task b
(define (reverseByPosition lst i)
  (cond ((<= i 0) (reverse lst))
        ((> i (length lst)) (reverse lst))
        (else (append
             (reverse (member (list-ref lst (- i 1)) (reverse lst)))
             (reverse (member (list-ref lst i) lst))))
        )
  )

; task c
(define (lstSum lst)
    (if (null? lst)
        0
        (+ (car lst) (lstSum (cdr lst)))))


; output
(display "List filled with powers of 2: ")
(define PowersO2 (createListPowers x n))
PowersO2
(newline)

(display "a) List with second elements deleted: ")
(getOddElements PowersO2 n 2)
(newline)

(display "b) Inverted list by index (2): ")
(reverseByPosition PowersO2 2)
(newline)


(display "c) Sum even elements of list ")
(define lst (getOddElements PowersO2 n 2))
(display lst)
(display ": ")
(lstSum lst)
(newline)

