(display "Довгалюк Павло ІПЗ-41. Лаб 6. Варіант 5. Частина 2.\n")

; Створити дві черги.
; Об’єднати дві черги в одну,
; елементи якої вибрані по черзі з вхідних черг, наприклад,
; перший елемент першої черги, перший елемент другої,
; другий елемент першої черги, другий елемент другої і т.д.. 


;;#######Implimenting Queue#########
(define (make-queue)
 (define p (cons '() '()))
 (cons p p)
)
;========== ==Перевірка черги на пустоту==========
(define (null-queue? q)
 (and
  (eq? (front q) (rear q)) (eq? (car (front q)) '() ))
)
;===== селектор (доступ) до першого елемента черги===
(define (front q)
 (car q)) 
;==== селектор (доступ) до останнього елемента черги ==
(define (rear q)
 (cdr q))
;========додавання нового елемента в чергу=========
(define (push q e)
 (define p (cons e '()))
 (if (null-queue? q)
  (begin (set-car! q p)
   (set-cdr! q p)
  )
  (begin
   (set-cdr! (rear q) p)
   (set-cdr! q p)
  ) ) )
;========== вилучення елемента з черги============
(define (pop q)
 (define x 0)
 (if (null-queue? q)
  'Empty    
  (if (and (eq? (front q) (rear q))  (eq? '() (cdr (front q)))   )
   (begin
    (set! x (car (front q)))
    (set-car! (front q) '() )
    x ) 
   (begin
    (set! x (car (front q)))
    (set-car! q (cdr (front q)) )
    x ))))

;==========Об’єднання двух черг============
(define (queue-join q1 q2)
  (let* ((q (make-queue)))
    (do ((index 0 (+ index 1))) ((and (null-queue? q1) (null-queue? q2)) q)
      (if (not (null-queue? q1))
          (push q (pop q1))
          )
      (if (not (null-queue? q2))
          (push q (pop q2))
          )
      )
    )
  )

(define q1 (make-queue))
(define q2 (make-queue))
(push q1 1) (push q1 3) (push q1 5) (push q1 7) (push q1 9)
(push q2 2) (push q2 4) (push q2 6) (push q2 8) 

(display "Перша черга: ") q1
(display "Друга черга: ") q2
(display "Об'єднана черга: ") (queue-join q1 q2)