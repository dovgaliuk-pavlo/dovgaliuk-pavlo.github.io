(display "Довгалюк Павло ІПЗ-41. Лаб 5. Варіант 5. Частина 1.\n")

;===========сума двох векторів===============
(define add-vectors
  (lambda (vec-1 vec-2)
    (let* ((len (vector-length vec-1))  ; The vectors shouldhave the same length.
           (result (make-vector len)))
      (do ((index 0 (+ index 1)))
        ((= index len) result)
        (vector-set! result index
                     (+ (vector-ref vec-1 index)
                        (vector-ref vec-2 index)))))))

;===========сума елементів вектора=================
(define vector-sum
  (lambda (vec)
    (let ((len (vector-length vec))
          (result 0))        
      (do ((index 0 (+ index 1)))
          ((= index len) result)
        (set! result (+ result (vector-ref vec index)))))))

;=================об'єднання векторів===================
(define vector-append
  (lambda vecs
    (let* ((len (apply + (map vector-length vecs)))
           (result (make-vector len)))
      (let loop ((result-index 0)
                 (source-index 0)
                 (rest-of-vecs vecs))
        (cond ((null? rest-of-vecs) result)
              ((= source-index (vector-length (car rest-of-vecs)))
               (loop result-index 0 (cdr rest-of-vecs)))
              (else
               (vector-set! result result-index
                            (vector-ref (car rest-of-vecs) source-index))
               (loop (+ result-index 1) (+ source-index 1) rest-of-vecs)))))))


;==================мінімальний елемент вектору==================
(define (vector-min vec)
  (let* ((len (vector-length vec)) (min (cons (vector-ref vec 0) 0)))
    (do ((index 0 (+ index 1))) ((= index len) min)
      (if (< (vector-ref vec index) (car min))
          (set! min (cons (vector-ref vec index) index))
          )
      )
    )
  )

;==================максимальний елемент вектору==================
(define (vector-max vec)
  (let* ((len (vector-length vec)) (max (cons (vector-ref vec 0) 0)))
    (do ((index 0 (+ index 1))) ((= index len) max)
      (if (> (vector-ref vec index) (car max))
          (set! max (cons (vector-ref vec index) index))
          )
      )
    )
  )

;=================найбільший серед від’ємних та найменший серед додатних елементів вектору===================


(define (minimax vec)
  (let* ((len (vector-length vec))
        (result (vector (vector-min vec) (vector-max vec))))
    (do ((index 0 (+ index 1))) ((= index len) result)
      (if (negative? (vector-ref vec index))
          (if (> (vector-ref vec index) (car (vector-ref result 0)))
               (vector-set! result 0 (cons (vector-ref vec index) index))
           )
          (if (< (vector-ref vec index) (car (vector-ref result 1)))
               (vector-set! result 1 (cons (vector-ref vec index) index))
           )
          )
      )
    )
  )

(define vec (vector -1 -25 -4 5 9 2))
(define miax (minimax vec))

(display "Вектор: ") vec
(display "Максимальне від'ємне(значення-індекс): ") (vector-ref miax 0)
(display "Мінімальне додатнє(значення-індекс): ") (vector-ref miax 1)



