(#%require scheme/base)
(#%require scheme/list)

(display "Довгалюк Павло ІПЗ-41. Лаб 4. Варіант 5. Частина 2.\n")

(define buyers (list 5 3 1 2 4))
(define serviceTime (list 21 18 25 16 48))


(define (minLst lst)
  (if (= (length lst) 1)
      (car lst)
      (min (car lst) (minLst (cdr lst)))
      ))


(define (countInLineTime timeLst counter)
  (if (null? timeLst)
      '()
      (cons (+ counter (car timeLst))
            (countInLineTime (cdr timeLst) (+ counter (car timeLst))))
      )
  )

(define (countInLineTime timeLst counter)
  (if (null? timeLst)
      '()
      (cons (+ counter (car timeLst))
            (countInLineTime (cdr timeLst) (+ counter (car timeLst))))
      )
  )

(define (lstIndexOf lst x)
  (- (length lst) (length (member x lst))))

(define (showLine personId inLineTime)
  (sleep 1) ;(sleep inLineTime)
  (display "Person(id): ")
  (display personId)
  (display ";  Time in line: ")
  (display inLineTime)
  (newline))

(define (lineProcess buyers buyersInLineTime)
  (showLine (car buyers) (car buyersInLineTime))
  (if (= (length buyers) 1)
      (display "End of queue")
      (lineProcess (cdr buyers) (cdr buyersInLineTime))
      )
  )

(define minServiceTime (minLst serviceTime))
(define indexOfMinServiceTime (lstIndexOf serviceTime minServiceTime))
(define minServiceTimeBuyerId (list-ref buyers indexOfMinServiceTime))
(define buyersInLineTime (countInLineTime serviceTime 0))


(display "Buyers(id): ")
buyers
(display "Time of service(sec): ")
serviceTime
(display "A person with the shortest service time(id): ")
minServiceTimeBuyerId

(lineProcess buyers buyersInLineTime)

