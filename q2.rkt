;Author: Kudo_kun
#lang racket
(require typed-stack)
(require dyoo-while-loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define stk (make-stack))
(define lst (list))
(define t1 0)

(define (prec A)
    (cond
      [(equal? A #\+) 1]
      [(equal? A #\-) 1]
      [(equal? A #\*) 2]
      [(equal? A #\/) 2]
      [else -1]
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (infixToPostfix expr)
  (for([i expr])
    (cond
      [(or (equal? i #\*)  (equal? i #\+)  (equal? i #\/) (equal? i #\-))
       (while (and (not (stack-empty? stk)) (<= (prec i) (prec (top stk))))
              (set! lst (append lst (list (top stk))))
              (pop! stk)
       )
       (push! stk i)
       ]

      [(equal? i #\()
       (push! stk i)
       ]

      [(equal? i #\))
       (while (and (not(stack-empty? stk)) (not(equal? (top stk) #\()))
              (set! lst (append lst (list (top stk))))
              (pop! stk)
       )
       (if (equal? (top stk) #\()
           (pop! stk)
           (set! t1 0) ;random null and void statement
       )
       ]

      [else (set! lst (append lst (list i)))]
    )
  )

  (while (not (stack-empty? stk))
      (set! lst (append lst (list (top stk))))
      (pop! stk)
  )

  lst
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


