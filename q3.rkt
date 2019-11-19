;Author: Kudo_kun
#lang racket
(require typed-stack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define stk (make-stack))
(define t1 0)
(define t2 0)
(define t3 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (evalPostfixExpr expr)
  (for([i expr])
    (cond
      [(equal? i #\+)
       (set! t1 (top stk))
       (pop! stk)
       (set! t2 (top stk))
       (pop! stk)
       (set! t3 (+ t2 t1))
       (push! stk t3)
       ]

      [(equal? i #\*)
       (set! t1 (top stk))
       (pop! stk)
       (set! t2 (top stk))
       (pop! stk)
       (set! t3 (* t2 t1))
       (push! stk t3)
       ]

      [(equal? i #\/)
       (set! t1 (top stk))
       (pop! stk)
       (set! t2 (top stk))
       (pop! stk)
       (set! t3 (/ t2 t1))
       (push! stk t3)
       ]

     [(equal? i #\-)
       (set! t1 (top stk))
       (pop! stk)
       (set! t2 (top stk))
       (pop! stk)
       (set! t3 (- t2 t1))
       (push! stk t3)
       ]

      [else (push! stk i)]
    )
  )

  (top stk)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





