(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (if (null? items) nil (cons (proc (car items)) (map proc (cdr items)))))

(define (cons-all first rests)
  (if (null? rests)
    nil
    (cons (cons first (car rests)) (cons-all first (cdr rests))))
  )

(define (zip pairs)
  (define (first-zipper pairs)
    (if (null? pairs) 
      nil
      (cons (caar pairs) (first-zipper (cdr pairs)))))
  (define (second-zipper pairs)
    (if (null? pairs)
      nil
      (cons (cadar pairs) (second-zipper (cdr pairs)))))
  (list (first-zipper pairs) (second-zipper pairs))
  )

;; Problem 18
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN Question 18
  (define (lst-maker lst count)
    (if (null? lst)
      nil
      (cons (cons count (cons (car lst) nil)) (lst-maker (cdr lst) (+ count 1)))
    )
  )
  (lst-maker s 0)
)
  ; END Question 18
; Problem 19

;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN Question 19
 (cond ((null? denoms) nil) ((= total 0) (list nil))
    ((> (car denoms) total)
      (list-change total (cdr denoms))) 
    (else (append (cons-all (car denoms)
                            (list-change (- total (car denoms)) denoms))
                  (list-change total (cdr denoms))))
  )
)  
  ; END Question 19


;; Problem 20
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (analyze expr)
  (cond ((atom? expr)
         ; BEGIN Question 20
         expr
         ; END Question 20
         )
        ((quoted? expr)
         ; BEGIN Question 20
         expr
         ; END Question 20
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20
           (define initial-lst (cons form (list params (car body))))
           (if (null? (cdr body))
            initial-lst
            (append initial-lst (list (analyze (cadr body))))
            )
           ; END Question 20
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20
           (define success1 (map analyze (car (zip values))))
           (define success2 (map analyze (cadr (zip values))))
           (define success3 (map analyze body))
           ; SUCCESS BRINGS SUCCESS.
           (cons (cons 'lambda (cons success1 success3)) success2)
           ; END Question 20
           ))
        (else
         ; BEGIN Question 20
         (map analyze expr)
         ; END Question 20
         )))