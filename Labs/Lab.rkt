; NAME: Norman Kuang
; CS145 - Naumov

; Given Helper Functions
(define in 
  (lambda (x A)
    (cond
      ((null? A) #f)
      ((eq? x (car A)) #t)
      (else (in x (cdr A))))))

(define remove
  (lambda (x A)
    (cond
      ((null? A) A)
      ((eq? x (car A)) (remove x (cdr A)))
      (else (cons (car A) (remove x (cdr A)))))))

(define remove-multiples
  (lambda (A)
    (cond
      ((null? A) A)
      (else (cons (car A)
                (remove (car A) (remove-multiples (cdr A))))))))

; Problem 1: Union
(define union
  (lambda (A B)
   (remove-multiples (append A B))))

; Problem 2: Intersection
(define intersection
  (lambda (A B)
    (cond
      ((null? A) '())
      ((in (car A) B) (cons (car A) (intersection (cdr A) B)))
      (else (intersection (cdr A) B)))))

; Problem 3: Difference
(define difference
  (lambda (A B)
    (cond
      ((null? A) '())
      ((not (in (car A) B)) (cons (car A) (difference (cdr A) B)))
      (else (difference (cdr A) B)))))

; Problem 4: Symmetric-Difference
(define symmetric-difference
  (lambda (A B)
    (difference (union A B) (intersection A B)))) 


; Problem 5: Subset
(define subset
  (lambda (A B)
    (equal? (intersection A B) A)))

; Problem 6: Disjoint
(define disjoint
  (lambda (A B)
    (null? (intersection A B))))

; Problem 7: Family-Union
(define family-union
  (lambda (A)
    (cond
      ((null? A) A)
      (else (remove-multiples (union (car A) (family-union (cdr A))))))))


; Problem 8: Family-Intersection
(define family-intersection
  (lambda (A)
   (cond
     ((null? (cdr A)) (car A))
     (else (intersection (car A) (family-intersection (cdr A)))))))

; Problem 9: Powerset
(define power
  (lambda (A)
    (cond
      ((null? A) '(()))
      (else (remove-multiples (append (consElement (car A) (power (cdr A)))
                  (power (cdr A))))))))

(define consElement
  (lambda (x A)
    (cond
      ((null? A) '(()))
      (else (cons (cons x (car A)) (consElement x (cdr A)))))))

; Problem 10: Choose
(define choose
  (lambda (x A)
    (filterPower x (power A))))

(define filterPower
  (lambda (x A)
    (cond
      ((null? A) '())
      ((= (length (car A)) x) (cons (car A) (filterPower x (cdr A))))
      (else (filterPower x (cdr A))))))

; Problem 11: Reflexive
(define reflexive
  (lambda (A B)
    (cond
      ((null? A) #t)
      ((member? (list (car A) (car A)) B) (reflexive (cdr A) B))
      (else #f))))

(define member?
  (lambda (x A)
    (cond
      ((equal? (member x A) #f) #f)
      (else #t))))

; Problem 12: Symmetric
(define symmetric
  (lambda (x A)
    (symmetric-helper x A A)))

(define symmetric-helper
  (lambda (x A Acopy)
    (cond
      ((null? A) #t)
      (else
       (and (member? (reverse (car A)) Acopy)
            (symmetric-helper x (cdr A) Acopy))))))

; Problem 13: Transitive
(define transitive
  (lambda (x A)
    (transitive-helper A A)))

(define transitive-helper
  (lambda (A Acopy)
    (cond
      ((null? A) #t)
      (else (and (transitive-pair-function (car A) Acopy Acopy)
                 (transitive-helper (cdr A) Acopy))))))

(define transitive-pair-function
  (lambda (x A Acopy)
    (cond
      ((null? A) #t)
      ((equal? (car (cdr x)) (car (car A)))
       (and (member? (list (car x) (car (cdr (car A)))) Acopy)
            (transitive-pair-function x (cdr A) Acopy)))
      (else (transitive-pair-function x (cdr A) Acopy)))))
      
(transitive `(1 2 3) `((1 2) (2 3) (1 3)))
(transitive `(1 2 3) `((1 1) (2 3)))
(transitive `(1 2 3) `((1 2) (3 1)))
(transitive `(1 2 3) `((1 2) (1 3)))










    