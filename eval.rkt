#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Spring 2025
;;
;; Lab #5
;;
;; Logan Britt
;; W01638650
;;
;; Evalutes an expression including special symbols that take multiple parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide lookup evaluate)
(provide special-form? evaluate-special-form)

(define add
  (lambda (a b)
    (cond ((number? a) (+ a b))
          ((list? a) (append a b))
          (else (error "unable to add" a b)))))

(define e1  (map list
                 '(     x  y  z + - * cons car cdr nil list add = else )
                 (list 10 20 30 + - * cons car cdr '() list add = #t   )))

;;Returns the prodecure in env of the passed symbol sym
;;Returns an error if x is not a symbol in environment env

(define lookup
  (lambda (sym env)
    (if (equal? (length env) 0)
        (error "Symbol " sym " does not exist in the environment")
        (if (symbol? sym)
            (if (equal? (car (car env)) sym)
                (car (cdr (car env)))
                (lookup sym (cdr env)))
            (error sym)))
    
    ))

;;If x is a number, return the number x
;;If x is a symbol, returns the value assigned to the symbol in environment env
;;If x is a list, applys evaluate to the list and returns that value for evaluation
(define evaluate
  (lambda (x env)
    (cond
     ((or (number? x) (procedure? x))  x)
     ((symbol? x) (lookup x env))
     ((special-form? x) (evaluate-special-form x env))
     ((list? x) (apply (evaluate (evaluate (car x) env) env) (map (lambda (y) (evaluate y env)) (cdr x))))
     )))

;;Runs a special evaluation for multi-parameter special symbols
;;Currently accepts symbols: 'if, 'cond. 'let
(define evaluate-special-form
  (lambda (x env)
    (cond
      ((equal? (car x) 'if)
       (if (evaluate (cadr x) env)
       (caddr x)
       (evaluate (cadddr x) env)))
      ((equal? (car x) 'cond)
       (condRec (cdr x) env))
      ((equal? (car x) 'let)
       (letAllowance (cdr x) env))
      (else (error "Passed non-special form"))
    
   )))

;;Recursively evaluates the value of the first parameter if true
;;If false, call again on rest of the list of conditions
(define condRec
  (lambda (para env)
    (if (evaluate (caar para) env)
        (evaluate (cadar para) env)
        (condRec (cdr para) env))
    ))

;;Returns true if the first value of the passed list is one of
;;the following special phrases 'if, 'cond. 'let
(define special-form?
  (lambda (check)
    (or
     (equal? (car check) 'if)
     (equal? (car check) 'cond)
     (equal? (car check) 'let))))

(define letAllowance
  (lambda (x env)
    (cond
      ((greater? (length (car x)) 0) (letAllowance (cons (cdar x) (cdr x)) (cons (evalInner (caar x) env) env)))
      (else (evaluate (cadr x) env)))))
      ;(else env))))

(define evalInner
  (lambda (x env)
    (list (car x) (evaluate (cadr x) env))
    ))

;;Returns true if x > y
(define greater?
  (lambda (x y)
    (positive? (- x y))))

;;Returns true if y > x
(define lesser?
  (lambda (x y)
    (positive? (- y x))))

;;Returns true if x >= y
(define greatEq?
  (lambda (x y)
    (or (equal? (- x y) 0) (greater? x y))))

;;Returns true if y >= x
(define lessEq?
  (lambda (x y)
    (or (equal? (- y x) 0) (lesser? x y))))