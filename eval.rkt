#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Spring 2025
;;
;; Lab #4
;;
;; Logan Britt
;; W01638650
;;
;; Purpose
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide lookup evaluate)
(provide special-form? evaluate-special-form)

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
;;If x is a list, applys evaluate to the list and returns that value
(define evaluate
  (lambda (x env)
    (cond
     ((or (number? x) (procedure? x))  x)
     ((symbol? x) (lookup x env))
     ((special-form? x) (evaluate-special-form x env))
     ((list? x) (apply (evaluate (evaluate (car x) env) env) (map (lambda (y) (evaluate y env)) (cdr x))))
     )))

;;Runs a special evaluation for multi-parameter special symbols
;;Currently accepts symbols: 'if, 'cond
(define evaluate-special-form
  (lambda (x env)
    (cond
      ((equal? (car x) 'if)
       (if (evaluate (cadr x) env)
       (caddr x)
       (evaluate (cadddr x) env)))
      ((equal? (car x) 'cond)
       (condRec (cdr x) env))
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

;;Returns true if the first value of the passed list is one of the following special phrases:
;;'if, 'cond
(define special-form?
  (lambda (check)
    (or (equal? (car check) 'if) (equal? (car check) 'cond))))
