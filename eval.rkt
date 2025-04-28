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


(define add
  (lambda (a b)
    (cond ((number? a) (+ a b))
          ((list? a) (append a b))
          (else (error "unable to add" a b)))))
(define e1  (map list
                 '(     x  y  z ls + - * cons car cdr nil list add = nil? else)
                 (list 10 20 30 (list 1 2) + - * cons car cdr '() list add = empty? #t)))

;;Returns the prodecure in env of the passed symbol sym
;;Returns an error if x is not a symbol in environment env

(define lookup
  (lambda (sym env)
    (if (equal? (length env) 0)
        (error "Symbol does not exist in the environment")
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
     ((number? x) x)
     ((symbol? x) (lookup x env))
     ((special-form? x) (esf x env))
     ((list? x) (apply (evaluate (car x) env) (map (lambda (y) (evaluate y env)) (cdr x)))))))

(define esf
  (lambda (x env)
    (cond
      ((equal? (car x) 'if)
          (if ((lookup (caadr x) env) (cadadr x) (car (cddadr x)))
              (caddr x)
              (cadddr x)))
      ((equal? (car x) 'cond)
          ;;(esf '(if ) env)
       x)
      (else (error "Passed non-special form")))
    
    ))

(define condHelp
  (lambda (para)
    para
    ))

;;Returns true if the first value of the passed list is one of the following special phrases:
;;if, cond

(define special-form?
  (lambda (check)
    (or (equal? (car check) 'if) (equal? (car check) 'cond))))

(define evaluate-special-form
  (lambda (x y)
    x
    ))