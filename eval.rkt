#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Spring 2025
;;
;; Lab #7
;;
;; Logan Britt
;; W01638650
;;
;; Evalutes an expression including special symbols that take multiple parameters
;; Also allows for the evaluation of expressions with previously declared variables using let
;; Includes functionality for using the lambda function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide lookup evaluate)
(provide special-form? evaluate-special-form)

;;
;Below is added case environment and function 'add' for testing
(define add
  (lambda (a b)
    (cond ((number? a) (+ a b))
          ((list? a) (append a b))
          (else (error "unable to add" a b)))))

(define e1  (map list
                 '(     x  y  z + - * cons car cdr nil list add = else )
                 (list 10 20 30 + - * cons car cdr '() list add = #t   )))
;;

;;Below is a group of specifics for closure usage
(define closure
(lambda (vars body env)
;(mcons ’closure (mcons env (mcons vars body)))))
(mcons closure (mcons env (mcons vars body)))))
(define closure?
;(lambda (clos) (and (mpair? clos) (eq? (mcar clos) ’closure))))
(lambda (clos) (and (mpair? clos) (eq? (mcar clos) closure))))
(define closure-env
(lambda (clos) (mcar (mcdr clos))))
(define closure-vars
(lambda (clos) (mcar (mcdr (mcdr clos)))))
(define closure-body
(lambda (clos) (mcdr (mcdr (mcdr clos)))))
(define set-closure-env!
(lambda (clos new-env) (set-mcar! (mcdr clos) new-env)))

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
;;If x is a list, generates a closure if it's a lambda function and applies it as such
;;or applys evaluate to the list and returns that value for evaluation
(define evaluate
  (lambda (x env)
    (cond
     ((or (number? x) (procedure? x))  x)
     ((symbol? x) (lookup x env))
     ((lambda? x) (closure (cadr x) (caddr x) env))
     ((special-form? x) (evaluate-special-form x env))
     ((list? x)
       (let ((func (evaluate (car x) env))
             (args (map (lambda (y) (evaluate y env)) (cdr x))))
         (cond
           ((procedure? func) (apply func args))
           ((closure? func)   (apply-closure func args))
           (else (error "Unknown Function Type" func)))))
     )))

;;Runs a special evaluation for multi-parameter special symbols
;;Currently accepts symbols: 'if, 'cond, 'let
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
       (evaluate (caddr x) (addTo (evalDecs (cadr x) env) env)))
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
;;the following special phrases 'if, 'cond, 'let
(define special-form?
  (lambda (check)
    (or
     (equal? (car check) 'if)
     (equal? (car check) 'cond)
     (equal? (car check) 'let))))

;;Returns an environment with the added variables in list x
;;into the base environment env
(define addTo
  (lambda (x env)
    (cond ((equal? (length x) 1)
           (cons (car x) env))
          (else
           (cons (car x) (addTo (cdr x) env))))
    ))

;;Evaluates everything in the passed list with respect to the passed environment
;;Returns the same list of variables post-evaluation
(define evalDecs
  (lambda (decs env)
    (cond ((greater? (length decs) 0)
           (cons (list (caar decs) (evaluate (cadar decs) env)) (evalDecs (cdr decs) env)))
           (else decs))
    ))

;;Expands the environment in close with the variables in close
;;Then computes the body of close with the expanded environment
(define apply-closure
  (lambda (close vals)
    (let*
        ((vars (closure-vars close))
         (body (closure-body close))
         (env (closure-env close))
         (newEnv (addTo (map list vars vals) env)))
      (evaluate body newEnv))
    ))

;;Returns true if the passed list begins with 'lambda
(define lambda?
  (lambda (x)
    (and (list? x)
         (eq? (car x) 'lambda)
         (list? (cadr x)))))


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