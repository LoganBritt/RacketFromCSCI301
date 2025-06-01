#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Spring 2025
;;
;; Lab #8
;;
;; Logan Britt
;; W01638650
;;
;; Implements a recursive-descent parser for a small Scheme
;; subset using an LL(1) grammar.
;; It parses a string into Racket expressions suitable
;; for evaluation using eval.rkt.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide parse)

;; Checks if char is a symbol that is not on of the following:
;; Whitespace ( ), parentheses (()), and numeric digits (1, 2, ... ,9)
(define char-symbolic?
  (lambda (char)
    (and (not (char-whitespace? char))
         (not (char-numeric? char))
         (not (equal? char #\())
         (not (equal? char #\))))))

;; Converts a char to a number
(define char->number
  (lambda (char)
    (- (char->integer char) (char->integer #\0))))

;; Parses functions written as "(+ 1 1)" into form '(+ 1 1)
(define parse
  (lambda (str)
    (let-values (((tree rest) (L (string->list str))))
      tree)))

;; Handles the L state (L -> L | EL | {})
(define L
  (lambda (chars)
    (cond
      ((null? chars) (values '() '()))
      ((char-whitespace? (car chars)) (L (cdr chars)))
      ((equal? (car chars) #\)) (values '() chars))
      (else
       (let-values (((e1 rem1) (E chars)))
         (let-values (((e2 rem2) (L rem1)))
           (values (cons e1 e2) rem2)))))
    ))

;; Handles the E state (E -> D N | A S | ( L ))
(define E
  (lambda (chars)
    (cond
      ((null? chars) (error "Unexpected end of input in E"))
      ((char-whitespace? (car chars)) (E (cdr chars)))
      ((equal? (car chars) #\()
       (let-values (((lst rest1) (L (cdr chars))))
         (if (and (not (null? rest1)) (equal? (car rest1) #\)))
             (values lst (cdr rest1))
             (error "Expected ')'"))))
      ((char-numeric? (car chars))
       (let-values (((nval rest) (N (char->number (car chars)) (cdr chars))))
         (values nval rest)))
      ((char-symbolic? (car chars))
       (let-values (((sym rest) (S (list (car chars)) (cdr chars))))
         (values sym rest)))
      (else
       (error "Unexpected character in E" (car chars))))
    ))

;; Handles the N state (N -> D N | {}) 
(define N
  (lambda (inherit chars)
    (cond
      ((null? chars) (values inherit '()))
      ((char-numeric? (car chars))
       (let* ((digit (char->number (car chars)))
              (new-inherit (+ (* inherit 10) digit)))
         (N new-inherit (cdr chars))))
      (else (values inherit chars)))
    ))

;; Handles the S state (S -> A S | {})
(define S
  (lambda (inherit chars)
    (cond
      ((null? chars)
       (values (string->symbol (list->string inherit)) '()))
      ((char-symbolic? (car chars))
       (S (append inherit (list (car chars))) (cdr chars)))
      (else
       (values (string->symbol (list->string inherit)) chars)))
    ))
