#lang racket
(require racket/trace)
(require racket/include)
(require "testCases.rkt")
(include "testCases.rkt")
;(require "program.rkt")
(provide (all-defined-out))


;-----------Helper Functions--------------;
;Check if Symbol
(define (isSymbol parameter)
  (if (symbol? parameter) #t #f)
)

;Check if Op
(define (isOp parameter)
  (if (equal? (cond
    ;Should these be a symbol?
    [(if (equal? parameter '+) #t #f)]
    [(if (equal? parameter '-) #t #f)]
    [(if (equal? parameter '*) #t #f)]
    [(if (equal? parameter '/) #t #f)]
  ) #t) #t #f)
)

;Check if Number
(define (isNumber parameter)
  (if (number? parameter) #t #f)
)

;Check if Var
(define (isVar parameter)
  (isSymbol parameter)
)

;Check if Decl
(define (isDecl parameter)
  (if (equal? (length parameter) 2) (if (equal? (car parameter) 'decl) (isVar (car (cdr parameter))) #f) #f)
)

;Check if ArithExpr
(define (isArithExpr parameter)
  (if (equal? (cond
    [(if (if (list? parameter) (= (length parameter) 3) #f) (if (isOp (car parameter)) (and (isArithExpr (car (cdr parameter))) (isArithExpr (car (cdr (cdr parameter))))) #f) #f)]
    [(isNumber parameter)]
    [(isVar parameter)]
  ) #t) #t #f)
)

;Check if Assign
(define (isAssign parameter)
  (if (= (length parameter) 3) (if (equal? (car parameter) 'assign) (and (isVar (car (cdr parameter))) (isArithExpr (car (cdr (cdr parameter))))) #f) #f)
)

;Check if Bcond
(define (isBcond parameter)
  (if (list? parameter) (if (= (length parameter) 3) (if (equal? (cond
    [(if (equal? (car parameter) 'gt) (and (isArithExpr (car (cdr parameter))) (isArithExpr (car (cdr (cdr parameter))))) #f)]
    [(if (equal? (car parameter) 'lt) (and (isArithExpr (car (cdr parameter))) (isArithExpr (car (cdr (cdr parameter))))) #f)]
    [(if (equal? (car parameter) 'eq) (and (isArithExpr (car (cdr parameter))) (isArithExpr (car (cdr (cdr parameter))))) #f)]
    ) #t) #t #f) #f) #f)
)

;Check if CondExpr
(define (isCondExpr parameter)
  (if (list? parameter)
      (if (= (length parameter) 3)
          (if (equal?
               (cond
                 [(if (equal? (car parameter) 'or) (and (isCondExpr (car (cdr parameter))) (isCondExpr (car (cdr (cdr parameter))))) #f)]
                 [(if (equal? (car parameter) 'and) (and (isCondExpr (car (cdr parameter))) (isCondExpr (car (cdr (cdr parameter))))) #f)]
                 [(if (isBcond parameter) #t #f)]
               )
               #t) #t #f)
          (if (= (length parameter) 2)
              (if (equal? (car parameter) 'not) (isCondExpr (car (cdr parameter))) #f) #f)) #f)
)

;Check if a SSeq
(define (isSSeq parameter)
  (if (list? parameter)
      (if (and (= (length parameter) 2) (not (isSymbol (car (cdr parameter)))))
          (if (and (isStatement parameter) (isSSeq (car (cdr parameter)))) #t #f)
          (if (isStatement parameter) #t #f)) #f)
)

;Check if If
(define (isIf parameter)
  (if (equal? (length parameter) 3) (if (equal? (car parameter) 'if) (and (isCondExpr (car (cdr parameter))) (isSSeq (car (car (cdr (cdr parameter)))))) #f) #f)
)

;Check if While
(define (isWhile parameter)
  (if (equal? (length parameter) 3) (if (equal? (car parameter) 'while) (and (isCondExpr (car (cdr parameter))) (isSSeq (car (car (cdr (cdr parameter)))))) #f) #f)
)

;Check if Statement
(define (isStatement parameter)
  (if (equal? (cond
    [(if (isDecl parameter) #t #f)]
    [(if (isAssign parameter) #t #f)]
    [(if (isIf parameter) #t #f)]
    [(if (isWhile parameter) #t #f)]
  ) #t) #t #f)
)

;--------------Entry point-----------------
(define (synchk program)
  (isSSeq (car program))
)

;(trace synchk)
;(trace isStatement)
;(trace isSSeq)
;(trace isCondExpr)
;(trace isBcond)
;(trace isOp)
;(trace isSymbol)
;(trace isNumber)
;(trace isVar)
;(trace isAssign)
;(trace isArithExpr)
;(trace isDecl)

