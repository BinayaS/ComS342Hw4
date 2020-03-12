#lang racket
(require racket/trace)
(require racket/include)
(require "testCases.rkt")
(include "testCases.rkt")
;(require "program.rkt")
(provide (all-defined-out))


;-----------Part 1 Helper Functions--------------;
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
          (if (and (isStatement parameter) (isSSeq (car (cdr parameter))))
              #t
              #f
           )
          (if (isStatement parameter)
              #t
              #f)
       )
      #f
   )
)

;Check if If
(define (isIf parameter)
  (if (equal? (length parameter) 3)
      (if (equal? (car parameter) 'if)
          (and (isCondExpr (car (cdr parameter))) (isSSeq (car (car (cdr (cdr parameter))))))
          #f
       )
      #f
   )
)

;Check if While
(define (isWhile parameter)
  (if (equal? (length parameter) 3)
      (if (equal? (car parameter) 'while)
          (and (isCondExpr (car (cdr parameter))) (isSSeq (car (car (cdr (cdr parameter))))))
          #f
      )
      #f
   )
)

;Check if Statement
(define (isStatement parameter)
  (if (equal? (cond
                [(if (isDecl parameter) #t #f)]
                [(if (isAssign parameter) #t #f)]
                [(if (isIf parameter) #t #f)]
                [(if (isWhile parameter) #t #f)]
                )
              #t)
      #t
      #f
   )
)

;--------------Synchk-----------------
(define (synchk program)
  (if (and (list? program) (> (length program) 0))
      (isSSeq (car program))
      #f
   )
)


;---------------Part 2 Helper Functions----------------
(define (getAtI Enviornment i)
  (if (= i 0)
      (car Enviornment)
      (getAtI (cdr Enviornment) (- i 1))
   )
)

(define (findVar Var Enviornment i)
  (if (= (length Enviornment) 0)
      -1
      (if (equal? (car (car Enviornment)) Var)
          i
          (findVar Var (cdr Enviornment) (+ i 1))
      )
   )
)

(define (changeAtI Var Change Enviornment front i)
  (if (= i 0)
      (if (= (length front) 0)
          (append (list (list Var Change)) (cdr Enviornment))
          (if (= (length Enviornment) 1)
              (append front (list (list Var Change)))
              (append front (append (list (list Var Change)) (cdr Enviornment)))
           )
       )
      (changeAtI Var Change (cdr Enviornment) (if (= (length front) 0) (list (car Enviornment)) (append front (list (car Enviornment)))) (- i 1))
   )
)

(define (applyOp Value ArithExpr ArithExpr2 Envornment)
  (cond
    [(if (equal? Value '+) (+ (applyArithExpr ArithExpr) (applyArithExpr ArithExpr2)) #f)]
    [(if (equal? Value '-) (- (applyArithExpr ArithExpr) (applyArithExpr ArithExpr2)) #f)]
    [(if (equal? Value '*) (* (applyArithExpr ArithExpr) (applyArithExpr ArithExpr2)) #f)]
    [(if (equal? Value '/) (/ (applyArithExpr ArithExpr) (applyArithExpr ArithExpr2)) #f)]
  )
)

(define (applyArithExpr Value Enviornment)
  ;If is a list and of length 3
  ;Evaluate Op and other ArithExpr
  ;else
  ;Evaluate Num or Var
  (if (and (list? Value) (= (length Value) 3))
      (applyOp (car Value) (applyArithExpr (car (cdr Value))) (applyArithExpr (car (cdr (cdr Value)))) Enviornment)
      (cond
        [(if (number? Value) Value #f)]
        [(if (symbol? Value) (car (cdr (getAtI Enviornment (findVar Value Enviornment 0)))) #f)]
        )
  )
)

;--------------Changes the Enviornment---------------
(define (applyAssign Var ArithExpr Enviornment)
  ;Find Var in Enviornment
  ;Get the value for ArithExpr
  ;Change the value of Var to be ArithExpr
  (if (= (findVar Var Enviornment 0) -1)
      #f
      (changeAtI Var (applyArithExpr ArithExpr Enviornment) Enviornment '() (findVar Var Enviornment 0))
  )
)

(define (applyDecl Var Enviornment)
  (if (= (length Enviornment) 0) (list (list Var 0))
  (append (list(list Var 0)) Enviornment))
)

;--------------Changes the Scope------------------
(define (applyIf program Enviornment)
  ;TODO
  0
)

(define (applyWhile program Enviornment)
  ;TODO
  0
)

;----------------Sem-----------------
(define (sem program Enviornment)
  ;TODO
  ;Figure out if Decl | Assign | If | While
  (if (isDecl )
      (applyDecl )
      (if (isAssign )
          (applyAssign )
          (if (isIf )
              (applyIf )
              (if (isWhile )
                  (applyWhile )
                  #f
               )
           )
       )
   )
)

;(trace applyAssign)
(trace changeAtI)