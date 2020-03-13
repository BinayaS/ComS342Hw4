#lang racket
(require "program.rkt")
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
  (if (equal? (length parameter) 2)
      (if (equal? (car parameter) 'decl)
          (isVar (car (cdr parameter)))
          #f
       )
      #f
   )
)

;Check if ArithExpr
(define (isArithExpr parameter)
  (if (equal? (cond
                [(if (if (list? parameter)
                         (= (length parameter) 3)
                         #f
                      )
                     (if (isOp (car parameter))
                         (and (isArithExpr (car (cdr parameter))) (isArithExpr (car (cdr (cdr parameter)))))
                         #f
                      )
                     #f
                  )]
                [(isNumber parameter)]
                [(isVar parameter)]
                ) #t)
      #t
      #f
   )
)

;Check if Assign
(define (isAssign parameter)
  (if (= (length parameter) 3)
      (if (equal? (car parameter) 'assign)
          (and (isVar (car (cdr parameter))) (isArithExpr (car (cdr (cdr parameter)))))
          #f
       )
      #f
   )
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
               #t)
              #t
              #f
           )
          (if (= (length parameter) 2)
              (if (equal? (car parameter) 'not)
                  (isCondExpr (car (cdr parameter)))
                  #f
               )
              #f
           )
       )
      #f
   )
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
              #f
           )
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
(define (removeI Environment i)
  (if (= i 0)
      Environment
      (removeI (cdr Environment) (- i 1))
   )
)

(define (getAtI Environment i)
  (if (= i 0)
      (car Environment)
      (getAtI (cdr Environment) (- i 1))
   )
)

(define (findVar Var Environment i)
  (if (= (length Environment) 0)
      -1
      (if (equal? (car (car Environment)) Var)
          i
          (findVar Var (cdr Environment) (+ i 1))
      )
   )
)

(define (changeAtI Var Change Environment front i)
  (if (= i 0)
      (if (= (length front) 0)
          (append (list (list Var Change)) (cdr Environment))
          (if (= (length Environment) 1)
              (append front (list (list Var Change)))
              (append front (append (list (list Var Change)) (cdr Environment)))
           )
       )
      (changeAtI Var Change (cdr Environment) (if (= (length front) 0)
                                                  (list (car Environment))
                                                  (append front (list (car Environment)))
                                               ) (- i 1))
   )
)

(define (applyOp Value ArithExpr ArithExpr2 Envornment)
  (cond
    [(if (equal? Value '+) (+ (applyArithExpr ArithExpr Envornment) (applyArithExpr ArithExpr2 Envornment)) #f)]
    [(if (equal? Value '-) (- (applyArithExpr ArithExpr Envornment) (applyArithExpr ArithExpr2 Envornment)) #f)]
    [(if (equal? Value '*) (* (applyArithExpr ArithExpr Envornment) (applyArithExpr ArithExpr2 Envornment)) #f)]
    [(if (equal? Value '/) (/ (applyArithExpr ArithExpr Envornment) (applyArithExpr ArithExpr2 Envornment)) #f)]
  )
)

(define (applyArithExpr Value Environment)
  ;If is a list and of length 3
  ;Evaluate Op and other ArithExpr
  ;else
  ;Evaluate Num or Var
  (if (and (list? Value) (= (length Value) 3))
      (applyOp (car Value) (applyArithExpr (car (cdr Value)) Environment) (applyArithExpr (car (cdr (cdr Value))) Environment) Environment)
      (cond
        [(if (number? Value) Value #f)]
        [(if (symbol? Value) (car (cdr (getAtI Environment (findVar Value Environment 0)))) #f)]
        )
  )
)

(define (applyBcond Value ArithExpr ArithExpr2 Environment)
  (if (equal? (cond
    [(if (equal? Value 'gt) (> (applyArithExpr ArithExpr Environment) (applyArithExpr ArithExpr2 Environment)) #f)]
    [(if (equal? Value 'lt) (< (applyArithExpr ArithExpr Environment) (applyArithExpr ArithExpr2 Environment)) #f)]
    [(if (equal? Value 'eq) (equal? (applyArithExpr ArithExpr Environment) (applyArithExpr ArithExpr2 Environment)) #f)]
  ) #t) #t #f)
)

(define (applyCondExpr program Environment)
  (if (= (length program) 3)
      (if (equal? (cond
        [(if (equal? (car program) 'or) (or (applyCondExpr (car (cdr program)) Environment) (applyCondExpr (car (cdr (cdr program))) Environment)) #f)]
        [(if (equal? (car program) 'and) (and (applyCondExpr (car (cdr program)) Environment) (applyCondExpr (car (cdr (cdr program))) Environment)) #f)]
        [(if (equal? (car program) 'gt) (applyBcond (car program) (car (cdr program)) (car (cdr (cdr program))) Environment) #f)]
        [(if (equal? (car program) 'lt) (applyBcond (car program) (car (cdr program)) (car (cdr (cdr program))) Environment) #f)]
        [(if (equal? (car program) 'eq) (applyBcond (car program) (car (cdr program)) (car (cdr (cdr program))) Environment) #f)]
       ) #t) #t #f)
      (if (= (length program) 2)
          (not (applyCondExpr (car (cdr program)) Environment))
          #f
      )
   )
)

;--------------Changes the Environment---------------
(define (applyAssign Var ArithExpr Environment)
  ;Find Var in Environment
  ;Get the value for ArithExpr
  ;Change the value of Var to be ArithExpr
  (if (= (findVar Var Environment 0) -1)
      #f
      (changeAtI Var (applyArithExpr ArithExpr Environment) Environment '() (findVar Var Environment 0))
  )
)

(define (applyDecl Var Environment)
  (if (= (length Environment) 0) (list (list Var 0))
  (append (list(list Var 0)) Environment))
)

;--------------Changes the Scope------------------
(define (lengthCheck Environment originalLength)
  (if (= (length Environment) originalLength)
      Environment
      (removeI Environment (- (length Environment) originalLength))
   )
)

(define (applyIf program Environment originalLength)
  (if (= (length program) 0)
      (lengthCheck Environment originalLength)
      (if (applyCondExpr (car (cdr program)) Environment)
          (lengthCheck (sem (car (cdr (cdr program))) Environment) originalLength)
          (lengthCheck Environment originalLength)
      )
  )
)

(define (applyWhile program Environment originalLength)
  (if (= (length program) 0)
      (lengthCheck Environment originalLength)
      (if (applyCondExpr (car (cdr program)) Environment)
          (applyWhile program (sem (car (cdr (cdr program))) Environment) originalLength)
          (lengthCheck Environment originalLength)
       )
   )
)

;----------------Sem-----------------
(define (sem program Environment)
  ;Figure out if Decl | Assign | If | While
  ;Apply the needed function and call sem again
  ;If none of the above, return the Environment
  (if (> (length program) 0)
      (if (equal? (car (car program)) 'if)
          (sem (cdr program) (applyIf (car program) Environment (length Environment)))
          (if (equal? (car (car program)) 'decl)
              (sem (cdr program) (applyDecl (car (cdr (car program))) Environment))
              (if (equal? (car (car program)) 'assign)
                  (sem (cdr program) (applyAssign (car (cdr (car program))) (car (cdr (cdr (car program)))) Environment))
                  (if (equal? (car (car program)) 'while)
                      (sem (cdr program) (applyWhile (car program) Environment (length Environment)))
                      Environment
                   )
               )
           )
       )
      Environment
   )
)