(define (eval_jc exp env)
    ((analyze exp) env))

(define (analyze exp)
    (cond ((self-eval_jcuating? exp) 
            (analyze-self-evalating exp))
          ((variable? exp) (analyze-variable exp))
          ((quoted? exp) (analyze-quoted exp))
          ((assignment? exp) (analyze-assignment exp))
          ((definition? exp) (analyze-definition exp))
          ((if? exp) (analyze-if exp))
          ((lambda? exp) (analyze-lambda exp))
          ((begin? exp)
           (analyze-sequence (begin-actions exp)))
          ((cond? exp) (analyze (cond->if exp)))
          ((application? exp)
            (analyze-application exp))
          (else
           (error "Unknow expression type-- eval_jc" exp))))

(define (list-of-values exps env)
    (if (no-operands? exps)
        '()
        (cons (eval_jc (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))
(define (eval_jc-if exp env)
    (if (true? (eval_jc (if-predicate exp) env))
        (eval_jc (if-consequent exp) env)
        (eval_jc (if-alternative exp) env)))

(define (eval_jc-sequence exps env)
    (cond ((last-exp? exps) (eval_jc (first-exp exps) env))
          (else (eval_jc (first-exp exps) env)
                (eval_jc-sequence (rest-exps exps) env))))

(define (eval_jc-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval_jc (assignment-value exp) env)
                         env)
    'ok)

(define (eval_jc-definition exp env)
    (define-variable! (definition-variable exp)
                      (eval_jc (definition-value exp) env)
                      env)
    'ok)







