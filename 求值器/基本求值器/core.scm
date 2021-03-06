(define (eval_jc exp env)
    (cond ((self-eval_jcuating? exp) exp)
          ((variable? exp) (lookup-variable-value exp env))
          ((quoted? exp) (text-of-quotation exp))
          ((assignment? exp) (eval_jc-assignment exp env))
          ((definition? exp) (eval_jc-definition exp env))
          ((if? exp) (eval_jc-if exp env))
          ((lambda? exp)
           (make-procedure (lambda-parameters exp)
                           (lambda-body exp)
                           env))
          ((begin? exp)
           (eval_jc-sequence (begin-actions exp) env))
          ((cond? exp) (eval_jc (cond->if exp) env))
          ((application? exp)
           (apply_jc (eval_jc (operator exp) env)
                  (list-of-values (operands exp) env)))
          (else
           (error "Unknow expression type-- eval_jc" exp))))

(define (apply_jc procedure arguments)
    (cond ((primitive-procedure? procedure)
           (apply_jc-primitive-procedure procedure arguments))
          ((compound-procedure? procedure)
           (eval_jc-sequence
              (procedure-body procedure)
              (extend-environment
                (procedure-parameters procedure)
                arguments
                (procedure-environment procedure))))
          (else 
           (error "Unknow procedure type -- apply_jc" procedure))))

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







