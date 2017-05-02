(define (analyze-self-evalating exp)
    (lambda (env) exp))
(define (analyze-quoted exp)
    (let ((qval (text-of-quotation exp)))
        (lambda (env) qval)))
(define (analyze-variable exp)
    (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
    (let ((var (assignment-variable exp))
          (vproc (analyze (assignment-value exp))))
          (lambda (env) 
            (set-variable-value var (vproc env) env)
            'ok)))
(define (analyze-definition exp)
    (let ((var (definition-variable exp))
         (vproc (analyze (definition-value exp))))
      (lambda (env)
        (define-variable! var  (vproc env) env)
        'ok)))

(define (analyze-if exp)
    (let ((pproc (analyze (if-predicate exp)))
          (cproc (analyze (if-consequent exp)))
          (aproc (analyze (if-alternatice exp))))
        (lambda (env)
            (if (true? (pproc env))
                (cproc env)
                (aproc env)))))

(define (analyze-lambda exp)
    (let ((vars (lambda-parameters exp))
          (bproc(analyze-sequence (lambda-body exp))))
          (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
    (define (sequentially proc1 proc2)
        (lambda (env) (proc1 env) (proc2 env)))
    (define (loop first-proc rest-procs)
        (if (null? rest-procs)
            first-proc
            (loop (sequentially first-proc (car rest-procs))
                  (cdr rest-procs))))
    (let ((procs (map analyze exps)))
        (if (null? procs)
            (error "Empty sequence --analyze"))
    (loop (car procs) (cdr procs))))


(define (analyze-application exp)
    (let ((fproc (analyze (operator exp)))
           (aprocs (map analyze (operands exp))))
         (lambda (env)
            (execute-application (fproc env)
                                 (map (lambda (aproc) (aproc env))
                                      aprocs)))))

(define (execute-application proc args)
    (cond ((primitive-procedure? proc)
       (apply_jc-primitive-procedure proc args))
       ((compound-procedure? proc)
        ((procedure-body proc)
         (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else 
          (error 
            "Unknow procedure type --execute application"
            proc))))
    
    
    
    
    
    
    
    
    
    


