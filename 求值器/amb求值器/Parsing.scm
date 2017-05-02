(define (analyze-self-evalating exp)
    (lambda (env succeed fail) (succeed exp fail)))

(define (analyze-quoted exp)
    (let ((qval (text-of-quotation exp)))
        (lambda (env succeed fail) (succeed qval fail))))

(define (analyze-variable exp)
    (lambda (env succeed fail) (succeed (lookup-variable-value exp env)
                               fail)))
(define (analyze-lambda exp)
    (let ((vars (lambda-parameters exp))
          (bproc(analyze-sequence (lambda-body exp))))
          (lambda (env succeed fail)
            (succeed (make-procedure vars bproc env)
                     fail))))

(define (analyze-if exp)
    (let ((pproc (analyze (if-predicate exp)))
          (cproc (analyze (if-consequent exp)))
          (aproc (analyze (if-alternatice exp))))
        (lambda (env succeed fail)
            (pproc env
                   (lambda (pred-value fail2)
                     (if (true? pred-value)
                         (cproc env succeed fail2)
                         (aproc env succeed fail2)))
                    fail))))
                    
(define (analyze-sequence exps)
    (define (sequentially a b)
        (lambda (env succeed fail) 
            (a env
               (lambda (a-value fail2)
                 (b env succeed fail2))
               fail)))
    (define (loop first-proc rest-procs)
        (if (null? rest-procs)
            first-proc
            (loop (sequentially first-proc (car rest-procs))
                  (cdr rest-procs))))
    (let ((procs (map analyze exps)))
        (if (null? procs)
            (error "Empty sequence --analyze"))
    (loop (car procs) (cdr procs))))


(define (analyze-definition exp)
    (let ((var (definition-variable exp))
         (vproc (analyze (definition-value exp))))
      (lambda (env succeed fail)
        (vproc env
               (lambda (val fail2)
                  (define-variable! var val env)
                  (succeed 'ok fail2))
               fail))))

(define (analyze-assignment exp)
    (let ((var (assignment-variable exp))
          (vproc (analyze (assignment-value exp))))
          (lambda (env succeed fail)
            (vproc env
                   (lambda (val fail2)
                     (let ((old-value
                           (lookup-variable-value var env)))
                     (set-variable-value! var val env)
                     (succeed 'ok 
                              (lambda ()
                                (set-variable-value! var
                                                     old-value
                                                     env)
                                (fail2)))))
                    fail))))

(define (analyze-application exp)
    (let ((fproc (analyze (operator exp)))
           (aprocs (map analyze (operands exp))))
         (lambda (env succeed fail)
            (fproc env
                   (lambda (proc fail2)
                     (get-args aprocs
                               env
                               (lambda (args fail3)
                                (execute-application
                                  proc args succeed fail3))
                               fail2))
                   fail))))
(define (get-args aprocs env succeed fail)
    (if (null? aprcs)
        (succeed '() fail)
        ((car aprocs) env
                      (lambda (arg fail2)
                        (get-args (cdr aprocs)
                                  env
                                  (lambda (args fail3)
                                    (succeed (cons arg args) 
                                             fail3))
                                  fail2))
                      fail)))

(define (execute-application proc args succeed)
    (cond ((primitive-procedure? proc)
            (succeed (apply_jc-primitive-procedure proc args)
                     fail))
       ((compound-procedure? proc)
        ((procedure-body proc)
         (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else 
          (error 
            "Unknow procedure type --execute application"
            proc))))

(define (analyze-amb exp)
    (let ((cprocs (map analyze (amb-choices exp))))
        (lambda (env succeed fail)
            (define (try-next choices)
                (if(null? choices)
                    (fail)
                    ((car choices) env
                                   succeed
                                   (lambda ()
                                     (try-next (cdr choices))))))
             (try-next cprocs))))
    
    
    
    
    
    
    
    
    
    


