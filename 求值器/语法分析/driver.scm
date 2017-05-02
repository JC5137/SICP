(load "F:\\中医药图书学术影响力研究\\py\\求值器\\core.scm")
(load "F:\\中医药图书学术影响力研究\\py\\求值器\\DataStructure.scm")
(load "F:\\中医药图书学术影响力研究\\py\\求值器\\expression.scm")
(load "F:\\中医药图书学术影响力研究\\py\\求值器\\Parsing.scm")

(define true #t)
(define false #f)

(define primitive-procedure
    (list (list 'car car)
          (list 'cdr cdr)
          (list 'cons cons)
          (list 'null? null?)
          (list '+  +)
          ))

(define (primitive-procedure-names)
    (map car 
         primitive-procedure))
(define (primitive-procedure-objects)
    (map (lambda (proc) (list 'primitive (cadr proc)))
         primitive-procedure))


(define (setup-environment)
    (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
         (define-variable! 'true true initial-env)
         (define-variable! 'false false initial-env)
          initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
    (tagged-list? proc 'primitive))
(define (primitive-implementation proc)
    (cadr proc))

(define (apply_jc-primitive-procedure proc args)
    (apply_jc-in-underlying-scheme
        (primitive-implementation proc) args))
(define apply_jc-in-underlying-scheme apply)


(define input-prompt ";;; M-eval_jc input:" )
(define output-prompt ";;; M-eval_jc value:")

(define (driver-loop)
    (prompt-for-input input-prompt)
    (let ((input(read)))
      (let ((output (eval_jc input the-global-environment)))
           (announce-output output-prompt)
           (user-print output)))
     (driver-loop))

(define (prompt-for-input string)
    (newline) (newline) (display string) (newline))
(define (announce-output string)
    (newline) (display string) (newline))

(define (user-print object)
    (if (compound-procedure? object)
        (display (list 'compound-procedure
                       (procedure-parameters object)
                       (procedure-body object)
                       '<procedure-env>))
        (display object)))

(driver-loop)