#lang racket

(define the-empty-stream '() )

(define (stream-null? stream)
    (if (null? stream)
        true
        false
    )
)

(define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1) )
    )
)
(define (stream-map proc s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (proc (stream-car s))
                     (stream-map proc (stream-cdr s))
        )
    )
)
(define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin (proc stream-car s)
               (stream-for-each proc (stream-cdr s))
        )
    )
)


(define (memo-proc proc)
        (let ( (already-run? false) (result false) )
            (lambda ()
                (if (not already-run?)
                    (begin (set! result (proc))
                           (set! already-run? true)
                           result
                    )
                    result
                )
            )
        )
)
(define-syntax delay
  (syntax-rules ()
    ((_ exp) (memo-proc (lambda () exp) ) )))

(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))

(define-syntax-rule (force delayed-object)
  (delayed-object))

(define (stream-car stream)   (car stream) )
(define (stream-cdr stream)   (force (cdr stream)) )

(define (stream-filter pred stream)
    (cond ( (stream-null? stream) the-empty-stream)
          ( (pred (stream-car stream))
            (cons-stream (stream-car stream)
                         (stream-filter pred (stream-cdr stream) ) )
          ) 
          (else 
            (stream-filter pred (stream-cdr stream))
          )
    )
)

(define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (cons-stream 
         low
         (stream-enumerate-interval (+ low 1) high)
        )
    )
)



(define (integers-starting-from n)
    (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0) )

(define no-sevens
    (stream-filter (lambda (x) (not (divisible? x 7)))
                    integers))


(define (sieve stream)
    (cons-stream
     (stream-car stream)
     (sieve (stream-filter 
             (lambda (x)
                (not (divisible? x (stream-car stream))))
                (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))
(stream-ref primes 50)