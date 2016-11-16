;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Simple interpreter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;
;;; EECS-111 Advanced Section Exercise 1
;;; A simple interpreter
;;;

(define (eval exp environment)
  (cond [(symbol? exp)
         (lookup exp
                 environment)]
        [(list? exp)
         (eval-complex-expression exp environment)]
        [else
         exp]))

(define (eval-cond clauses environment)
  (if (or (eq? 'else (first (first clauses)))
          (eval (first (first clauses))
                environment)) 
      (eval (second (first clauses)) environment)
      (eval-cond (rest clauses) environment)))

(define (eval-complex-expression exp environment)
  (cond
    [(eq? 'if (first exp))
     (if(eval (second exp) environment)
        (eval (third exp) environment)
        (eval (fourth exp) environment))]
    [(eq? 'cond (first exp))
     (eval-cond (second exp) environment)
     ]
    [(eq? 'local (first exp)) (eval (third exp)
                                       (append
                                        (map (λ(d)
                                               (list (second d)
                                                     (eval (third d )
                                                           environment)))
                                             (second exp))
                                        environment))]
    [else (eval-procedure-call (map (λ (subexp)
                                      (eval subexp
                                            environment))
                                    exp))]))

(define (eval-procedure-call proc-and-args)
  (apply (first proc-and-args)
         (rest proc-and-args)))

(define (lookup variable-name
                environment)
  (local [(define binding
            (assoc variable-name environment))]
    (if (list? binding)
        (second binding)
        (error "Undefined variable"
               variable-name))))

(define default-environment
  (list (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'equal? equal?)
        (list '< <)
        (list '> >)
        (list 'eq? eq?)
        (list '= =)
        ))

(define (definition->binding definition)
  (if (not (eq?
            (first definition)
            'define))
      (error "Invalid definition"
             definition)
      (list (second definition)
            (eval
             (third definition)
             default-environment))))

(define (program definitions)
  (append (map definition->binding
               definitions)
          default-environment))

(check-expect (eval '(+ n (* a b))
                    (program '((define n
                                 7)
                               (define a 4)
                               (define b 3))))
              19)

(check-expect (eval '(if (= 1 0)
1
0)
(list (list '= =)))
0)

(check-expect (eval '(if (= 1 1)
1
0)
(list (list '= =)))
1)

(check-expect (eval '(cond [(= n 0)
0]
[(= n 1)
1]
[else
2])
(list (list 'n 0)
(list '= =)))
0)

(check-expect (eval '(cond [(= n 0)
0]
[(= n 1)
1]
[else
2])
(list (list 'n 1)
(list '= =)))
1)

(check-expect (eval '(cond [(= n 0)
0] 
[(= n 1)
1]
[else
2])
(list (list 'n -1)
(list '= =)))
2)
 

(check-expect (eval '(+ 2
(local [(define b (* 2 a))]
(* b 2))
a)
(program
'((define a 2))))
12)

(check-expect (eval '(+ 2
(local [(define b (* a a))]
(local [(define c (* b 2))]
(+ c 1)))
a)
(program
'((define a 2))))
13)