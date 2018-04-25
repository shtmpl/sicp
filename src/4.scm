;; 1
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; Left -> Right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-evaluated (eval (first-operand exps) env)))
        (cons first-evaluated
              (list-of-values (rest-operands exps) env)))))

; Left <- Right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest-evaluated (list-of-values (rest-operands exps) env)))
           (cons (eval (first-operand exps) env)
                 rest-evaluated))))


;; 2
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ; reoredered
        ((application? exp) (apply (eval (operator exp) env) (list-of-values (operands exp) env)))

        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else (error "Unknown expression type -- EVAL" exp))))

;; a
; We check if an expression represents a procedure application as follows:
(define (application? exp) (pair? exp))

; Reordering the clause for procedure application so that it appears before the clauses for special forms (assignment, definition, etc),
; would cause special forms to be evaluated as procedure applications.

;; b
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

; (call <operator> <operand> ... <operand>)
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))


;; 3
(define (exp-type exp) (car exp))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else ((get 'eval (exp-type exp)) exp env))))


;; 4
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (and? exp) (tagged-list? exp 'and))
(define (and-exps exp) (cdr exp))
(define (no-and-exps? seq) (null? seq))
(define (last-and-exp? seq) (null? (cdr seq)))
(define (first-and-exp seq) (car seq))
(define (rest-and-exps seq) (cdr seq))

(define (eval-and exps env)
  (cond ((no-and-exps? exps) 'true)
        ((last-and-exp? exps)
         (let ((result (eval (first-and-exp exps) env)))
           (if (true? result)
               result
               'false)))
        (else (if (true? (eval (first-and-exp exps) env))
                  (eval-and (rest-and-exps exps) env)
                  'false))))

(define (expand-and-exps exps)
  (if (null? exps)
      'true
      (if (null? (cdr exps))
          (make-if (car exps) (car exps) 'false)
          (make-if (car exps) 'true (expand-and-exps (cdr exps))))))

(define (and->if exp)
  (expand-and-exps (and-exps exp)))


(define (or? exp) (tagged-list? exp 'or))
(define (or-exps exp) (cdr exp))
(define (no-or-exps? seq) (null? seq))
(define (last-or-exp? seq) (null? (cdr seq)))
(define (first-or-exp seq) (car seq))
(define (rest-or-exps seq) (cdr seq))

(define (eval-or exps env)
  (cond ((no-or-exps? exps) 'false)
        (else (let ((result (eval (first-or-exp exps) env)))
                (if (true? result)
                    result
                    (eval-or (rest-or-exps env)))))))

(define (expand-or-exps exps)
  (if (null? exps)
      'false
      (make-if (car exps) (car exps) (expand-or-exps (cdr exps)))))

(define (or->if exp)
  (expand-or-exps (or-exps exp)))


(define (eval exp env)
  (cond ...
        ((and? exp) (eval-and (and-exps exp) env))
        ; Alternatively
        ; ((and? exp) (eval (and->if exp) env))

        ((or? exp) (eval-or (or-exps exp) env))
        ; Alternatively
        ; ((or? exp) (eval (or->if exp) env))

        ...))


;; 5
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-procedure-clause? clause) (eq? (cadr clause) '=>))
(define (cond-procedure clause) (caddr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last -- COND->IF" clauses)))
              ((cond-procedure-clause? first)
               (make-if (cond-predicate first)
                        (list (cond-procedure first) (cond-predicate first))
                        (expand-clauses rest)))
              (else
               (make-if (cond-predicate first)
               (sequence->exp (cond-actions first))
               (expand-clauses rest)))))))


;; 6
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-binding-var binding) (car binding))
(define (let-binding-exp binding) (cadr binding))
(define (let-exps exp) (cddr exp))

(define (let->combination exp)
  (let ((vars (map let-binding-var (let-bindings exp)))
        (exps (map let-binding-exp (let-bindings exp))))
    (cons (make-lambda vars (let-exps exp)) exps)))

(define (eval exp env)
  (cond ...
        ((let? exp) (eval (let->combination exp) env))
        ...))


;; 7
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-binding-var binding) (car binding))
(define (let-binding-exp binding) (cadr binding))
(define (let-exps exp) (cddr exp))
(define (make-let-binding binding)
  (list binding))
(define (make-let bindings exps)
  (cons 'let (cons bindings exps)))

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-bindings exp) (cadr exp))
(define (first-let*-binding bindings) (car bindings))
(define (last-let*-binding? bindings) (null? (cdr bindings)))
(define (rest-let*-bindings bindings) (cdr bindings))
(define (let*-exps exp) (cddr exp))

(define (expand-let*-bindings bindings exps)
  (if (last-let*-binding? bindings)
      (make-let (make-let-binding (first-let*-binding bindings)) exps)
      (make-let (make-let-binding (first-let*-binding bindings))
                (list (expand-let*-bindings (rest-let*-bindings bindings) exps)))))

(define (let*->nested-lets exp)
  (expand-let*-bindings (let*-bindings exp) (let*-exps exp)))


;; 8
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-binding-var binding) (car binding))
(define (let-binding-exp binding) (cadr binding))
(define (let-exps exp) (cddr exp))
(define (named-let? exp) (not (null? (cdddr exp))))
(define (named-let-var exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-binding-var binding) (car binding))
(define (named-let-binding-exp binding) (cadr binding))
(define (named-let-exps exp) (cdddr exp))
(define (make-let bindings exps)
  (cons 'let (cons bindings exps)))
(define (make-let-binding var exp)
  (list (list var exp)))

(define (let->combination exp)
  (if (named-let? exp)
    (let ((vars (map named-let-binding-var (named-let-bindings exp)))
          (exps (map named-let-binding-exp (named-let-bindings exp))))
      (make-let (make-let-binding (named-let-var exp)
                                  (make-lambda vars (named-let-exps exp)))
                (list (cons (named-let-var exp) exps))))
    (let ((vars (map let-binding-var (let-bindings exp)))
          (exps (map let-binding-exp (let-bindings exp))))
      (cons (make-lambda vars (let-exps exp)) exps))))

(let->combination
 '(let factorial ((r 1)
                  (x 4))
   (if (= x 0) r (factorial (* r x) (- x 1)))))

(define Y (lambda (f)
            ((lambda (x) (f (x x)))
             (lambda (x) (f (x x))))))

(define F (lambda (f)
            (lambda (x)
              (if (< x 1) 1 (* x (f (- x 1)))))))
; FIXME


;; 11
(define (make-frame variables values) (cons '*frame* (map cons variables values)))
(define (frame-variables frame) (map car (cdr frame)))
(define (frame-values frame) (map cdr (cdr frame)))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

(define (frame-bindings frame) (cdr frame))
(define (first-binding bindings) (car bindings))
(define (rest-bindings bindings) (cdr bindings))

(define (empty-bindings? bindings) (null? bindings))
(define (binding-var binding) (car binding))
(define (set-binding! binding v) (set-cdr! binding v))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((empty-bindings? bindings) (env-loop (enclosing-environment env)))
            ((eq? (binding-var (first-binding bindings)) var) (set-binding! (first-binding bindings) val))
            (else (scan (rest-bindings bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-bindings frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan bindings)
      (cond ((empty-bindings? bindings) (add-binding-to-frame! var val frame))
            ((eq? (binding-var (first-binding bindings)) var) (set-binding! (first-binding bindings) val))
            (else (scan (rest-bindings bindings)))))
    (scan (frame-bindings frame))))


;; 25
(define (unless condition usual-value exceptional-value)
  (if condition
      exceptional-value
      usual-value))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

(factorial 5)
(unless (= 5 1) (* 5 (factorial (- 5 1))) 1)
(unless false (* 5 (factorial 4)) 1)
(unless false (* 5 (unless (= 4 1) (* 4 (factorial (- 4 1))) 1)) 1)
(unless false (* 5 (unless false (* 4 (factorial 3)) 1)) 1)
(unless false (* 5 (unless false (* 4 (unless (= 3 1) (* 3 (factorial (- 3 1))) 1)) 1)) 1)
(unless false (* 5 (unless false (* 4 (unless false (* 3 (factorial 2)) 1)) 1)) 1)
(unless false (* 5 (unless false (* 4 (unless false (* 3 (unless (= 2 1) (* 2 (factorial (- 2 1))) 1)) 1)) 1)) 1)
(unless false (* 5 (unless false (* 4 (unless false (* 3 (unless false (* 2 (factorial 1)) 1)) 1)) 1)) 1)
(unless false (* 5 (unless false (* 4 (unless false (* 3 (unless false (* 2 (unless (= 1 1) (* 1 (factorial (- 1 1))) 1)) 1)) 1)) 1)) 1)
(unless false (* 5 (unless false (* 4 (unless false (* 3 (unless false (* 2 (unless true (* 1 (factorial 0)) 1)) 1)) 1)) 1)) 1)
...


