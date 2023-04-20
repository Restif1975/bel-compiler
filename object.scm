(define-module (object)
  #:use-module (oop goops)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:export (Program Constant
		    Variable Local-Variable Global-Variable Predefined-Variable
		    Reference Local-Reference Global-Reference
		    Predefined-Reference Free-Reference Dynamic-Reference
		    Alternative
		    Sequence
		    Local-Assignment Global-Assignment
		    Function Flat-Function Continuation Flat-Continuation
		    Application Regular-Application Continuation-Application Predefined-Application
		    Arguments No-Argument
		    Box-Creation Box-Read Box-Write
		    Free-Environment No-Free
		    Fix-Let Dynamic-Let
		    Functional-Description
		    g.current g.init find-variable?
		    number-of extract
		    Sexp->object objectify
		    insert-box! lift! convert! cpsify letify
		    pp ppo
		    convert2arguments
		    arguments->CPS
		    ->Sexp Variable Local-Variable Pseudo-Variable
		    class-slots-names
		    objectify-variables-list
		    objectify-complex-fn objectify-complex-args objectify-complex-args1
		    ->Sexp variable->Sexp
		    sify ->S Call/cc annotation annotate cl-set! non-trivial? trivial?
		    convert2Regular-Application convert2Continuation-Application new-Variable
		    convert2toplevel-sequence
		    el-global-name el-global-name1
		    Condition-Case))

(define (static-error msg . culprits)
  (display `(*static-error* ,msg . ,culprits))
  (newline)
  '())

(define-class Variable (<object>)
  (name #:init-keyword #:name))

(define-class Global-Variable (Variable)
  (function? #:init-keyword #:function?))

(define-class Predefined-Variable (Variable)
  (description #:init-keyword #:description))

(define-class Local-Variable (Variable)
  (mutable? #:init-keyword #:mutable?)
  (dotted? #:init-keyword #:dotted?))

(define-class Functional-Description (<object>)
  (comparator #:init-keyword #:comparator)
  (arity #:init-keyword #:arity)
  (generator #:init-keyword #:generator))

(define-class Constant-Description (<object>)  ; unused
  (value #:init-keyword #:value))

(define-class Program (<object>))

(define-class Reference (Program)
  (variable #:init-keyword #:variable))

(define-class Local-Reference (Reference))

(define-class Global-Reference (Reference))

(define-class Predefined-Reference (Reference))

(define-class Local-Assignment (Program)
  (reference #:init-keyword #:reference)
  (form #:init-keyword #:form))

(define-class Global-Assignment (Program)
  (variable #:init-keyword #:variable)
  (form #:init-keyword #:form))

(define-class Box-Read (Program)
  (reference #:init-keyword #:reference))

(define-class Box-Write (Program)
  (reference #:init-keyword #:reference)
  (form #:init-keyword #:form))

(define-class Box-Creation (Program)
  (reference #:init-keyword #:reference))

(define-class Function (Program)
  (variables #:init-keyword #:variables)
  (body #:init-keyword #:body))

(define-class Alternative (Program)
  (condition #:init-keyword #:condition)
  (consequent #:init-keyword #:consequent)
  (alternant #:init-keyword #:alternant))

(define-class Sequence (Program)
  (first #:init-keyword #:first)
  (last #:init-keyword #:last))

(define-class Top-Level-Sequence (Sequence))

(define-class Constant (Program)
  (value #:init-keyword #:value))

(define-class Application (Program))

(define-class Regular-Application (Application)
  (function #:init-keyword #:function)
  (arguments #:init-keyword #:arguments))

(define-class Continuation-Application (Regular-Application))

(define-class Predefined-Application (Application)
  (variable #:init-keyword #:variable)
  (arguments #:init-keyword #:arguments))

(define-class Fix-Let (Program)
  (variables #:init-keyword #:variables)
  (arguments #:init-keyword #:arguments)
  (body #:init-keyword #:body))

(define-class Arguments (Program)
  (first #:init-keyword #:first)
  (others #:init-keyword #:others))

(define-class No-Argument (Program))

(define-class Flat-Function (Function)
  (free #:init-keyword #:free))

(define-class Free-Environment (Program)
  (first #:init-keyword #:first)
  (last #:init-keyword #:last))

(define-class No-Free (Program))

(define-class Free-Reference (Reference)
  (index #:init-keyword #:index))

(define-class Dynamic-Let (Program)    ; TODO (?) add to lift!
  (variable #:init-keyword #:variable)
  (argument #:init-keyword #:argument)
  (body #:init-keyword #:body))

(define-class Dynamic-Reference (Reference))

(define-class Continuation (Function))

(define-class Flat-Continuation (Continuation)
  (free #:init-keyword #:free))

(define-class Pseudo-Variable (Local-Variable))

(define-class Call/cc (Regular-Application))  ; now unused

(define-class Condition-Case (Program)
  (variable #:init-keyword #:variable)
  (exp #:init-keyword #:exp)
  (fail #:init-keyword #:fail)
  (ok #:init-keyword #:ok))

(define (objectify e r d)
  (if (atom? e)
      (if (symbol? e)
	(objectify-reference e r d)
	(objectify-quotation e r d))
      (case (car e)
	((quote) (objectify-quotation (cadr e) r d))
	((if) (objectify-alternative (cdr e) r d))
	((do) (objectify-sequence (cdr e) r d))
	((assign) (objectify-assignment (cadr e) (caddr e) r d))
	((fn) (objectify-function (cadr e) (cddr e) r d))
	((dynamic-let)
	 (objectify-dynamic-binding (cadr e) (caddr e) (cdddr e) r d))
	((dynamic)
	 (objectify-dynamic-reference (cadr e) r d))
	((eif) (objectify-eif (cdr e) r d))
	(else (objectify-application (car e) (cdr e) r d)))))

(define (Sexp->object e)
  (objectify e '() '()))

(define (objectify-quotation value r d)
  (make Constant #:value value))

(define (objectify-reference name r d)
  (let ((v (or (find-variable? name r)
	       (find-variable? name g.current)
	       (find-variable? name g.init))))
    (if v
	(cond
	   ((is-a? v Local-Variable)
	    (make Local-Reference
	      #:variable v))  ; second pass -> Box-Read
	   ((is-a? v Global-Variable)
	    (make Global-Reference
	      #:variable v))
	   ((is-a? v Predefined-Variable)
	    (make Predefined-Reference
	      #:variable v))
	   (else
	    (objectify-free-global-reference name r d)))
	(objectify-free-global-reference name r d))))

;; Bel style if
(define (objectify-alternative e* r d)
  (let ((ec (car e*))
	(et (cadr e*))
	(rest (cddr e*)))
    (make Alternative
      #:condition (objectify ec r d)
      #:consequent (objectify et r d)
      #:alternant (if (pair? rest)
		       (if (pair? (cdr rest))
			   (objectify-alternative rest r d)
			   (objectify (car rest) r d))
		       (make Predefined-Reference
			 #:variable (find-variable? 'nil g.init))))))

(define (objectify-sequence e* r d)
  (if (pair? e*)
      (if (pair? (cdr e*))
	  (make Sequence
	    #:first (objectify (car e*) r d)
	    #:last (objectify-sequence (cdr e*) r d))
	  (objectify (car e*) r d))
      (make Constant #:value 42)))

(define (objectify-application f e* r d)
  (let ((ff (objectify f r d))
	(ee* (convert2arguments (map (lambda (e) (objectify e r d))
				     e*))))
    (cond
     ((is-a? ff Function)
      (process-closed-application ff ee*))
     ((is-a? ff Predefined-Reference)
      (let* ((fvf (slot-ref ff 'variable))
	     (desc (slot-ref fvf 'description)))
	(if (is-a? desc Functional-Description)
	    (if ((slot-ref desc 'comparator)
		 (length e*) (slot-ref desc 'arity))
		(make Predefined-Application #:variable fvf #:arguments ee*)
		(static-error "Incorrect predefined arity" f e*))
	    (make Predefined-Application #:variable fvf #:arguments ee*))))
     (else (make Regular-Application #:function ff #:arguments ee*)))))

(define (process-closed-application f e*)
  (let ((v* (slot-ref f 'variables))
	(b (slot-ref f 'body)))
    (if (and (pair? v*) (slot-ref (car (last-pair v*)) 'dotted?))
	(process-nary-closed-application f e*)
	(if (= (number-of e*) (length v*))
	    (make Fix-Let
	      #:variables v*
	      #:arguments e*
	      #:body b)
	    (static-error "Incorrect regular arity" f e*)))))

(define (process-nary-closed-application f e*)
  (let* ((v* (slot-ref f 'variables))
	(b (slot-ref f 'body))
	(o (make Fix-Let
	      #:variables v*
	      #:arguments (let gather ((v* v*)
				       (e* e*))
			    (if (slot-ref (car v*) 'dotted?)
				(make Arguments
				  #:first (make Predefined-Application
					    #:variable (find-variable? 'list g.init)
					    #:arguments (make Arguments
							  #:first (slot-ref e* 'first)
							  #:others (slot-ref e* 'others)))
				  #:others (make No-Argument))
				(if (pair? v*)
				    (make Arguments
				      #:first (slot-ref e* 'first)
				      #:others (gather (cdr v*) (slot-ref e* 'others)))
				    (static-error "Incorrect dotted arity" f e*))))
	      #:body b)))
    (slot-set! (car (last-pair v*)) 'dotted? #f)
    o))

(define (convert2arguments e*)
  (if (pair? e*)
      (make Arguments
	#:first (car e*)
	#:others (convert2arguments (cdr e*)))
      (make No-Argument)))

(define (convert2free e*)
  (if (pair? e*)
      (make Free-Environment
	#:first (car e*)
	#:last (convert2free (cdr e*)))
      (make No-Free)))

(define-method (number-of (o Arguments))
  (+ 1 (number-of (slot-ref o 'others))))

(define-method (number-of (o No-Argument)) 0)

(define-method (number-of (o Free-Environment))
  (+ 1 (number-of (slot-ref o 'last))))

(define-method (number-of (o No-Free)) 0)

(define (objectify-assignment n e r d)
  (let ((ov (objectify n r d))
	(of (objectify e r d)))
    (cond ((is-a? ov Local-Reference)
	   (slot-set! (slot-ref ov 'variable) 'mutable? #t)
	   (make Box-Write
	     #:reference ov
	     #:form of))  ; initially Local-Assignment
	  ((is-a? ov Global-Reference)
	   (slot-set! (slot-ref ov 'variable) 'function? (is-a? of Function))
	   (make Global-Assignment
	     #:variable (slot-ref ov 'variable)
	     #:form of))
	  (else
	   (static-error "Illegal mutated reference" n)))))

(define (objectify-function args body r d)
  (if (complex-args? args)
      (objectify-complex-fn args body r d)
      (let* ((vars (objectify-variables-list args))
	     (b (objectify-sequence body (r-extend r vars) d)))
	(make Function
	  #:variables vars
	  #:body (boxify-mutable-variables b vars)))))

(define (objectify-complex-fn pattern body r d)
  (let* ((gv (make Local-Variable
	       #:name (gensym "v")
	       #:mutable? #f
	       #:dotted? #t))
	 (bindings (objectify-complex-args pattern gv r d))
	 (vars (map car bindings))
	 (b (objectify-sequence body (r-extend r vars) d)))
    (make Function
      #:variables (list gv)
      #:body (bind-complex-args vars
			       (map cadr bindings)
			       (boxify-mutable-variables b vars)))))

(define (objectify-complex-args pattern var r d)
  (objectify-complex-args1 pattern
			   (make Local-Reference #:variable var)
			   r
			   d
			   identity))

(define (objectify-complex-args1 pattern access r d k)
  (cond ((null? pattern)
	 (k '()))
	((symbol? pattern)
	 (k (list (list (make Local-Variable
			  #:name pattern
			  #:mutable? #f
			  #:dotted? #f)
			access))))
	((pair? pattern)
	 (if (optional? (car pattern))
	     (objectify-complex-args1
	      (cadar pattern)
	      (make Alternative
		#:condition
		(make Predefined-Application
		  #:variable (find-variable? 'pair g.init)
		  #:arguments (make Arguments
				#:first access
				#:others (make No-Argument)))
		#:consequent
		(make Predefined-Application
		  #:variable (find-variable? 'car g.init)
		  #:arguments (make Arguments
				#:first access
				#:others (make No-Argument)))
		#:alternant
		(if (pair? (cddar pattern))
		    (objectify (caddar pattern) r d)  ; default expr
		    (make Constant #:value 'nil)))
	      r
	      d
	      (lambda (a)
		(objectify-complex-args1
		 (cdr pattern)
		 (make Predefined-Application
		   #:variable (find-variable? 'cdr g.init)
		   #:arguments (make Arguments
				 #:first access
				 #:others (make No-Argument)))
		 (r-extend r (map car a))
		 d
		 (lambda (b)
		   (k (append a b))))))
	     (objectify-complex-args1
	      (car pattern)
	      (make Predefined-Application
		#:variable (find-variable? 'car g.init)
		#:arguments (make Arguments
			      #:first access
			      #:others (make No-Argument)))
	      r
	      d
	      (lambda (a)
		(objectify-complex-args1 
		 (cdr pattern)
		 (make Predefined-Application
		   #:variable (find-variable? 'cdr g.init)
		   #:arguments (make Arguments
				 #:first access
				 #:others (make No-Argument)))
		 (r-extend r (map car a))
		 d
		 (lambda (b)
		   (k (append a b))))))))
	(else
	 (static-error "Invalid argument list" pattern))))

(define (bind-complex-args vars access body)
  (if (pair? vars)
      (make Fix-Let
	#:variables (list (car vars))
	#:arguments (make Arguments
		      #:first (car access)
		      #:others (make No-Argument))
	#:body (bind-complex-args (cdr vars) (cdr access) body))
      body))

(define (optional? arg)
  (and (pair? arg) (eq? (car arg) 'o)))

(define (complex-args? args)
  (cond ((null? args) #f)
	((symbol? args) #f)
	((and (pair? args) (symbol? (car args)))
	 (complex-args? (cdr args)))
	(else #t)))

(define (objectify-variables-list names)
  (if (pair? names)
      (cons (make Local-Variable
	      #:name (car names)
	      #:mutable? #f
	      #:dotted? #f)
	    (objectify-variables-list (cdr names)))
      (if (symbol? names)
	  (list (make Local-Variable
		  #:name names
		  #:mutable? #f
		  #:dotted? #t))
	  '())))

;; Now unused
(define (objectify-ccc e r d)
  (make Call/cc
    #:function (make Global-Reference
		 #:variable (find-variable? '_ccc g.current))
    #:arguments (make Arguments
		      #:first (objectify e r d)
		      #:others (make No-Argument))))

(define (objectify-dynamic-binding name e body r d)
  (let ((v (make Local-Variable
		 #:name name
		 #:mutable? #f
		 #:dotted? #f)))
    (make Dynamic-Let
      #:variable v
      #:argument (make Arguments
		   #:first (objectify e r d)
		   #:others (make No-Argument))
      #:body (objectify-sequence body r (r-extend d (list v))))))

(define (objectify-dynamic-reference name r d)
  (let ((v (find-variable? name d)))
    (if v
	(make Dynamic-Reference
	  #:variable v)
	(static-error "Warning: dynamic variable not found" name))))

;; (eif x (car 'a) 'oops x)
(define (objectify-eif e* r d)
  (let ((v (make Local-Variable
	     #:name (car e*)
	     #:mutable? #f
	     #:dotted? #f)))
    (make Condition-Case
      #:variable v
      #:exp (objectify (cadr e*) r d)
      #:fail (objectify (caddr e*) (r-extend r (list v)) d)
      #:ok (objectify (cadddr e*) (r-extend r (list v)) d))))

(define (boxify-mutable-variables form variables)
  (if (pair? variables)
      (if (slot-ref (car variables) 'mutable?)
          (make Sequence
            #:first (make Box-Creation
		      #:reference (make Local-Reference
				    #:variable (car variables)))
            #:last (boxify-mutable-variables form (cdr variables)))
          (boxify-mutable-variables form (cdr variables)))
      form))

(define (objectify-free-global-reference name r d)
  (let ((v (make Global-Variable
	     #:name name
	     #:function? #f)))
    (insert-global! v)
    (make Global-Reference #:variable v)))

(define (r-extend r vars)
  (append vars r))

(define (insert-global! variable)
  (set! g.current (cons variable g.current)))

(define (local-variable? name r)
  (if (pair? r)
      (if (eq? name (slot-ref (caar r) 'name))
	  (car r)
	  (local-variable? name (cdr r)))
      #f))

(define (find-variable? name env)
  (if (pair? env)
      (if (eq? name (slot-ref (car env) 'name))
	  (car env)
	  (find-variable? name (cdr env)))
      #f))

(define g.current '())

(define g.init '())

;; Code walker

(define (update-walk! g o . args)
  (for-each (lambda (field)
	      (let ((vf (slot-ref o field)))
		(when (is-a? vf Program)
		  (let ((v (if (null? args)
			       (g vf)
			       (apply g vf args))))
		    (slot-set! o field v)))))
	    (class-slots-names (class-of o)))
  o)

;; Second pass: boxes

(define-method (insert-box! (o Program))
  (update-walk! insert-box! o))

(define-method (insert-box! (o Local-Reference))
  (if (slot-ref (slot-ref o 'variable) 'mutable?)
      (make Box-Read #:reference o)
      o))

;; Now done in objectify (first pass)
(define-method (insert-box! (o Local-Assignment))
  (make Box-Write
    #:reference (slot-ref o 'reference)
    #:form (insert-box! (slot-ref o 'form))))

(define-method (insert-box! (o Box-Creation))
  o)

(define-method (insert-box! (o Box-Write))
  (slot-set! o 'form (insert-box! (slot-ref o 'form)))
  o)

;; Third pass: free variables

(define (lift! o)
  (lift-procedures! o #f '()))

(define-method (lift-procedures! (o Program) flatfun vars)
  (update-walk! lift-procedures! o flatfun vars))

(define-method (lift-procedures! (o Local-Reference) flatfun vars)
  (let ((v (slot-ref o 'variable)))
    (if (memq v vars)
	o
	(begin
	  (adjoin-free-variable! flatfun o)
	  (make Free-Reference #:variable v #:index #f)))))

(define (adjoin-free-variable! flatfun ref)
  (when (or (is-a? flatfun Flat-Function)
	    (is-a? flatfun Flat-Continuation))
    (let check ((free* (slot-ref flatfun 'free)))
      (if (is-a? free* Free-Environment)
	  (unless (eq? (slot-ref ref 'variable)
		       (slot-ref (slot-ref free* 'first) 'variable))
	    (check (slot-ref free* 'last)))
	  (slot-set! flatfun 'free
		     (make Free-Environment
		       #:first ref
		       #:last (slot-ref flatfun 'free)))))))

(define-method (lift-procedures! (o Fix-Let) flatfun vars)
  (slot-set! o 'arguments
	     (lift-procedures! (slot-ref o 'arguments) flatfun vars))
  (let ((newvars (append (slot-ref o 'variables) vars)))
    (slot-set! o 'body
	       (lift-procedures! (slot-ref o 'body) flatfun newvars))
    o))

(define-method (lift-procedures! (o Condition-Case) flatfun vars)
  (slot-set! o 'exp
	     (lift-procedures! (slot-ref o 'exp) flatfun vars))
  (let ((newvars (cons (slot-ref o 'variable) vars)))
    (slot-set! o 'fail
	       (lift-procedures! (slot-ref o 'fail) flatfun newvars))
    (slot-set! o 'ok
	       (lift-procedures! (slot-ref o 'ok) flatfun newvars))
    o))

(define-method (lift-procedures! (o Function) flatfun vars)
  (let* ((localvars (slot-ref o 'variables))
	 (body (slot-ref o 'body))
	 (newfun (make Flat-Function
		  #:variables localvars
		  #:body body
		  #:free (make No-Free))))
    (slot-set! newfun 'body
	       (lift-procedures! body newfun localvars))  ; free local references are collected
    (let ((free* (slot-ref newfun 'free)))
      (slot-set! newfun 'free
		 (lift-procedures! free* flatfun vars))     ; a free ref may be free in parent abstraction
      newfun)))

(define-method (lift-procedures! (o Continuation) flatfun vars)
  (let* ((localvars (slot-ref o 'variables))
	 (body (slot-ref o 'body))
	 (newfun (make Flat-Continuation
		  #:variables localvars
		  #:body body
		  #:free (make No-Free))))
    (slot-set! newfun 'body
	       (lift-procedures! body newfun localvars))
    (let ((free* (slot-ref newfun 'free)))
      (slot-set! newfun 'free
		 (lift-procedures! free* flatfun vars))
      newfun)))

;; Build the free variables environment

(define (convert! o)
  (closure-convert! o #f '()))

(define-method (closure-convert! (o Program) flatfun env)
  (update-walk! closure-convert! o flatfun env))

(define-method (closure-convert! (o Free-Reference) flatfun env)
  (when flatfun
    (let ((v (slot-ref o 'variable))
	  (free* (slot-ref flatfun 'free)))
      (slot-set! o 'index (lookup v free* 0))
      o)))

(define (lookup variable env i)
  (when (is-a? env Free-Environment)
    (if (eq? variable (slot-ref (slot-ref env 'first) 'variable))
	i
	(lookup variable (slot-ref env 'last) (+ i 1)))))

(define-method (closure-convert! (o Fix-Let) flatfun env)
  (slot-set! o 'arguments
	     (closure-convert! (slot-ref o 'arguments) flatfun env))
  (slot-set! o 'body
	     (closure-convert! (slot-ref o 'body)
			       flatfun
			       (append env (slot-ref o 'variables))))
  o)

(define-method (closure-convert! (o Condition-Case) flatfun env)
  (slot-set! o 'exp
	     (closure-convert! (slot-ref o 'exp) flatfun env))
  (slot-set! o 'fail
	     (lift-procedures! (slot-ref o 'fail)
			       flatfun
			       (append env (list (slot-ref o 'variable)))))
  (slot-set! o 'ok
	     (lift-procedures! (slot-ref o 'ok)
			       flatfun
			       (append env (list (slot-ref o 'variable)))))
  o)

(define-method (closure-convert! (o Flat-Function) flatfun env)
  (let ((variables (slot-ref o 'variables))
	(body (slot-ref o 'body)))
    (slot-set! o 'free
	       (convert2free (intersect (extract (slot-ref o 'free)) env)))  ; sort free refs in stack order
    (slot-set! o 'body
	       (closure-convert! body o (append env variables)))
    (let ((free* (slot-ref o 'free)))
      (slot-set! o 'free
		 (closure-convert! free* flatfun env)))  ; free refs may be free in parent abstraction
    o))

(define-method (closure-convert! (o Flat-Continuation) flatfun env)
  (let ((variables (slot-ref o 'variables))
	(body (slot-ref o 'body)))
    (slot-set! o 'free
	       (convert2free (intersect (extract (slot-ref o 'free)) env)))
    (slot-set! o 'body
	       (closure-convert! body o (append env variables)))
    (let ((free* (slot-ref o 'free)))
      (slot-set! o 'free
		 (closure-convert! free* flatfun env)))
    o))

(define (intersect freevars env)
  (if (pair? env)
      (if (pair? freevars)
	  (let ((tmp (find (lambda (ref)
			     (eq? (slot-ref ref 'variable) (car env)))
			   freevars)))
	    (if tmp
		(cons tmp (intersect (delq tmp freevars) (cdr env)))
		(intersect freevars (cdr env))))
	  '())
      '()))

(define-method (extract (o Free-Environment))
  (cons (slot-ref o 'first)
	    (extract (slot-ref o 'last))))

(define-method (extract (o No-Free))
  '())

(define (make-lexenv vars)
  (let build ((vars vars)
	      (i 0))
    (if (pair? vars)
	(cons (cons (car vars) i)
	      (build (cdr vars) (+ i 1)))
	'())))

(define (make-free-env free* env)
  (make-lexenv (intersect (extract free*) env)))

;;; CPS conversion

(define (cpsify e)
  (let ((v (new-Variable)))
    (->CPS e
	   (make Continuation
	     #:variables (list v)
	     #:body (make Local-Reference #:variable v))
	   #t)))

(define new-Variable
  (let ((counter 0))
    (lambda ()
      (set! counter (+ 1 counter))
      (make Pseudo-Variable
	#:name counter
	#:mutable? #f
	#:dotted? #f))))

(define (convert2Regular-Application f . args)
  (make Regular-Application
    #:function f
    #:arguments (convert2arguments args)))

(define (convert2Continuation-Application k . args)
  (make Continuation-Application
    #:function k
    #:arguments (convert2arguments args)))

;; Intermediate continuations will be reduced to let forms in letify
;; Only the initial Continuation will remain in tail position
(define (convert2Application tail? k . args)
  (if tail?
      (apply convert2Continuation-Application k args)
      (apply convert2Regular-Application k args)))

;; Default transformation
(define-method (->CPS (e Program) k tail?)
  (convert2Application tail? k e))

;; Local-Assignment
(define-method (->CPS (e Box-Write) k tail?)
  (->CPS (slot-ref e 'form)
	 (let ((v (new-Variable)))
	   (make Continuation
	     #:variables (list v)
	     #:body (convert2Application
		     tail?
		     k
		     (make Box-Write
		       #:reference (slot-ref e 'reference)
		       #:form (make Local-Reference #:variable v)))))
	 #f))

(define-method (->CPS (e Global-Assignment) k tail?)
  (->CPS (slot-ref e 'form)
	 (let ((v (new-Variable)))
	   (make Continuation
	     #:variables (list v)
	     #:body (convert2Application
		     tail?
		     k
		     (make Global-Assignment
		       #:variable (slot-ref e 'variable)
		       #:form (make Local-Reference #:variable v)))))
	 #f))

(define-method (->CPS (e Alternative) k tail?)
  (->CPS (slot-ref e 'condition)
	 (let ((v (new-Variable)))
	   (make Continuation
	     #:variables (list v)
	     #:body (make Alternative
		      #:condition (make Local-Reference #:variable v)
		      #:consequent (->CPS (slot-ref e 'consequent) k tail?)
		      #:alternant (->CPS (slot-ref e 'alternant) k tail?))))
	 #f))

(define-method (->CPS (e Sequence) k tail?)  ; a Sequence is always binary
  (->CPS (slot-ref e 'first)
	 (let ((v (new-Variable)))
	   (make Continuation
	     #:variables (list v)
	     #:body (->CPS (slot-ref e 'last) k tail?)))
	 #f))

;; (+ e1 e2)
;; (->CPS e1 (lambda (a)
;;             (->CPS e2 (lambda (b)
;;                         (k (+ a b))))))

(define-method (->CPS (e Predefined-Application) k tail?)
  (let* ((args (slot-ref e 'arguments))
	 (vars (let name ((args args))
		(if (is-a? args Arguments)
		    (cons (new-Variable)
			  (name (slot-ref args 'others)))
		    '())))
	 (appl (convert2Application tail? k (make Predefined-Application
						  #:variable (slot-ref e 'variable)
						  #:arguments (convert2arguments
							       (map (lambda (v)
								      (make Local-Reference
									#:variable v))
								    vars))))))
    (arguments->CPS args vars appl)))

;; A regular function is now (fn (k ...))
(define-method (->CPS (e Regular-Application) k tail?)
  (let* ((fun (slot-ref e 'function))
	 (args (slot-ref e 'arguments))
	 (varfun (new-Variable))
	 (vars (let name ((args args))
		(if (is-a? args Arguments)
		    (cons (new-Variable)
			  (name (slot-ref args 'others)))
		    '())))
	 (appl (make Regular-Application
		 #:function (make Local-Reference #:variable varfun)
		 #:arguments (make Arguments
			       #:first k
			       #:others (convert2arguments
					 (map (lambda (v)
						(make Local-Reference
						  #:variable v))
					      vars))))))
    (->CPS fun
	   (make Continuation
		 #:variables (list varfun)
		 #:body (arguments->CPS args vars appl))
	   #f)))

(define (arguments->CPS args vars appl)
  (if (pair? vars)
      (->CPS (slot-ref args 'first)
		(make Continuation
		  #:variables (list (car vars))
		  #:body (arguments->CPS (slot-ref args 'others)
					 (cdr vars)
					 appl))
		#f)
      appl))

;; Now unused
(define-method (->CPS (e Call/cc) k tail?)
  (let ((fun (slot-ref (slot-ref e 'arguments) 'first))
	(varfun (new-Variable)))
    (->CPS fun
	   (make Continuation
	     #:variables (list varfun)
	     #:body (make Regular-Application
		      #:function (make Local-Reference #:variable varfun)
		      #:arguments (convert2arguments
				   (list k
					 (let ((current_k (new-Variable))
					       (v (new-Variable)))
					   (make Function  ; reified continuation
					     #:variables (list current_k v)
					     #:body (convert2Continuation-Application
						     k
						     (make Local-Reference
						       #:variable v))))))))
	   #f)))

(define-method (->CPS (e Fix-Let) k tail?)
  (let ((fun (make Function
	       #:variables (slot-ref e 'variables)
	       #:body (slot-ref e 'body))))
    (->CPS (make Regular-Application
	     #:function fun
	     #:arguments (slot-ref e 'arguments))
	   k
	   tail?)))

(define-method (->CPS (e Dynamic-Let) k tail?)
  (convert2Application tail? k
		       (make Dynamic-Let
			 #:variable (slot-ref e 'variable)
			 #:argument (slot-ref e 'argument)
			 #:body (->CPS (slot-ref e 'body) k #t))))

(define-method (->CPS (e Dynamic-Let) k tail?)
  (->CPS (slot-ref e 'argument)
	 (let ((v (new-Variable)))
	   (make Continuation
	     #:variables (list v)
	     #:body (make Dynamic-Let
		      #:variable (slot-ref e 'variable)
		      #:argument (make Local-Reference #:variable v)
		      #:body (->CPS (slot-ref e 'body)
				    k
				    tail?))))
	 #f))

(define-method (->CPS (e Function) k tail?)
  (convert2Application tail? k
		       (let ((cont (new-Variable)))
			 (make Function
			   #:variables (cons cont (slot-ref e 'variables))
			   #:body (->CPS (slot-ref e 'body)
					 (make Local-Reference #:variable cont)
					 #t)))))

;;; Selective CPS conversion

;; T = trivial
;; N = non-trivial (ccc)

(define (sify e)
  ;; A regular Sequence will be converted to cps
  ;; if one of the subexpressions is non-trivial
  ;; To avoid transforming a whole file to cps
  ;; a Top-Level-Sequence will not be annotated
  ;; and the subexpressions will remain independent
  (when (is-a? e Sequence)
    (set! e (convert2toplevel-sequence e)))
  (annotate e)
  (->S e))

(define annotation (make-object-property))

(define (convert2toplevel-sequence e)
  (if (is-a? e Sequence)
      (make Top-Level-Sequence
	#:first (convert2toplevel-sequence (slot-ref e 'first))
	#:last (convert2toplevel-sequence (slot-ref e 'last)))
      e))

(define-syntax cl-set!
  (syntax-rules ()
    ((cl-set! variable form)
     (let ((a form))
       (set! variable a)
       a))))

(define-syntax in
  (syntax-rules ()
    ((in e) #f)
    ((in e a) (equal? e a))
    ((in e a b ...) (or (equal? e a) (in e b ...)))))

;; Where it all starts
(insert-global! (make Global-Variable
		  #:name 'ccc
		  #:function? #t))

(set! (annotation (find-variable? 'ccc g.current)) 'N)

(define-method (->S (e Program))
  (if (eq? (annotation e) 'N)
      (letify (cpsify e) '())
      e))

(define-method (->S (e Top-Level-Sequence))
  (slot-set! e 'first (->S (slot-ref e 'first)))
  (slot-set! e 'last (->S (slot-ref e 'last)))
  e)

;; Currently unused
(define-method (annotate (e Call/cc))
  (annotate (slot-ref e 'arguments))
  (cl-set! (annotation e) 'N))

(define-method (annotate (e Global-Reference))
  (cl-set! (annotation e)
	   (or (annotation (slot-ref e 'variable)) 'T)))

(define-method (annotate (e Program))
  (cl-set! (annotation e) 'T))

(define-method (annotate (e Function))
  (cl-set! (annotation e)
	   (annotate (slot-ref e 'body))))

(define-method (annotate (e Regular-Application))
  (let ((a1 (annotate (slot-ref e 'function)))
	(a2 (annotate (slot-ref e 'arguments))))
    (cl-set! (annotation e)
	     (if (in 'N a1 a2) 'N 'T))))

(define-method (annotate (e Predefined-Application))
  (let ((a (annotate (slot-ref e 'arguments))))
    (cl-set! (annotation e) a)))

(define-method (annotate (e Fix-Let))
  (let ((a1 (annotate (slot-ref e 'arguments)))
	(a2 (annotate (slot-ref e 'body))))
    (cl-set! (annotation e)
	     (if (in 'N a1 a2) 'N 'T))))

(define-method (annotate (e Arguments))
  (let ((a1 (annotate (slot-ref e 'first)))
	(a2 (annotate (slot-ref e 'others))))
    (cl-set! (annotation e)
	     (if (in 'N a1 a2) 'N 'T))))

(define-method (annotate (e No-Argument))
  (cl-set! (annotation e) 'T))

(define-method (annotate (e Alternative))
  (let ((a1 (annotate (slot-ref e 'condition)))
	(a2 (annotate (slot-ref e 'consequent)))
	(a3 (annotate (slot-ref e 'alternant))))
    (cl-set! (annotation e)
	     (if (in 'N a1 a2 a3) 'N 'T))))

(define-method (annotate (e Top-Level-Sequence))
  (annotate (slot-ref e 'first))
  (annotate (slot-ref e 'last)))

(define-method (annotate (e Sequence))
  (let ((a1 (annotate (slot-ref e 'first)))
	(a2 (annotate (slot-ref e 'last))))
    (cl-set! (annotation e)
	     (if (in 'N a1 a2) 'N 'T))))

;; Assignment to a non-trivial expression is noted in the variable
;; So future references to the variable will also be annotated non-trivial
(define-method (annotate (e Global-Assignment))
  (let ((a (annotate (slot-ref e 'form))))
    (cl-set! (annotation (slot-ref e 'variable)) a)
    (cl-set! (annotation e) a)))

(define-method (annotate (e Box-Write))  ; aka Local-Assignment
  (cl-set! (annotation e)
	   (annotate (slot-ref e 'form))))

;; Letify
;; Since let forms are destructured during CPS, try to reintroduce them.
;; Since CPS also duplicates parts of the AST, copy it completely
;; to remove those sharing (C. Queinnec).

(define-method (shallow-clone (o Pseudo-Variable))
  (new-Variable))

(define-method (letify (o Program) env)
  (update-walk! letify (shallow-clone o) env))

(define-method (letify (o Function) env)
  (let* ((vars (slot-ref o 'variables))
	 (body (slot-ref o 'body))
	 (new-vars (map shallow-clone vars)))
    (make Function
      #:variables new-vars
      #:body (letify body (append (map cons vars new-vars) env)))))

(define-method (letify (o Continuation) env)
  (let* ((vars (slot-ref o 'variables))
	 (body (slot-ref o 'body))
	 (new-vars (map shallow-clone vars)))
    (make Continuation
      #:variables new-vars
      #:body (letify body (append (map cons vars new-vars) env)))))

(define-method (letify (o Condition-Case) env)
  (let* ((var (slot-ref o 'variable))
	 (new-var (shallow-clone var))
	 (env2 (cons (cons var new-var) env)))
    (make Condition-Case
      #:variable new-var
      #:exp (letify (slot-ref o 'exp) env)
      #:fail (letify (slot-ref o 'fail) env2)
      #:ok (letify (slot-ref o 'ok) env2))))

(define-method (letify (o Local-Reference) env)
  (let* ((v (slot-ref o 'variable))
	 (r (assq v env)))
    (make Local-Reference #:variable (cdr r))))

;; (define-method (letify (o Dynamic-Reference) env)
;;   (let* ((v (slot-ref o 'variable))
;; 	 (r (assq v env)))
;;     (make Dynamic-Reference #:variable (cdr r))))

(define-method (letify (o Regular-Application) env)
  (if (is-a? (slot-ref o 'function) Function)
      (letify (process-closed-application (slot-ref o 'function) (slot-ref o 'arguments))
	      env)
      (make Regular-Application
	#:function (letify (slot-ref o 'function) env)
	#:arguments (letify (slot-ref o 'arguments) env))))

(define-method (letify (o Continuation-Application) env)
  (make Continuation-Application
	#:function (letify (slot-ref o 'function) env)
	#:arguments (letify (slot-ref o 'arguments) env)))

(define-method (letify (o Fix-Let) env)
  (let* ((vars (slot-ref o 'variables))
	 (body (slot-ref o 'body))
	 (new-vars (map shallow-clone vars)))
    (make Fix-Let
      #:variables new-vars
      #:arguments (letify (slot-ref o 'arguments) env)
      #:body (letify body (append (map cons vars new-vars) env)))))

(define-method (letify (o Dynamic-Let) env)
  (let* ((var (slot-ref o 'variable))
	 (body (slot-ref o 'body))
	 (new-var (shallow-clone var)))
    (make Dynamic-Let
      #:variable new-var
      #:argument (letify (slot-ref o 'argument) env)
      #:body (letify body (cons (cons var new-var) env)))))

(define-method (letify (o Box-Creation) env)
  (make Box-Creation
    #:reference (letify (slot-ref o 'reference) env)))

;; Interpreter

(define-method (->Sexp e)
  "UFO")

(define-method (->Sexp (e Variable))
  (variable->Sexp e))

(define-method (->Sexp (e Constant))
  `(quote ,(slot-ref e 'value)))

(define-method (variable->Sexp (e Variable))
  (slot-ref e 'name))

(define-method (variable->Sexp (e Pseudo-Variable))
  (symbolify "v" (slot-ref e 'name)))

(define-method (->Sexp (e Reference))
  (variable->Sexp (slot-ref e 'variable)))

(define-method (->Sexp (e Free-Environment))
  (cons (->Sexp (slot-ref e 'first))
	(->Sexp (slot-ref e 'last))))

(define-method (->Sexp (e No-Free))
  '())

(define-method (->Sexp (e Sequence))
  `(begin ,(->Sexp (slot-ref e 'first))
	  ,(->Sexp (slot-ref e 'last))))

(define-method (->Sexp (e Alternative))
  `(if ,(->Sexp (slot-ref e 'condition))
       ,(->Sexp (slot-ref e 'consequent))
       ,(->Sexp (slot-ref e 'alternant))))

(define-method (->Sexp (e Global-Assignment))
  `(set! ,(variable->Sexp (slot-ref e 'variable)) ,(->Sexp (slot-ref e 'form))))

(define-method (->Sexp (e Box-Write))
  `(box-set! ,(variable->Sexp (slot-ref (slot-ref e 'reference) 'variable))
	     ,(->Sexp (slot-ref e 'form))))

(define-method (->Sexp (e Box-Read))
  `(box-ref ,(variable->Sexp (slot-ref (slot-ref e 'reference) 'variable))))

(define-method (->Sexp (e Box-Creation))
  `(make-box ,(variable->Sexp (slot-ref (slot-ref e 'reference) 'variable))))

(define-method (->Sexp (e Function))
  `(lambda ,(map variable->Sexp (slot-ref e 'variables))
     ,(->Sexp (slot-ref e 'body))))

(define-method (->Sexp (e Flat-Function))
  `(lambda ,(map variable->Sexp (slot-ref e 'variables))
     (*free-variables* . ,(->Sexp (slot-ref e 'free)))
     ,(->Sexp (slot-ref e 'body))))

(define-method (->Sexp (e Flat-Continuation))
  `(lambda ,(map variable->Sexp (slot-ref e 'variables))
     (*free-variables* . ,(->Sexp (slot-ref e 'free)))
     ,(->Sexp (slot-ref e 'body))))

(define-method (->Sexp (e Fix-Let))
  `(let ,(map (lambda (vars args)
		`(,vars ,args))
	      (map variable->Sexp (slot-ref e 'variables))
	      (->Sexp (slot-ref e 'arguments)))
     ,(->Sexp (slot-ref e 'body))))

(define-method (->Sexp (e Dynamic-Let))
  `(dyn ,(variable->Sexp (slot-ref e 'variable))
	,(->Sexp (slot-ref e 'argument))
	,(->Sexp (slot-ref e 'body))))

(define-method (->Sexp (e Dynamic-Reference))
  (variable->Sexp (slot-ref e 'variable)))

(define-method (->Sexp (e Arguments))
  (cons (->Sexp (slot-ref e 'first))
	(->Sexp (slot-ref e 'others))))

(define-method (->Sexp (e No-Argument))
  '())

(define-method (->Sexp (e Regular-Application))
  `(,(->Sexp (slot-ref e 'function))
    . ,(->Sexp (slot-ref e 'arguments))))

(define-method (->Sexp (e Continuation-Application))
  `(invoke ,(->Sexp (slot-ref e 'function))
	   . ,(->Sexp (slot-ref e 'arguments))))

(define-method (->Sexp (e Predefined-Application))
  `(,(variable->Sexp (slot-ref e 'variable))
    . ,(->Sexp (slot-ref e 'arguments))))

(define-method (->Sexp (e Call/cc))
  `(ccc . ,(->Sexp (slot-ref e 'arguments))))

(define (symbolify . names)
  (string->symbol
   (apply string-append (map (lambda (n)
			       (cond ((symbol? n) (symbol->string n))
				     ((number? n) (number->string n))
				     ((string? n) n)
				     (else (format #f "~A" n))))
			     names))))


;;; Utilities

(define (atom? x)
  (not (pair? x)))

(define (class-slots-names class)
  (map slot-definition-name (class-slots class)))

(define (pp e)
  (pretty-print (->Sexp e)))

(define (read-file filename)
  (call-with-input-file filename
    (lambda (in)
      (let gather ((e (read in))
                   (content '()))
        (if (eof-object? e)
            (reverse content)
            (gather (read in) (cons e content)))))))

(define (ppo e)
  (pretty-print (pp-obj e)))

(define-method (pp-obj (e Program))
  (cons (class-name (class-of e))
	 (map pp-obj (map (lambda (f)
			       (slot-ref e f))
			     (class-slots-names (class-of e))))))

(define-method (pp-obj (e Variable))
  (slot-ref e 'name))

(define-method (pp-obj (e Pseudo-Variable))
  (string->symbol
   (string-append "v" (number->string (slot-ref e 'name)))))

(define-method (pp-obj e)
  (if (pair? e)
      (map pp-obj e)
      e))

;;; Primitives

(define-syntax defprimitive
  (syntax-rules (>=0 >=1 >=2)
    ((defprimitive name elname 1)
     (let ((v (make Predefined-Variable
		#:name 'name
		#:description (make Functional-Description
				#:comparator =
				#:arity 1
				#:generator 'elname))))
       (set! g.init (cons v g.init))
       'name))
    ((defprimitive name elname 2)
     (let ((v (make Predefined-Variable
		#:name 'name
		#:description (make Functional-Description
				#:comparator =
				#:arity 2
				#:generator 'elname))))
       (set! g.init (cons v g.init))
       'name))
    ((defprimitive name elname >=0)
     (let ((v (make Predefined-Variable
		#:name 'name
		#:description (make Functional-Description
				#:comparator >=
				#:arity 0
				#:generator 'elname))))
       (set! g.init (cons v g.init))
       'name))
    ((defprimitive name elname >=1)
     (let ((v (make Predefined-Variable
		#:name 'name
		#:description (make Functional-Description
				#:comparator >=
				#:arity 1
				#:generator 'elname))))
       (set! g.init (cons v g.init))
       'name))
    ((defprimitive name elname >=2)
     (let ((v (make Predefined-Variable
		#:name 'name
		#:description (make Functional-Description
				#:comparator >=
				#:arity 2
				#:generator 'elname))))
       (set! g.init (cons v g.init))
       'name))))

(define-syntax definitial
  (syntax-rules ()
    ((definitial name val)
     (let ((v (make Predefined-Variable
		#:name 'name
		#:description 'val)))
       (set! g.init (cons v g.init))
       'name))))

(define-syntax defopcode
  (syntax-rules ()
    ((defopcode name op)
     (set-symbol-property! 'name 'byte-opcode 'op))))

(defprimitive id eq 2)
(defprimitive el-cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defprimitive cadr cadr 1)
(defprimitive cddr cddr 1)
(defprimitive caddr caddr 1)
(defprimitive xcar setcar  2)
(defprimitive xcdr setcdr 2)
(defprimitive pair consp 1)
(defprimitive symbol symbolp 1)
(defprimitive string stringp 1)
(defprimitive number numberp 1)
(defprimitive function functionp 1)
(defprimitive sym intern 1)
(defprimitive nom symbol-name 1)
(defprimitive rand random 1)

(defprimitive + + >=0)
(defprimitive - - >=0)
(defprimitive * * >=0)
(defprimitive / / >=1)
(defprimitive = equal 2)
(defprimitive > > 2)
(defprimitive >= >= 2)
(defprimitive < < 2)
(defprimitive <= <= 2)
(defprimitive list list >=0)
(defprimitive append append >=0)
(defprimitive print prin1 1)
(defprimitive prc write-char 1)  ; missing prnice princ

;;(defprimitive apply apply >=2)
;; apply has to be included in the library functions
;; If f is in cps:
;; (apply f l) -> (_apply k f l)
;; So that at runtime:
;; (_apply k f l) -> (apply_cps k f l) -> (apply f k l)
;; problem: losing the arity check at compile time for apply

(definitial t t)
(definitial nil nil)
(definitial \sp ?\s)
(definitial \lf ?\n)

(defopcode cons byte-cons)
(defopcode car byte-car)
(defopcode cdr byte-cdr)
(defopcode setcar byte-setcar)
(defopcode setcdr byte-setcdr)
(defopcode consp byte-consp)
(defopcode null byte-not)
(defopcode not byte-not)
(defopcode numberp byte-numberp)
(defopcode symbolp byte-symbolp)
(defopcode stringp byte-stringp)
(defopcode functionp byte-functionp)
(defopcode eq byte-eq)
(defopcode equal byte-equal)
(defopcode + byte-plus)
(defopcode - byte-diff)
(defopcode * byte-mult)
(defopcode / byte-quo)

;;; Library functions

;; To recompile "lib":
;; In comp.scm
;; comp -> letify cpsify for cps version, sify for direct version
;; meaning Global-Reference and meaning Global-Assignment
;; change el-global-name to el-global-name1
;; do not forget to update the wrappers in lib.el

(define (el-global-name1 s)
  (string->symbol (string-append (symbol->string s) "_plain")))  ; "_cps"

(define (el-global-name s)
  (string->symbol (string-append "_" (symbol->string s))))

(define (objectify-file filename)
  (for-each (lambda (e)
	      (Sexp->object e))
	    (read-file filename)))

;; Side-effect is to add library functions to the initial global environment g.current

(objectify-file "lib.ebel")

;; ccc was added above in the selective cps transformation

(insert-global! (make Global-Variable
		  #:name 'apply
		  #:function? #t))


