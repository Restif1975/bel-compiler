(define-module (expand)
  #:export (*macro*
	    destructure
	    hug zip
	    expand
	    expand-once
	    expand-set*
	    expand-set
	    macroexpand-file
	    setforms
	    *setter*)  ; debug
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 match))  ; defset

(define *macro* '())

(define (add-macro-definition! form)
  (set! *macro* (cons form *macro*)))

(define (get-expander n)
  (let ((tmp (assq n *macro*)))
    (and tmp (cdr tmp))))

(define (expand e)
  (initial-expander e initial-expander))

(define (initial-expander e m)
  (let ((m1 (cond
	     ((symbol? e)
	      identifier-expander)
	     ((not (pair? e))
	      (lambda (e m) e))
	     ((symbol? (car e))
	      (or (get-expander (car e))
		  application-expander))
	     (else
	      application-expander))))
    (m1 e m)))

(define (install-expander keyword function)
  (add-macro-definition! (cons keyword function)))

(define identifier-expander
  (lambda (e m) e))

(define application-expander
  (lambda (e m)
    (map (lambda (e) (m e m)) e)))

(define (expand-once e)
  (initial-expander e (lambda (e m) e)))

(install-expander 'quote
		  (lambda (e m) e))

(install-expander 'fn
		  (lambda (e m)
		    `(fn ,(cadr e)
		       ,@(map (lambda (e) (m e m)) (cddr e)))))

(install-expander 'mac
		  (lambda (e m)
		    (let ((name (cadr e))
			  (pattern (caddr e))
			  (body (cadddr e)))
		      (install-expander name
					(lambda (e m)
					  (m (apply
					      (eval `(lambda vals
						       (let ,(destructure pattern 'vals '())
							 ,body))
						    (interaction-environment))
					      (cdr e))
					     m)))
		      name)))

;; (expand '(push x (car foo)))
;; (set (car foo) (cons x (car foo)))
;; (setcar foo (cons x (car foo))
;; (setter (cons x getter))
;; should be
;; (let* ((v foo)) (setcar v (cons x (car v))))

;;(defmacro push (x place) (with-once-only (place) `(setf ,place (cons ,x ,place))))

;; Beware multiple evaluation in
;; (push x (nth (incf i) foo))
;; (setcar (nthcdr (incf i) foo) (cons x (nth (incf i) foo)))

(define *setter* '())

(define-macro (defset name parms . body)
  (let ((ge (gensym "g")))
    `(set! *setter*
	   (cons (cons ',name
		       (lambda (,ge)         ; (car foo)
			 "evaluate body with elements in parms bound to elements in (cdr ge)"
			 (match (cdr ,ge)
				(,parms
				 ,@body))))  ; ((g1347 foo) (car g1347) (fn (v) (xcar g1347 v)))
		*setter*))))

(defset car (x)
  (let ((g (gensym "g")))
    (list (list g x)
	  `(car ,g)
	  (lambda (v) `(xcar ,g ,v)))))

(defset cdr (x)
  (let ((g (gensym "g")))
    (list (list g x)
	  `(cdr ,g)
	  (lambda (v) `(xcdr ,g ,v)))))

(defset caar (x)
  (let ((g (gensym "g")))
    (list (list g x)
	  `(caar ,g)
	  (lambda (v) `(xcar (car ,g) ,v)))))

(defset cadr (x)
  (let ((g (gensym "g")))
    (list (list g x)
	  `(cadr ,g)
	  (lambda (v) `(xcar (cdr ,g) ,v)))))

(defset cddr (x)
  (let ((g (gensym "g")))
    (list (list g x)
	  `(cddr ,g)
	  (lambda (v) `(xcdr (cdr ,g) ,v)))))

(defset nth (n xs)
  (let ((g (gensym "g"))
	(h (gensym "g")))
    (list (list g n h xs)
	  `(nth ,g ,h)
	  (lambda (v) `(xcar (nthcdr ,g ,h) ,v)))))

;; Experimental
(defset if (ec et ef)
  (match (setforms et)
	 ((vars get setter)
	  (match (setforms ef)
		 ((vars2 get2 setter2)
		  (list (append vars vars2)
			`(if ,ec ,get ,get2)
			(lambda (v) `(if ,ec ,(setter v) ,(setter2 v)))))))))

(define (setforms e)
  (let ((ee (expand e)))
    (if (symbol? ee)
	(list '()
		ee
		(lambda (v) `(assign ,ee ,v)))
	(let ((tmp (assq (car ee) *setter*)))
	  (if tmp
	      ((cdr tmp) ee)
	      '())))))

(define (hug l f)
  (if (pair? l)
      (if (pair? (cdr l))
	  (cons (f (car l) (cadr l)) (hug (cddr l) f))
	  (list (f (car l))))
      '()))

(define (zip . l*)
  (apply map list l*))

(define (destructure pattern access bindings)
  (cond ((null? pattern)
	 bindings)
	((symbol? pattern)
	 (cons `(,pattern ,access) bindings))
	((pair? pattern)
	 (if (optional? (car pattern))
	     (destructure (cadar pattern)
			  `(if (pair? ,access)
			       (car ,access)
			       ,(if (pair? (cddar pattern))
				    (caddar pattern)
				    'nil))
			  bindings)
	     (destructure (car pattern)
			  `(car ,access)
			  (destructure (cdr pattern) `(cdr ,access) bindings))))
	(else
	 bindings)))

(define (optional? arg)
  (and (pair? arg) (eq? (car arg) 'o)))

(define (initialise-macro)
  (for-each (lambda (e)
	      (expand e))
	    (read-file "mac.scm")))

(define (read-file filename)
  (call-with-input-file filename
    (lambda (in)
      (let gather ((e (read in))
                   (content '()))
        (if (eof-object? e)
            (reverse content)
            (gather (read in) (cons e content)))))))

(define (macroexpand-file filename)
  (let* ((complete-filename (string-append filename ".bel"))
	 (out-filename (string-append filename ".ebel"))
	 (code (read-file complete-filename)))
    (call-with-output-file out-filename
    (lambda (out)
      (for-each (lambda (e)
		  (let ((exp (expand e)))
		    (pretty-print exp out)
		    (newline out)))
		code)))))

(initialise-macro)


