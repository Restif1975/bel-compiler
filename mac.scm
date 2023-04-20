(use-modules (expand))

(mac def (name args . body)
     `(assign ,name
	   (fn ,args ,@body)))

(mac let (var val . body)
     `((fn (,var) ,@body) ,val))

(mac with (bindings . body)
     (let ((ps (hug bindings list)))
       `((fn ,(map car ps)
	     ,@body)
	 ,@(map cadr ps))))

(mac withs (bindings . body)
     (if (pair? bindings)
	 `(let ,(car bindings) ,(cadr bindings)
	       (withs ,(cddr bindings) ,@body))
	 `(do ,@body)))

(mac or args
     (let ((g (gensym "g")))
       (if (pair? args)
           `(let ,g ,(car args)
		 (if ,g ,g (or ,@(cdr args))))
           'nil)))

(mac and args
     (let ((g (gensym "g")))
       (if (pair? args)
	   (if (pair? (cdr args))
               `(let ,g ,(car args)
		     (if ,g (and ,@(cdr args)) nil))
	       (car args))
           't)))

(mac aif (e . rest)
     `(let it ,e
	(if it
            ,@(if (and (pair? (cdr rest)) (pair? (cddr rest)))
		  `(,(car rest) (aif ,@(cdr rest)))
		  rest))))

(mac when (e . body)
     `(if ,e (do ,@body)))

(mac case (x . args)
     (if (pair? args)
	 (if (null? (cdr args))
	     (car args)
	     (let ((g (gensym "g")))
	       `(let ,g ,x
		     (if (= ,g ',(car args))
			 ,(cadr args)
			 (case ,g ,@(cddr args))))))
	 'nil))

(mac do1 args
     (let ((g (gensym "g")))
       `(let ,g ,(car args)
	     ,@(cdr args)
	     ,g)))

(mac pop (l)
  `(car
    (do1 ,l
	 (set ,l (cdr ,l)))))

(mac rfn (name parms . body)
  `(let ,name nil
	(set ,name (fn ,parms ,@body))))

(mac loop (var init update test . body)
     (let ((v (gensym "v")))
       `((rfn ,v (,var)
	      (when ,test ,@body (,v ,update)))
	 ,init)))

(mac for (var init max . body)
     (let ((vi (gensym "v"))
	   (vm (gensym "v")))
       `(with (,vi ,init ,vm ,max)
	      (loop ,var ,vi (+ ,var 1) (<= ,var ,vm)
		    ,@body))))

(mac repeat (n . body)
     (let ((v (gensym "v")))
       `(for ,v 1 ,n ,@body)))

(mac n-of (n expr)
     (let ((ga (gensym "g")))
       `(let ,ga nil
	     (repeat ,n (push ,expr ,ga))
	     (rev ,ga))))

(mac set args
     (if (pair? args)
	 `(set2 ,@args)
	 'nil))

(mac set2 (place val . args)
     (match (setforms place)
	    ((vars get setter)
	     (let ((g (gensym "g")))
	       `(do
		 (withs ,(append vars (list g val))
		   ,(setter g))
		 ,@(if (pair? args) `((set2 ,@args)) '()))))))

(mac unless (e . body)
     `(when (no ,e) ,@body))

;; eif is defined as a special form in object.scm
(mac onerr (e1 e2)
     (let ((v (gensym "v")))
       `(eif ,v ,e2 ,e1 ,v)))

(mac safe (e)
     `(onerr nil ,e))

(mac catch body
     (let ((v (gensym "v")))
       `(ccc (fn (,v) (dynamic-let throw ,v ,@body)))))

(mac throw (v)
     `((dynamic throw) ,v))

(mac zap (op place . args)
     (match (setforms place)
	    ((vars get setter)
	     (let ((gop (gensym "g"))
		   (gargs (map (lambda (_) (gensym "g")) args)))
	       `(withs ,(apply append vars (list gop op) (zip gargs args))
		       ,(setter `(,gop ,get ,@gargs)))))))

(mac push (x place)
     (let ((v (gensym "v")))
       `(let ,v ,x
	     (zap (fn (_) (cons ,v _)) ,place))))

(mac pull (x place (o f '=))
     (let ((v (gensym "v")))
       `(let ,v ,x
	     (zap (fn (_) (rem ,v _ ,f)) ,place))))

(mac ++ (place (o i 1))
     `(zap + ,place ,i))

(mac -- (place (o i 1))
     `(zap - ,place ,i))

(mac pushnew (x place (o f '=))
     (let ((v (gensym "v")))
       `(let ,v ,x
	     (zap (fn (_) (adjoin ,v _ ,f)) ,place))))



