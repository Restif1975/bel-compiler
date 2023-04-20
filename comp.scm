;;; Compiles Bel to Emacs Lisp byte-code

(define-module (comp)
  #:use-module (comp-init)
  #:use-module (object)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)  ; proper-list?
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:export (comp
	    comp-file
	    read-file))

(define (static-error msg . culprits)
  (display `(*static-error* ,msg . ,culprits))
  (newline)
  '())

(define (stack-adjustment op operand)
  (if (memq op '(byte-call byte-discardN byte-discardN-preserve-tos))
      (- operand)
      (let ((pair (assq op stack-effect)))
	(if (eq? (cdr pair) 'nil)
	    (- 1 operand)
	    (cdr pair)))))

(define *tag-number* 0)

(define (r-extend* r n* depth)
  (if (pair? n*)
      (cons (cons (car n*) depth)
	    (r-extend* r (cdr n*) (+ 1 depth)))
      r))

(define r.init '())           ; alist (var . pos)
(define stack.init '(0 . 0))  ; (depth . maxdepth)
(define const.init '())       ; alist (const . nil)
(define lap.init '())

(define (atom? e) (not (pair? e)))

(define (quote-if e)
  (cond
   ((symbol? e) `(quote ,e))
   ((atom? e) e)
   ((eq? (car e) 'make-byte-code) e)
   (else `(quote ,e))))

;;TODO for-effect #t
(define (meaning-out lap const stack for-effect reserved-csts output-type)
  (set! lap (reverse! (output lap 'byte-return 0)))
  (set! stack (stack-adjust stack 'byte-return 0))
  (set! const (reverse! const))
  (const-vector! const reserved-csts)
  (when (positive? reserved-csts)
    (set! const (let loop ((n 0))
		  (if (< n reserved-csts)
		      (cons (list (string->symbol (format #f "V~a" n)))
			    (loop (+ n 1)))
		      const))))
  (case output-type
    ((lap)
     lap)
    ((debug)
     (list lap (apply vector const) stack))
    ((bytecode)
     (list 'byte-code
	   `(apply 'unibyte-string ',(compile-lapcode lap))
	   `(apply 'vector (list ,@(map (lambda (pair)
					  (quote-if (car pair)))
					const)))
	   (cdr stack)))
    (else
     (error "Output type should be 'lap, 'debug or 'bytecode" output-type))))

;; (const . nil) -> (const . offset)
(define (const-vector! const reserved-csts)
  (let ((i reserved-csts))
    (for-each
     (lambda (cst)
       (set-cdr! cst i)
       (set! i (+ i 1)))
     const)))

(define (meaning-top-level e r k)
  (meaning e r lap.init
	       const.init
	       (stack-extend stack.init (length r))
	       #f
	       k))

;; Compile Bel code
(define* (comp e #:optional (out 'bytecode))
  (let* ((ee (sify (Sexp->object e)))  ; (letify (cpsify (Sexp->object e)) '())
	 (prg (convert! (lift! (insert-box! ee))))
	 (k.init (make-k.init out)))
    (meaning-top-level prg r.init k.init)))

(define (make-k.init out)
  (lambda (l c s d)
    (meaning-out l c s d 0 out)))

(define-method (meaning (e Constant) r lap const stack for-effect k)
  (let ((v (slot-ref e 'value)))
    (if for-effect
	(k lap const stack #f)
	(k (output lap
		   'byte-constant
		   (or (assv v const)
		       (begin
			 (set! const (cons (list v) const))
			 (car const))))
	   const
	   (stack-adjust stack 'byte-constant (list v))
	   for-effect))))

(define (meaning-constant v r lap const stack for-effect k)
  (meaning (make Constant #:value v) r lap const stack for-effect k))

(define-method (meaning (e Box-Creation) r lap const stack for-effect k)
  (meaning (slot-ref e 'reference) r lap const stack #f
	   (lambda (l c s d)
	     (set! l (output l 'byte-list1 0))
	     (set! s (stack-adjust s 'byte-list1 0))
	     (let* ((depth (car s))
		    (index (cdr (assq (slot-ref (slot-ref e 'reference) 'variable) r)))
		    (off (- depth (+ 1 index))))
	       (out-discard (output l 'byte-stack-set off)
			    c
			    (stack-adjust s 'byte-stack-set off)
			    #f
			    k)))))

(define-method (meaning (e Box-Read) r lap const stack for-effect k)
  (meaning (slot-ref e 'reference) r lap const stack #f
	   (lambda (l c s d)
	     (out-discard (output l 'byte-car 0)
			  c
			  (stack-adjust s 'byte-car 0)
			  for-effect
			  k))))

(define-method (meaning (e Box-Write) r lap const stack for-effect k)
  (meaning (slot-ref e 'reference) r lap const stack #f
	    (lambda (l c s d)
	      (meaning (slot-ref e 'form) r l c s d
		       (lambda (l c s d)
			 (out-discard (output l 'byte-setcar 0)
				      c
				      (stack-adjust s 'byte-setcar 0)
				      for-effect
				      k))))))

;; Useful?
(define-method (meaning (e Reference) r lap const stack for-effect k)
  (let* ((n (slot-ref (slot-ref e 'variable) 'name))
	 (tmp (assq n const)))
    (unless tmp
      (static-error "Warning: reference to free variable" n)
      (set! tmp (list n))
      (set! const (cons tmp const)))
    (out-discard (output lap 'byte-varref tmp)
		 const
		 (stack-adjust stack 'byte-varref tmp)
		 for-effect
		 k)))

(define-method (meaning (e Local-Reference) r lap const stack for-effect k)
  (let* ((v (slot-ref e 'variable))
	 (index (cdr (assq v r)))
	 (depth (car stack))
	 (off (- depth (+ 1 index))))
    (if (zero? off)
	(out-discard (output lap 'byte-dup 'nil)
		     const
		     (stack-adjust stack 'byte-dup 0)
		     for-effect
		     k)
	(out-discard (output lap 'byte-stack-ref off)
		     const
		     (stack-adjust stack 'byte-stack-ref off)
		     for-effect
		     k))))

(define-method (meaning (e Global-Reference) r lap const stack for-effect k)
  (let* ((variable (slot-ref e 'variable))
	 (name (el-global-name (slot-ref variable 'name))))
    (if (slot-ref variable 'function?)
	(meaning-constant name r lap const stack for-effect k)  ; (function name) equivalent
	(let ((tmp (assq name const)))
	  (unless tmp
	    (set! tmp (list name))
	    (set! const (cons tmp const)))
	  (out-discard (output lap 'byte-varref tmp)
		       const
		       (stack-adjust stack 'byte-varref tmp)
		       for-effect
		       k)))))

(define-method (meaning (e Predefined-Reference) r lap const stack for-effect k)
  (let* ((variable (slot-ref e 'variable))
	 (desc (slot-ref variable 'description)))
    (if (is-a? desc Functional-Description)
	(meaning-constant (el-global-name (slot-ref desc 'generator)) r lap const stack for-effect k)
	(meaning-constant desc r lap const stack for-effect k))))  ; el-global-name

(define (compile-lapcode lap)
  ;; Lapcode modifications: changes the tag-number of a TAG to be the TAGs pc
  (let ((bytes '())
	 (pc 0)
	 (patchlist '()))
    (for-each
     (lambda (lap-entry)
       (let* ((op (car lap-entry))
	      (off (cdr lap-entry))
	      (opcode (opcode-ref op)))
	 (cond
	  ((eq? op 'TAG)
	   (set-car! off pc))
	  ((goto? op)
	   (set! bytes (append! (list (cdr off) 'nil opcode)
				bytes))
	   (set! pc (+ pc 3))
	   (set! patchlist (cons bytes patchlist)))
	  ((or (and (pair? off) (begin (set! off (cdr off)) (eq? op 'byte-constant)))
	       (and (eq? op 'byte-constant) (integer? off)))
	   (cond
	    ((< off 64)
	     (set! bytes (cons (+ opcode off) bytes))
	     (set! pc (+ pc 1)))
	    (else
	     (set! bytes (append! (list (ash off -8) (logand off 255) (opcode-ref 'byte-constant2))
				  bytes))
	     (set! pc (+ pc 3)))))
	  ((and (eq? op 'byte-stack-set) (> off 255))
	   (set! bytes (append! (list (ash off -8) (logand off 255) (opcode-ref 'byte-stack-set2))
				bytes))
	   (set! pc (+ pc 3)))
	  ((memq op '(byte-listN byte-concatN byte-insertN byte-stack-set))
	   (set! bytes (append! (list off opcode)
				bytes))
	   (set! pc (+ pc 2)))
	  ((nil? off)
	   (set! bytes (cons opcode bytes))
	   (set! pc (+ pc 1)))
	  ((< off 6)
	   (set! bytes (cons (+ opcode off) bytes))
	   (set! pc (+ pc 1)))
	  ((< off 256)
	   (set! bytes (append! (list off (+ opcode 6))
				bytes))
	   (set! pc (+ pc 2)))
	  (else
	   (set! bytes (append! (list (ash off -8) (logand off 255) (+ opcode 7))
				bytes))
	   (set! pc (+ pc 3))))))
     lap)
    ;; patch gotos
    ;; ((pc . depth) nil 203 ...) becomes
    ;; ((ash pc -8) (logand pc 255) 203 ...)
    (for-each
     (lambda (bytes-tail)
       (set! pc (caar bytes-tail))
       (set-car! (cdr bytes-tail) (logand pc 255))
       (set-car! bytes-tail (ash pc -8)))
     patchlist)
    (reverse! bytes)))

(define (opcode-ref op)
  (let ((tmp (assq op opcodes)))  ; comp-init
    (and tmp (cdr tmp))))

(define (goto? op)
  (memq op '(byte-goto
	     byte-goto-if-nil
	     byte-goto-if-not-nil
	     byte-goto-if-nil-else-pop
	     byte-goto-if-not-nil-else-pop
	     byte-pushcatch byte-pushconditioncase)))

(define (output lap op operand)
  (if (eq? operand 'nil)
      (cons (list op) lap)
      (cons (cons op operand) lap)))

(define (stack-adjust stack op operand)
  (let ((depth (+ (car stack) (stack-adjustment op operand))))
    (cons depth (max depth (cdr stack)))))

(define (stack-extend stack n)
  (let ((depth (+ n (car stack))))
    (cons depth (max depth (cdr stack)))))

(define (make-bytecode-function args bytecode const maxdepth)
  (list 'make-byte-code
        `(byte-compile-make-args-desc ',(transcode-args args output-args))
         bytecode
         const
         maxdepth))

;; lift! will transform Function to Flat-Function
(define-method (meaning (e Function) r lap const stack for-effect k)
  (let* ((n* (slot-ref e 'variables))
	 (e* (slot-ref e 'body)))
    (if for-effect
	(k lap const stack #f)
	(meaning-constant (meaning-lambda n* e* 0) r lap const stack for-effect k))))

;; lift! will transform Continuation to Flat-Continuation
(define-method (meaning (e Continuation) r lap const stack for-effect k)
  (let* ((n* (slot-ref e 'variables))
	 (e* (slot-ref e 'body))
	 (cont (convert2Regular-Application
		  (make Constant #:value 'make-instance)
		  (make Constant #:value 'Continuation)
		  (make Constant #:value ':value)
		  (make Constant #:value (meaning-lambda n* e* 0)))))
    (if for-effect
	(k lap const stack #f)
	(meaning cont r lap const stack for-effect k))))

;; new
(define-method (meaning (e Flat-Continuation) r lap const stack for-effect k)
  (let* ((n* (slot-ref e 'variables))
	 (e* (slot-ref e 'body))
	 (free* (slot-ref e 'free))
	 (operand (+ (number-of free*) 1))
	 (cont (convert2Regular-Application
		  (make Constant #:value 'make-instance)
		  (make Constant #:value 'Continuation)
		  (make Constant #:value ':value)
		  (make Constant #:value (meaning-lambda n* e* 0)))))
    (if for-effect
	(k lap const stack #f)
	(if (is-a? free* No-Free)
	    (meaning cont r lap const stack for-effect k)
	    (meaning (convert2Regular-Application
		      (make Constant #:value 'make-instance)
		      (make Constant #:value 'Continuation)
		      (make Constant #:value ':value)
		      (apply convert2Regular-Application
			     (make Constant #:value 'make-closure)
			     (make Constant #:value (meaning-lambda n* e* (number-of free*)))
			     (extract free*)))  ; free* should be spliced to have the correct number of arguments
	     r lap const stack for-effect k)))))

(define-method (meaning (e Flat-Function) r lap const stack for-effect k)
  (let* ((n* (slot-ref e 'variables))
	 (e* (slot-ref e 'body))
	 (free* (slot-ref e 'free))
	 (operand (+ (number-of free*) 1)))
    (if for-effect
	(k lap const stack #f)
	(if (is-a? free* No-Free)
	    (meaning-constant (meaning-lambda n* e* 0) r lap const stack for-effect k)
	    (let ((fun (meaning-lambda n* e* (number-of free*))))
	      (meaning-constant 'make-closure r lap const stack #f
				(lambda (l c s d)
				  (meaning-constant fun r l c s #f
						    (lambda (l c s d)
						      (meaning free* r l c s #f
							       (lambda (l c s d)
								 (out-discard
								  (output l 'byte-call operand)
								  c
								  (stack-adjust s 'byte-call operand)
								  for-effect
								  k))))))))))))

(define (meaning-lambda n* e* reserved-csts)
  (meaning e*
	   (r-extend* r.init n* 0)
	   lap.init
	   const.init
	   (stack-extend stack.init (length n*))
	   #f
	   (lambda (l c s d)
	     (let ((compiled (meaning-out l c s d reserved-csts 'bytecode)))
	       (apply make-bytecode-function n* (cdr compiled))))))

;; Free variables
(define-method (meaning (e Free-Environment) r lap const stack for-effect k)
  (meaning (slot-ref e 'first) r lap const stack #f
	   (lambda (l c s d)
	     (meaning (slot-ref e 'last) r l c s for-effect k))))

(define-method (meaning (o Free-Reference) r lap const stack for-effect k)
  (let ((index (slot-ref o 'index)))
    (out-discard (output lap 'byte-constant index)
		 const
		 (stack-adjust stack 'byte-constant index)
		 for-effect
		 k)))

(define-method (meaning (e No-Free) r lap const stack for-effect k)
  (out-discard lap const stack for-effect k))

(define-method (meaning (e Arguments) r lap const stack for-effect k)
  (meaning (slot-ref e 'first) r lap const stack #f
	   (lambda (l c s d)
	     (meaning (slot-ref e 'others) r l c s for-effect k))))

(define-method (meaning (e No-Argument) r lap const stack for-effect k)
  (out-discard lap const stack for-effect k))

(define (transcode-args v* k)
  (let loop ((v* v*)
	     (regulars '()))
    (if (pair? v*)
	(if (slot-ref (car v*) 'dotted?)
	    (k (reverse! regulars) (list (slot-ref (car v*) 'name)))
	    (loop (cdr v*)
		  (cons (slot-ref (car v*) 'name) regulars)))
	(k (reverse! regulars) '()))))

(define (output-args regulars rest)
  (append regulars
	  (if (pair? rest) (list '&rest) '())
	  rest))

(define-method (meaning (e Sequence) r lap const stack for-effect k)
  (meaning (slot-ref e 'first) r lap const stack #t
	   (lambda (l c s d)
	     (meaning (slot-ref e 'last) r l c s for-effect k))))

(define (out-discard l c s d k)
  (when d
    (set! l (output l 'byte-discard 'nil))
    (set! s (stack-adjust s 'byte-discard 'nil)))
  (k l c s d))

;; Tags will be (TAG . (tag-number . stack-depth))
(define (make-tag)
  (set! *tag-number* (+ 1 *tag-number*))
  (list 'TAG *tag-number*))

(define (out-tag tag lap const stack for-effect k)
  (set-car! stack (cdr (cdr tag)))
  (k (cons tag lap) const stack for-effect))

(define (output-goto op tag lap stack k)
  (let ((new-stack (stack-adjust stack op tag)))
    (set-cdr! (cdr tag) (car new-stack))
    (k (cons (cons op tag) lap) new-stack)))

(define-method (meaning (e Alternative) r lap const stack for-effect k)
  (let ((ec (slot-ref e 'condition))
	(et (slot-ref e 'consequent))
	(ef (slot-ref e 'alternant)))
    (if ef
	(meaning-ternary-alternative ec et ef r lap const stack for-effect k)
	(meaning-binary-alternative ec et r lap const stack for-effect k))))

(define (meaning-binary-alternative ec et r lap const stack for-effect k)
  (let ((donetag (make-tag))
	(elsetag (make-tag)))
    (meaning
     ec r lap const stack #f
     (lambda (l c s d)
       (output-goto
	(if for-effect 'byte-goto-if-nil 'byte-goto-if-nil-else-pop) donetag l s
	(lambda (l s)
	  (meaning
	   et r l c s for-effect
	   (lambda (l c s d)
	     (out-tag
	      donetag l c s #f k)))))))))

(define (meaning-ternary-alternative ec et ef r lap const stack for-effect k)
  (let ((donetag (make-tag))
	(elsetag (make-tag)))
    (meaning
     ec r lap const stack #f
     (lambda (l c s d)
       (output-goto
	'byte-goto-if-nil elsetag l s
	(lambda (l s)
	  (meaning
	   et r l c s for-effect
	   (lambda (l c s d)
	     (output-goto
	      'byte-goto donetag l s
	      (lambda (l s)
		(out-tag
		 elsetag l c s d
		 (lambda (l c s d)
		   (meaning
		    ef r l c s for-effect
		    (lambda (l c s d)
		      (out-tag
		       donetag l c s #f k)))))))))))))))

(define-method (meaning (e Global-Assignment) r lap const stack for-effect k)
  (let* ((variable (slot-ref e 'variable))
	 (form (slot-ref e 'form))
	 (name (el-global-name (slot-ref variable 'name))))
    (if (slot-ref variable 'function?)
	(meaning-constant name r lap const stack #f
			(lambda (l c s d)
			  (meaning form r l c s #f
				   (lambda (l c s d)
				     (k (output l 'byte-fset 0)
					c
					(stack-adjust s 'byte-fset 0)
					for-effect)))))
	(meaning form r lap const stack #f
		 (lambda (l c s d)
		   (unless for-effect
		     (set! l (output l 'byte-dup 'nil))
		     (set! s (stack-adjust s 'byte-dup 'nil)))
		   (let ((var (assq name c)))
		     (unless var
		       (set! var (list name))
		       (set! c (cons var c)))
		     (k (output l 'byte-varset var)
			c
			(stack-adjust s 'byte-varset var)
			#f)))))))

;; Replaced by Box-Write
(define-method (meaning (e Local-Assignment) r lap const stack for-effect k)
  (meaning (slot-ref e 'form) r lap const stack #f
	   (lambda (l c s d)
	     (let* ((depth (car s))
		    (index (slot-ref (slot-ref e 'reference) 'index))
		    (off (- depth (+ 1 index))))
	       (k (output l 'byte-stack-set off)
		  c
		  (stack-adjust s 'byte-stack-set off)
		  #f)))))

(define-method (meaning (e Regular-Application) r lap const stack for-effect k)
  (let ((count (number-of (slot-ref e 'arguments))))
    (meaning (slot-ref e 'function) r lap const stack #f
				 (lambda (l c s d)
				   (meaning (slot-ref e 'arguments) r l c s #f
					    (lambda (l c s d)
					      (out-discard (output l 'byte-call count)
							   c
							   (stack-adjust s 'byte-call count)
							   for-effect
							   k)))))))

(define-method (meaning (e Continuation-Application) r lap const stack for-effect k)
  (let ((count (number-of (slot-ref e 'arguments))))
    (meaning-constant '_invoke r lap const stack #f
		      (lambda (l c s d)
			(meaning (slot-ref e 'function) r l c s #f
				 (lambda (l c s d)
				   (meaning (slot-ref e 'arguments) r l c s #f
					    (lambda (l c s d)
					      (out-discard (output l 'byte-call (+ count 1))
							   c
							   (stack-adjust s 'byte-call (+ count 1))
							   for-effect
							   k)))))))))

(define-method (meaning (e Predefined-Application) r lap const stack for-effect k)
  (let* ((variable (slot-ref e 'variable))
	 (desc (slot-ref variable 'description))
	 (name (slot-ref desc 'generator))
	 (op (symbol-property name 'byte-opcode))
	 (operand 0))
    (cond
     ((eq? name 'list)
      (meaning-list e r lap const stack for-effect k))
     ((memq name '(+ *))
      (meaning-variadic-numeric e r lap const stack for-effect k))
     ((eq? name '-)
      (meaning-minus e r lap const stack for-effect k))
     ((eq? name '/)
      (meaning-quo e r lap const stack for-effect k))
     (op
      (meaning (slot-ref e 'arguments) r lap const stack #f
	       (lambda (l c s d)
		 (out-discard (output l op operand)
			      c
			      (stack-adjust s op operand)
			      for-effect
			      k))))
     (else
      (meaning-constant name r lap const stack #f
			  (lambda (l c s d)
			    (meaning (slot-ref e 'arguments) r l c s #f
			     (lambda (l c s d)
			       (out-discard (output l 'byte-call (number-of (slot-ref e 'arguments)))
					    c
					    (stack-adjust s 'byte-call (number-of (slot-ref e 'arguments)))
					    for-effect
					    k)))))))))

;; TODO meaning-concat
(define-method (meaning-list (e Predefined-Application) r lap const stack for-effect k)
  (let ((count (number-of (slot-ref e 'arguments)))
	(op 'nil))
    (when (and (positive? count) (< count 5))
      (set! op (vector-ref #(byte-list1 byte-list2 byte-list3 byte-list4) (- count 1))))
    (cond
     ((= count 0)
      (meaning-constant 'nil r lap const stack for-effect k))
     ((< count 5)
      (meaning (slot-ref e 'arguments) r lap const stack #f
	       (lambda (l c s d)
		 (out-discard (output l op 0)
			      c
			      (stack-adjust s op 0)
			      for-effect
			      k))))
     ((< count 256)
      (meaning (slot-ref e 'arguments) r lap const stack #f
	       (lambda (l c s d)
		 (out-discard (output l 'byte-listN count)
			      c
			      (stack-adjust s 'byte-listN count)
			      for-effect
			      k))))
     (else
      (meaning-constant 'list r lap const stack #f
			(lambda (l c s d)
			  (meaning (slot-ref e 'arguments) r l c s #f
				   (lambda (l c s d)
				     (out-discard (output l 'byte-call count)
						  c
						  (stack-adjust s 'byte-call count)
						  for-effect
						  k)))))))))

(define-method (meaning-minus (e Predefined-Application) r lap const stack for-effect k)
  (let ((count (number-of (slot-ref e 'arguments))))
    (if (= count 1)
	(meaning (slot-ref e 'arguments) r lap const stack #f
		 (lambda (l c s d)
		   (out-discard (output l 'byte-negate 0)
				c
				(stack-adjust s 'byte-negate 0)
				for-effect
				k)))
	(meaning-variadic-numeric e r lap const stack for-effect k))))

(define-method (meaning-quo (e Predefined-Application) r lap const stack for-effect k)
  (let ((count (number-of (slot-ref e 'arguments))))
    (if (= count 2)
	(meaning (slot-ref e 'arguments) r lap const stack #f
		 (lambda (l c s d)
		   (out-discard (output l 'byte-quo 0)
				c
				(stack-adjust s 'byte-quo 0)
				for-effect
				k)))
	(meaning-constant '/ r lap const stack #f
			  (lambda (l c s d)
			    (meaning (slot-ref e 'arguments) r l c s #f
			     (lambda (l c s d)
			       (out-discard (output l 'byte-call (number-of (slot-ref e 'arguments)))
					    c
					    (stack-adjust s 'byte-call (number-of (slot-ref e 'arguments)))
					    for-effect
					    k))))))))

(define-method (meaning-variadic-numeric (e Predefined-Application) r lap const stack for-effect k)
  (let* ((variable (slot-ref e 'variable))
	 (name (slot-ref variable 'name))
	 (op (symbol-property name 'byte-opcode))
	 (count (number-of (slot-ref e 'arguments))))
    (cond
     ((= count 0)
      (meaning-constant (case name
			  ((+ -) 0)
			  ((*) 1))
			r lap const stack for-effect k))
     ((= count 1)
      (meaning (slot-ref e 'arguments) r lap const stack #f
	       (lambda (l c s d)
		 (meaning-constant 1 r l c s d
				   (lambda (l c s d)
				     (out-discard (output l 'byte-mult 0)
						  c
						  (stack-adjust s 'byte-mult 0)
						  for-effect
						  k))))))
     ((= count 2)
      (meaning (slot-ref e 'arguments) r lap const stack #f
		  (lambda (l c s d)
		    (out-discard (output l op 0)
				 c
				 (stack-adjust s op 0)
				 for-effect
				 k))))
     (else
      (meaning-constant name r lap const stack #f
			(lambda (l c s d)
			  (meaning (slot-ref e 'arguments) r l c s #f
				   (lambda (l c s d)
				     (out-discard (output l 'byte-call count)
						  c
						  (stack-adjust s 'byte-call count)
						  for-effect
						  k)))))))))

(define (byte-discard* n lap)
  (if (positive? n)
      (output (byte-discard* (- n 1) lap) 'byte-discard 'nil) 
      lap))

(define (discard num preserve-tos lap)
  (if preserve-tos
      (if (> num 0)
	  (byte-discard* (- num 1) (output lap 'byte-stack-set num))
	  lap)
      (byte-discard* num lap)))

(define (append1 l x)
  (append l (list x)))

(define-method (meaning (e Fix-Let) r lap const stack for-effect k)
  (let* ((n* (slot-ref e 'variables))
	 (r2 (r-extend* r n* (car stack))))
    (meaning (slot-ref e 'arguments) r lap const stack #f
	     (lambda (l c s d)
	       (meaning (slot-ref e 'body) r2 l c s d
			(lambda (l c s d)
			  (k (discard (number-of (slot-ref e 'arguments)) (> (car s) (car stack)) l)
			     c
			     (stack-adjust s 'byte-discardN (number-of (slot-ref e 'arguments)))
			     #f)))))))

(define (output-varbind n lap const stack for-effect k)
  (let ((tmp (or (assq n const)
		 (begin (set! const (cons (list n) const))
			(car const)))))
    (out-discard (output lap 'byte-varbind tmp)
		 const
		 (stack-adjust stack 'byte-varbind tmp)
		 for-effect
		 k)))

(define-method (meaning (e Dynamic-Let) r lap const stack for-effect k)
  (let* ((variable (slot-ref e 'variable))
	 (name (slot-ref variable 'name)))
    (meaning (slot-ref e 'argument) r lap const stack #f
	     (lambda (l c s d)
	       (output-varbind name l c s #f
			       (lambda (l c s d)
				 (meaning (slot-ref e 'body) r l c s for-effect
						   (lambda (l c s d)
						     (k (output l 'byte-unbind 1)
							c
							(stack-adjust s 'byte-unbind 1)
							#f)))))))))

(define-method (meaning (e Dynamic-Reference) r lap const stack for-effect k)
  (let* ((variable (slot-ref e 'variable))
	 (name (slot-ref variable 'name))
	 (tmp (assq name const)))
    (unless tmp
      (set! tmp (list name))
      (set! const (cons tmp const)))
    (out-discard (output lap 'byte-varref tmp)
		 const
		 (stack-adjust stack 'byte-varref tmp)
		 for-effect
		 k)))

(define-method (meaning (e Condition-Case) r lap const stack for-effect k)
  (let ((var (slot-ref e 'variable))
	 (depth (car stack))
	 (clause (cons (make-tag) (slot-ref e 'fail)))
	 (endtag (make-tag)))
    (meaning-constant
     '(error) r lap const stack #f
     (lambda (l c s d)
       (output-goto
	'byte-pushconditioncase (car clause) l s
	(lambda (l s)
	  (meaning
	   (slot-ref e 'exp) r l c s #f
	   (lambda (l c s d)
	     (set! l (output l 'byte-pophandler 'nil))
	     (set! s (stack-adjust s 'byte-pophandler 'nil))
	     (meaning
	      (slot-ref e 'ok) (r-extend* r (list var) (- (car s) 1)) l c s #f
	      (lambda (l c s d)
		(set! l (discard 1 #t l))
		(output-goto
		 'byte-goto endtag l s
		 (lambda (l s)
		   (set-cdr! (cdr (car clause)) (+ depth 1))
		   (out-tag
		    (car clause) l c s d
		    (lambda (l c s d)
		      (meaning
		       (slot-ref e 'fail) (r-extend* r (list var) (- (car s) 1)) l c s #f
		       (lambda (l c s d)
			 (set! l (discard 1 #t l))
			 (output-goto
			  'byte-goto endtag l s
			  (lambda (l s)
			    (out-tag
			     endtag l c s d k)))))))))))))))))))

;; Compiling files

(define (read-file filename)
  (call-with-input-file filename
    (lambda (in)
      (let gather ((e (read in))
                   (content '()))
        (if (eof-object? e)
            (reverse content)
            (gather (read in) (cons e content)))))))

(define (write-result-file filename code)
  (call-with-output-file filename
    (lambda (out)
      (display ";;; Compiled" out)
      (newline out)
      (display ";;; in Emacs version xx" out)
      (newline out)
      (display ";;; without optimization" out)
      (newline out) (newline out)
      (write code out))))  ; pretty-print

(define (comp-file filename)
  (let* ((complete-filename (string-append filename ".ebel"))
	 (out-filename (string-append filename ".elc"))
	 (code (read-file complete-filename)))
    (when (pair? code)
      (write-result-file out-filename
			 (comp `(do . ,code))))))

