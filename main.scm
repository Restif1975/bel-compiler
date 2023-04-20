(use-modules (expand)
	     (comp)
	     (ice-9 match))

(define (main args)
  (apply macroexpand-file (cdr args))
  (apply comp-file (cdr args)))





