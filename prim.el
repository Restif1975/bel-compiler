;;; Emacs Lisp functions used for Bel primitives -*- lexical-binding: t -*-

(load "wrapper.el")

;; Primitives wrapper
;; used only for reference, not application (except apply)
(defwrap2 _eq eq eq_cps)
(defwrap2 _cons cons cons_cps)
(defwrap1 _car car car_cps)
(defwrap1 _cdr cdr cdr_cps)
(defwrap1 _cadr cadr cadr_cps)
(defwrap1 _cddr cddr cddr_cps)
(defwrap1 _caddr caddr caddr_cps)
(defwrap2 _setcar setcar setcar_cps)
(defwrap2 _setcdr setcdr setcdr_cps)
(defwrap1 _consp consp consp_cps)
(defwrap1 _symbolp symbolp symbolp_cps)
(defwrap1 _stringp stringp stringp_cps)
(defwrap1 _numberp numberp numberp_cps)
(defwrap1 _functionp functionp functionp_cps)
(defwrap1 _intern intern intern_cps)
(defwrap1 _symbol-name symbol-name symbol-name_cps)
(defwrap1 _random random random_cps)

(defwrapN _+ + +_cps)
(defwrapN _- - -_cps)
(defwrapN _* * *_cps)
(defwrapN _/ / /_cps)
(defwrap2 _equal equal equal_cps)
(defwrap2 _> > >_cps)
(defwrap2 _>= >= >=_cps)
(defwrap2 _< < <_cps)
(defwrap2 _<= <= <=_cps)
(defwrapN _list list list_cps)
(defwrapN _append append append_cps)
(defwrap1 _prin1 prin1 prin1_cps)
(defwrap1 _write-char write-char write-char_cps)

(defwrapN _apply apply apply_cps)

;; Used by the direct version of library functions
(fset 'apply_plain #'apply)

;; ccc only has a cps version
(fset '_ccc #'ccc_cps)

(defun ccc_cps (k f)
  (funcall f k (reify-k k)))

(defun apply_cps (k f &rest args)
  (cl-labels ((loop (args)
		    (if (null args)
			nil
		      (if (null (cdr args))
			  (car args)
			(cons (car args) (loop (cdr args)))))))
    (apply f k (loop args))))

(defun reify-k (k)
  #'(lambda (current-k value)
      (_invoke k value)))

;; k is a Continuation object
(defun _invoke (k v)
  (funcall (value k) v))

;; Other primitives CPS version

(defmacro defCPSsubr1 (newname oldname)
  `(defun ,newname (k x)
     (_invoke k (,oldname x))))

(defmacro defCPSsubr2 (newname oldname)
  `(defun ,newname (k x y)
     (_invoke k (,oldname x y))))

(defmacro defCPSsubrN (newname oldname)
  `(defun ,newname (k &rest args)
     (_invoke k (apply ',oldname args))))

(defCPSsubr2 eq_cps eq)
(defCPSsubr2 el-cons_cps cons)
(defCPSsubr1 car_cps car)
(defCPSsubr1 cdr_cps cdr)
(defCPSsubr1 cadr_cps cadr)
(defCPSsubr1 cddr_cps cddr)
(defCPSsubr1 caddr_cps caddr)
(defCPSsubr2 setcar_cps setcar)
(defCPSsubr2 setcdr_cps setcdr)
(defCPSsubr1 consp_cps consp)
(defCPSsubr1 symbolp_cps symbolp)
(defCPSsubr1 stringp_cps stringp)
(defCPSsubr1 numberp_cps numberp)
(defCPSsubr1 functionp_cps functionp)
(defCPSsubr1 intern_cps intern)
(defCPSsubr1 symbol-name_cps symbol-name)
(defCPSsubr1 random_cps random)

(defCPSsubrN +_cps +)
(defCPSsubrN -_cps -)
(defCPSsubrN *_cps *)
(defCPSsubrN /_cps /)
(defCPSsubr2 equal_cps equal)
(defCPSsubr2 >_cps >)
(defCPSsubr2 >=_cps >=)
(defCPSsubr2 <_cps <)
(defCPSsubr2 <=_cps <=)
(defCPSsubrN list_cps list)
(defCPSsubrN append_cps append)
(defCPSsubr1 prin1_cps prin1)
(defCPSsubr1 write-char_cps write-char)



