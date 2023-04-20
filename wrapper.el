;;; Dispatch between direct & cps versions -*- lexical-binding: t -*-

(defclass Continuation ()
  ((value
    :initarg :value
    :accessor value)))

;; Wrapper function

(defmacro defwrap1 (name direct cps)
  `(progn
     (cl-defmethod ,name (x &rest args)
       (funcall ',direct x))
     (cl-defmethod ,name ((k Continuation) &rest args)
       (funcall ',cps k (car args)))))

(defmacro defwrap2 (name direct cps)
  `(progn
     (cl-defmethod ,name (x y &rest args)
       (funcall ',direct x y))
     (cl-defmethod ,name ((k Continuation) x &rest args)
       (funcall ',cps k x (car args)))))

(defmacro defwrap3 (name direct cps)
  `(progn
     (cl-defmethod ,name (x y z &rest args)
       (funcall ',direct x y z))
     (cl-defmethod ,name ((k Continuation) x y &rest args)
       (funcall ',cps k x y (car args)))))

(defmacro defwrapN (name direct cps)
  (let ((name2 (intern (concat (symbol-name name) "2"))))
    `(progn
       (defun ,name (&rest args)
	 (if (null args)
	     (funcall ',direct)
	   (apply ',name2 args)))
       (cl-defmethod ,name2 (x &rest args)
	 (apply ',direct x args))
       (cl-defmethod ,name2 ((k Continuation) &rest args)
	 (apply ',cps k args)))))

