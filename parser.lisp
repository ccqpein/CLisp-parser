(defpackage #:parser-parser
  (:use #:CL)
  (:export #:scan-and-update-scope
	   #:scan-code-block	   
   ))

(in-package #:parser-parser)


(defun update-list-in-hashtabe (key table eles)
  (if (eql 'CONS (type-of eles))
      (setf (gethash key table)
	    (append (gethash key table) eles))
      (setf (gethash key table)
	    (append (gethash key table) (list eles)))))


(defun scan-and-update-scope (elis stack table dep)
  "update stack, scope table, and dependency table with code list"
  (cond ((eql nil elis)
	 stack) ; setf no side effect, so return stack to keep stack state
	((eql #\( (car elis)) ;; need more test
	 (scan-and-update-scope (cdr elis)
				(progn
				  (setf stack (push (gensym) stack))
				  (update-list-in-hashtabe (symbol-name (cadr stack)) table (car stack))
				  stack) ; need some other function
				(progn
				  (setf (gethash (symbol-name (car stack)) table) '())
				  table)
				(progn
				  (update-list-in-hashtabe (symbol-name (car stack)) dep (cdr stack))
				  dep)))
	((eql #\) (car elis))
	 (scan-and-update-scope (cdr elis)
				(cdr stack)
				table
				dep))
	(t
	 (scan-and-update-scope (cdr elis)
				stack
				(progn 
				  (update-list-in-hashtabe (symbol-name (car stack)) table (car elis))
				  table)
				dep))))


(defun scan-code-block (code-line)
  "cut a line of code to list"
  (let ((code-char-list
	  (append (concatenate 'list code-line)
		  (list #\linefeed)))
	(in-str-flag 'nil))
    (do* ((code-chars code-char-list (cdr code-chars))
	  (this-char (car code-chars) (car code-chars))
	  (result '())
	  (temp '())
	  (last nil))
	 ((null code-chars) result)
      (cond (in-str-flag
	       (if (not (eql #\" this-char))
		   (setf temp (append temp (list this-char))
			 last this-char)
		   (setf temp (append temp (list this-char))
			 result (append result (list (concatenate 'string temp)))
			 temp nil
			 last nil
			 in-str-flag (not in-str-flag))))
	    ((equal this-char #\;)
	     (setf code-chars '()))
	    ((or (eql #\( this-char) (eql #\) this-char))
	     (if (not (eql last nil))
		 (setf result (append result (list (concatenate 'string temp) this-char))
		       temp nil
		       last nil)
		 (setf result (append result (list this-char))
		       last nil
		       )))
	    ((or (eql #\  this-char) (eql #\tab this-char) (eql #\linefeed this-char))
	     (if (not (eql last nil))
		 (setf result (append result (list (concatenate 'string temp)))
		       temp nil
		       last nil)))
	    ((eql #\" this-char)
	     (setf result (append result (if temp (list (concatenate 'string temp))))
		   temp (append temp (list this-char))
		   last this-char
		   in-str-flag (not in-str-flag)))
	    (t
	       (setf temp (append temp (list this-char))
		     last this-char))))))
