(defvar *test* "(a b (c))")
(defvar *test1* "(a b (c d (e 4))")
(defvar *test2* "( ( aa (a) a aa ))))")


(defvar *scope-table* (make-hash-table))
(defvar *scope-dependency-table* (make-hash-table))
(defvar *scope-stack* '())

(defun scan-and-update-scope (elis stack table dep)
  (print elis)
  (cond ((eql #\( (car elis))
	 (scan-and-update-scope ; tail recursive here
	  (cdr elis)
	  (push (gensym) stack)
	  (setf (gethash (car stack) table) '())
	  (setf (gethash (car stack) table) (append (gethash (car stack) table) (list (car stack))))))
	((eql #\) (car elis))
	 (scan-and-update-scope
	  (cdr elis)
	  (cdr stack)
	  table
	  dep))))

(defun scan-code-block (code)
  "cut code to a list including all symbol/word"
  (loop
     with result = '()
     with temp = '()
     with last = nil
     for c across code
     do (cond ((or (eql #\( c) (eql #\) c))
	       (if (not (eql last nil))
		   (setf result (append result (list (concatenate 'string temp) c))
			 temp nil
			 last nil)
		   (setf result (append result (list c))
			 last nil
			 )))
	      ((eql #\  c)
	       (if (not (eql last nil))
		   (setf result (append result (list (concatenate 'string temp)))
			 temp nil
			 last nil)))
	      (t
	       (setf temp (append temp (list c))
		     last c)))
     finally (return-from scan-code-block result)))
