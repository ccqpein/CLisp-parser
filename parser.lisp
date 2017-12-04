(defvar *test* "(a b (c))")
(defvar *test1* "(a b ((c d (e 4)))")
(defvar *test2* "( ( aa (a) a aa ))))")


(defvar *scope-table* (make-hash-table))
(defvar *scope-dependency-table* (make-hash-table))
(defvar *scope-stack* '("Adam"))

(defun scan-and-update-scope (elis stack table dep)
  ; stack no side effection
  (print (list elis stack table dep))
  (cond ((eql nil elis)
	 nil)
	((eql #\( (car elis))
	 (scan-and-update-scope
	  (cdr elis)
	  (progn (setf stack (push (symbol-name (gensym)) stack))
		 (setf (gethash (cadr stack) table) (append (gethash (cadr stack) table) (list (car stack))))
		 stack)
	  (progn (setf (gethash (car stack) table) '())
		 table)
	  (progn (setf (gethash (car stack) dep) (append (gethash (car stack) dep) (list (car stack))))
		 dep)))
	((eql #\) (car elis))
	 (scan-and-update-scope
	  (cdr elis)
	  (cdr stack)
	  table
	  dep))
	(t
	 (scan-and-update-scope
	  (cdr elis)
	  stack
	  (progn (setf (gethash (car stack) table) (append (gethash (car stack) table) (list (car elis))))
		 table)
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


(setf *scope-table* (make-hash-table)
      *scope-dependency-table* (make-hash-table)
)
(scan-and-update-scope (scan-code-block *test1*)
		       *scope-stack*
		       *scope-table*
		       *scope-dependency-table*)
