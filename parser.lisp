(defvar *test* "(a b (c))")
(defvar *test1* "(a b ((c d (e 4)))")
(defvar *test2* "( ( aa (a) a aa ))))")


(defvar *scope-table* (make-hash-table))
(defvar *scope-dependency-table* (make-hash-table))
(defvar *scope-stack* (list (make-symbol "Adam")))

(defun scan-and-update-scope (elis stack table dep)
  ; stack no side effection
  (print (list elis stack table dep))
  (cond ((eql nil elis)
	 nil)
	((eql #\( (car elis)) ;; need more test
	 (scan-and-update-scope
	  (cdr elis)
	  (progn (setf stack (push (gensym) stack))
		 (setf (gethash (symbol-name (cadr stack)) table) (append (gethash (symbol-name (cadr stack)) table) (list (car stack))))
		 stack)
	  (progn (setf (gethash (symbol-name (car stack)) table) '())
		 table)
	  (progn (setf (gethash (symbol-name (car stack)) dep) (append (gethash (symbol-name (car stack)) dep) (list (car stack))))
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
	  (progn (setf (gethash (symbol-name (car stack)) table) (append (gethash (symbol-name (car stack)) table) (list (car elis))))
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
      *scope-stack* (list (make-symbol "Adam"))
)
(scan-and-update-scope (scan-code-block *test1*)
		       *scope-stack*
		       *scope-table*
		       *scope-dependency-table*)
