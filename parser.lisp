(defvar *test* "(a b (c))")
(defvar *test1* "(a b ((c d (e 4)))")
(defvar *test2* "( ( aa (a) a aa ))))")
(defvar *test3* "( ( aa (a) a aa )) (c d (b) (d))")


(defvar *scope-table* (make-hash-table :test 'equal))
(defvar *scope-dependency-table* (make-hash-table :test 'equal))
(defvar *scope-stack* (list (make-symbol "Adam")))


(defun update-list-in-hashtabe (key table &optional (all nil) &rest eles)
  (if all
      (setf (gethash key table)
	 (append (gethash key table) all))
      (setf (gethash key table)
	 (append (gethash key table) eles))))

;; maybe recursive do not have good expressiveness here.
(defun scan-and-update-scope (elis stack table dep)
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
				  ;;:= DEBUG
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


(setf *scope-table* (make-hash-table :test 'equal)
      *scope-dependency-table* (make-hash-table :test 'equal)
      *scope-stack* (list (make-symbol "Adam"))
)
(scan-and-update-scope (scan-code-block *test2*)
		       *scope-stack*
		       *scope-table*
		       *scope-dependency-table*)
