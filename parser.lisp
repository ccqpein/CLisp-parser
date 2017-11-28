(defvar *test* "(a b (c))")
(defvar *test1* "(a b (c d (e 4))")
(defvar *test2* "( ( aa (a) a aa ))))")

;;:= parser may not need symbol table.
;; symbol table using in compiling to find symbol scope
(defvar *symbol-table* (make-hash-table))
(defvar *scope-stack* '())

(defun match-char (char symbol-stack)
  ;:= could use char= in future
  (cond ((equal char #\()
	 ;:= could use tail recursive
	 (cons (gensym) symbol-stack))
	(t symbol-stack)
	))


(defun scan-code-block (code)
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
