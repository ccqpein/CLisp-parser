(defvar *test* "(a b (c))")
(defvar *test1* "(a b (c d (e 4))")

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
  (let ((stack '()))
    (loop 
       for c in code
       do (progn (setf stack (match-char c stack))
		 (print stack)))
    stack))


