(defpackage #:parser-parser
  (:use #:CL)
  (:export #:scan-and-update-scope
	   #:scan-code-block	   
   ))

(in-package #:parser-parser)

(defvar *test* "(a b (c))")
(defvar *test1* "(a b ((c d (e 4)))")
(defvar *test2* "( ( aa (a) a aa ))))")
(defvar *test3* "( ( aa (a) a aa )) (c d (b) (d))")


(defvar *scope-table* (make-hash-table :test 'equal))
(defvar *scope-dependency-table* (make-hash-table :test 'equal))
(defvar *scope-stack* (list (make-symbol "Adam")))


(defun update-list-in-hashtabe (key table eles)
  (if (eql 'CONS (type-of eles))
      (setf (gethash key table)
	    (append (gethash key table) eles))
      (setf (gethash key table)
	    (append (gethash key table) (list eles)))))

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
     with in-str-flag = nil
     for c across code
     do (cond (in-str-flag
	       (if (not (eql #\" c))
		   (setf temp (append temp (list c))
			 last c)
		   (setf temp (append temp (list c))
			 result (append result (list (concatenate 'string temp)))
			 temp nil
			 last nil
			 in-str-flag (not in-str-flag))))
	      ((or (eql #\( c) (eql #\) c))
	       (if (not (eql last nil))
		   (setf result (append result (list (concatenate 'string temp) c))
			 temp nil
			 last nil)
		   (setf result (append result (list c))
			 last nil
			 )))
	      ((or (eql #\  c) (eql #\tab c))
	       (if (not (eql last nil))
		   (setf result (append result (list (concatenate 'string temp)))
			 temp nil
			 last nil)))
	      ((eql #\" c)
	       (setf result (append result
				    (if temp (list (concatenate 'string temp))))
		     temp (append temp (list c))
		     last c
		     in-str-flag (not in-str-flag)
		     ))
	      (t
	       (setf temp (append temp (list c))
		     last c)))
     finally (return-from scan-code-block result)))

#|
(setf *scope-table* (make-hash-table :test 'equal)
      *scope-dependency-table* (make-hash-table :test 'equal)
      *scope-stack* (list (make-symbol "Adam"))
      )

(scan-and-update-scope (scan-code-block *test2*)
		       *scope-stack*
		       *scope-table*
		       *scope-dependency-table*)

;; should in io.lisp

(defun read-code (filepath)
  (with-open-file (in filepath)
    (do ((line (read-line in) (read-line in))
	 (stack (list (make-symbol "Adam"))))
	((null line))
      (setf stack
	    (scan-and-update-scope (scan-code-block line)
				   stack
				   *scope-table*
				   *scope-dependency-table*)))
))

(read-code "./parser.lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|#
(with-open-file (f "./table.ccq"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  (loop for key being the hash-keys of *scope-table*
     using (hash-value value)
     do (format f "~a~%" (list key value))))

