(load "/Users/cchen386/Desktop/CLisp-parser/parser.fasl")

(defpackage #:parser-io
  (:use #:CL #:parser-parser))

(in-package #:parser-io)


;;:= TODO: need move this variables to global env
(defvar *scope-table* (make-hash-table :test 'equal))
(defvar *scope-dependency-table* (make-hash-table :test 'equal))

(setf *scope-table* (make-hash-table :test 'equal)
      *scope-dependency-table* (make-hash-table :test 'equal)
      )

(defun read-code (filepath)
  (with-open-file (in filepath)
    (do ((line (read-line in 'nil) (read-line in 'nil))
	 (stack (list (make-symbol "Adam"))))
	((null line))
      (setf stack
	    (scan-and-update-scope (scan-code-block line)
				   stack
				   *scope-table*
				   *scope-dependency-table*)))
))


(with-open-file (f "./table.ccq"
		   :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create)
  (loop
    for key being the hash-keys of *scope-table*
      using (hash-value value)
    do (format f "~a~%" (list key value))))

; debug scan-code-block
;(read-code "./parser.lisp")
