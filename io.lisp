(defpackage #:parser-io
  (:use #:CL #:parser-parser))

(in-package #:parser-io)


;;:= TODO: need move this variables to global env.
;;:= TODO: need to read several files in ASDF/package struct.
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


(read-code "./parser.lisp")


(defun write-ccq-file (scope dependency)
  (with-open-file (f "./table.ccq"
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (format f "#:-> scope-table~%")
    (loop
      for key being the hash-keys of scope
	using (hash-value value)
      do (format f "~a~%" (list key value)))

    (format f "#:-> dependency-table~%")
    (loop
      for key being the hash-keys of dependency
	using (hash-value value)
      do (format f "~a~%" (list key value)))))


(write-ccq-file *scope-table* *scope-dependency-table*)
; debug scan-code-block

