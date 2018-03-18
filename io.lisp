(defpackage #:parser-io
  (:use #:CL #:parser-parser)
  (:export #:read-code
	   #:write-ccq-file
	   #:write-ccq-file-with-filename
	   ))

(in-package #:parser-io)

(defvar filename "./table.ccq")
;;:= TODO: need move this variables to global env.
;;:= TODO: need to read several files in ASDF/package struct.
(defvar *scope-table* (make-hash-table :test 'equal))
(defvar *scope-dependency-table* (make-hash-table :test 'equal))


;; make global var: two table
(setf *scope-table* (make-hash-table :test 'equal)
      *scope-dependency-table* (make-hash-table :test 'equal)
      )


;; read lisp file and create two table
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


(defun write-ccq-file (scope dependency)
  (with-open-file (f filename
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
      do (format f "~A~%" (list key value)))))


(defun write-ccq-file-with-filename (filename scope dependency)
  (write-ccq-file scope dependency))
