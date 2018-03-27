(defpackage #:parser-io
  (:use #:CL #:parser-parser)
  (:export #:read-code
	   #:write-ccq-file
	   #:write-ccq-file-with-filename
	   ))

(in-package #:parser-io)

(defvar output-filename "./table.ccq")
(defvar filter-filetype "lisp")

(defvar *scope-table* (make-hash-table :test 'equal))
(defvar *scope-dependency-table* (make-hash-table :test 'equal))


;; make global var: two table
(setf *scope-table* (make-hash-table :test 'equal)
      *scope-dependency-table* (make-hash-table :test 'equal)
      )


(defun all-files-in-folder (folderpath)
  "return list of turple of path and filename"
  (let* (files
	 (base-directory (pathname-directory (truename folderpath)))
	 (sub-ind (- (length base-directory) 1)))
    (cl-fad:walk-directory folderpath #'(lambda (x) (push x files))
			   :test #'(lambda (x) (equal filter-filetype (pathname-type x)))
			   )
    (mapcar #'(lambda (x)
		(cons (subseq (pathname-directory x) sub-ind)
		      (pathname-name x)))
	    files)))


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
 				   *scope-dependency-table*)))))


(defun write-ccq-file (scope dependency)
  (with-open-file (f output-filename
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


;; re-binding filename
(defun write-ccq-file-with-filename (filename scope dependency)
  (write-ccq-file scope dependency))
