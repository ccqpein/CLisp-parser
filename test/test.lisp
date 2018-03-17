(defpackage #:parser-test
  (:use #:CL #:parser-parser #:parser-io))


(in-package #:parser-test)


(defvar *test1* "(a b ((c d (e 4)))")
(defvar *test2* "( ( aa (a) a aa ))))")
(defvar *test3* "( ( aa (a) a aa )) (c d (b) (d))")
(defvar *test4* "(( da ee da ) ; )))")

(defun test-parse-flow (case)
  (let ((*scope-table* (make-hash-table :test 'equal))
	(*scope-dependency-table* (make-hash-table :test 'equal))
	(stack (list (make-symbol "Adam"))))
    (scan-and-update-scope (scan-code-block case)
			   stack
			   *scope-table*
			   *scope-dependency-table*)
    (print "stack")
    (print stack)
    (print "scope-table")
    (loop
      for key being the hash-keys of *scope-table*
	using (hash-value value)
       do (print (list key value)))
    (print "scope-dependency-table")
    (loop
       for key being the hash-keys of *scope-dependency-table*
       using (hash-value value)
       do (print (list key value)))
    ))

;(test-parse-flow *test1*)
;(test-parse-flow *test2*)
;(test-parse-flow *test3*)
;(test-parse-flow *test4*)
(defun read-file-test ()
  (read-code "./testcase0.lisp")
  (print "scope-table")
  (loop
     for key being the hash-keys of parser-io::*scope-table*
     using (hash-value value)
     do (print (list key value)))
  (print "scope-dependency-table")
  (loop
     for key being the hash-keys of parser-io::*scope-dependency-table*
     using (hash-value value)
     do (print (list key value))))

(write-ccq-file parser-io::*scope-table*
		parser-io::*scope-dependency-table*)
