#!/usr/local/bin/sbcl --script

(load "./parser.lisp")
(load "./io.lisp")

(defpackage #:parser-main
  (:use #:CL #:parser-io))

(in-package #:parser-main)

(defun main ()
  (read-code (cadr sb-ext:*posix-argv*))
  (write-ccq-file parser-io::*scope-table*
		  parser-io::*scope-dependency-table*)
  (format t "./table.ccq")
  )

(main)

