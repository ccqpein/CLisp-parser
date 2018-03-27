#!/usr/local/bin/sbcl --script

(ql:quickload "cl-fad")
(load "./parser.lisp")
(load "./io.lisp")

(defpackage #:parser-main
  (:use #:CL #:parser-io))

(in-package #:parser-main)

(defun directory-or-file (input)
  )

(defun main ()
  (read-code (cadr sb-ext:*posix-argv*))
  (write-ccq-file parser-io::*scope-table*
		  parser-io::*scope-dependency-table*)
  #|(write-ccq-file-with-filename "./table1.ccq"
				parser-io::*scope-table*
				parser-io::*scope-dependency-table*)|#
  )

(main)
