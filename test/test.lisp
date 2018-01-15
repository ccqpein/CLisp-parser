(defpackage #:parser-test
  (:use #:CL #:parser-parser #:parser-io))


(in-package #:parser-test)


(defvar *test1* "(a b ((c d (e 4)))")
(defvar *test2* "( ( aa (a) a aa ))))")
(defvar *test3* "( ( aa (a) a aa )) (c d (b) (d))")
(defvar *test4* "(( da ee da ) ; )))")

(read-code "./testcase0.lisp")

(write-ccq-file parser-io::*scope-table*
		parser-io::*scope-dependency-table*)
