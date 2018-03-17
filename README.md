# CLisp-parser

## Summary ##

`Clisp-parser` is a lite tool to parse common lisp code (maybe works with other lisp too, I haven't tried).

`Clisp-parser` read lisp file and create code struct in two tables:
~~~lisp
*scope-table*
*scope-dependency-table*
~~~

***scope-table\***

Every s-expression has a symbol, this symbol be a key of scope-table to point to s-expression. Content of key is s-expression.

***scope-dependency-table\***

I try to make this table for symbol depend relationship to find symbol scope. However, it looks like `go-translater` do not need find symbols scope.

#### Example ####

If you code like this
~~~lisp
;; root scope named "Adam"
( ( aa (a) a aa )) 

(c d (b) (d))
~~~

Two tables will like this:
~~~
"scope-table" 
("Adam" (#:G683 #:G686)) 
("G683" (#:G684)) 
("G684" ("aa" #:G685 "a" "aa")) 
("G685" ("a")) 
("G686" ("c" "d" #:G687 #:G688)) 
("G687" ("b")) 
("G688" ("d")) 
"scope-dependency-table" 
("G683" (#:|Adam|)) 
("G684" (#:G683 #:|Adam|)) 
("G685" (#:G684 #:G683 #:|Adam|)) 
("G686" (#:|Adam|)) 
("G687" (#:G686 #:|Adam|)) 
("G688" (#:G686 #:|Adam|)) 
~~~

## Usage ##

> Only can handle one file so far

`./main.lisp filename`

then there is a `.ccq` file created in folder
