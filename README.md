# CLisp-parser

`common-lisp` parse lisp file && create a file including two tables. 

~~~lisp
*scope-table*
*scope-dependency-table*
~~~

***scope-table\***

Every s-expression has a symbol, this symbol be a key of scope-table to point to s-expression. Content of key is s-expression.

***scope-dependency-table\***

I try to make this table for symbol depend relationship to find symbol scope. However, it looks like `go-translater` do not need find symbols scope.


## Usage ##
