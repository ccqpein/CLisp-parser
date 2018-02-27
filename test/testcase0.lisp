(package main)

(import "fmt")

(func helloWorld(s:string) -> string
      (fmt.Println "hello world" s)
      (return "waahaha"))

(struct Test1
	(A:string
	 B:string
	 C:int))
