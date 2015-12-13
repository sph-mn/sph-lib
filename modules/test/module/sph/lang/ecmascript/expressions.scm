(define-test-module (test module sph lang ecmascript expressions)
  (import
    (sph lang ecmascript expressions)
    (sph hashtable))

  (test-execute-procedures-lambda
    (es-apply
      ("a" ("1" 2)) "a(\"1\",2)")
    (es-chain
      ("proc" "base" "arg1" "arg2") "base.proc(arg1,arg2)")
    (es-define
      ("a") "var a"
      ("a" 2) "var a=2"
      ("a" "2") "var a=\"2\"")
    (es-environment
      ((a "b" c)) "{\"a\":a,\"b\":\"b\",\"c\":c}")
    (es-function
      #f "(function(){})"
      (#f ("a" b "c")) "(function(a,b,c){})"
      ("return a" ("a" b "c")) "(function(a,b,c){return a})"
      (#f ("a") #:name b #:rest rest) "function b (a){var rest=new Array(arguments.length);for(var ___i=1;___i<arguments.length;___i+=1){rest.push(arguments[___i])}}")
    (es-identifier
      (a) "a"
      "a" "a")
    (es-if
      ("a==1" "false") "a==1?false:undefined"
      ("1" "false" "true") "1?false:true")
    (es-object
      ((("a" . 1) ("b" . 2))) "{\"a\":1,\"b\":2}")
    (es-ref
      (a "b") "a[\"b\"]"
      ("a[\"b\"]" "c") "a[\"b\"][\"c\"]")
    (es-set!
      (a 1) "a=1"
      ("a" 2) "a=2"
      (a "2") "a=\"2\"")
    (es-value
      1 "1"
      a "a"
      #t "true"
      #\a "\"a\""
      "1" "\"1\""
      (((1 . 2))) "{\"1\":2}"
      ((1 2 "3")) "[1,2,\"3\"]"
      (("1" . 2)) "[\"1\",2]"
      (unquote (vector 1 2 "3")) "[1,2,\"3\"]"
      (unquote (hashtable 1 2 "3" 4)) "{\"3\":4,\"1\":2}")
    (es-vector
      (1 2 "3" 4) "[1,2,\"3\",4]")
    (list->es-vector
      ((1 #t)) "[1,true]")))
