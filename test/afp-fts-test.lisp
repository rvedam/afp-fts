;;;; afp-fts-test.lisp
(in-package :cl-user)
(defpackage :afp-fts-test
  (:use :cl
	:prove
	:afp-fts))

(in-package #:afp-fts-test)

(prove:plan 1)

(defparameter *server-thread* (afp-fts:start-server "127.0.0.1" 8002))

(prove:ok (bt:thread-alive-p *server-thread*))

(handler-case 
   (usocket:with-client-socket (clientfd clstrm "127.0.0.1" 8002)
     (format clstrm "quit"))
 (usocket:connection-refused-error () (print "connection refused")))

(prove:ok (bt:thread-alive-p *server-thread*))

(prove:finalize)
