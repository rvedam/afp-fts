;;;; afp-fts-test.lisp
(in-package :cl-user)
(defpackage :afp-fts-test
  (:use :cl
	:prove
	:afp-fts))

(in-package #:afp-fts-test)

(prove:plan 2)

(defparameter *server-thread* (afp-fts:start-server "127.0.0.1" 8888))

(prove:ok (bt:thread-alive-p *server-thread*))

(usocket:with-client-socket (clientfd clstrm "127.0.0.1" 8888)
  (format (usocket:socket-stream clientfd) "quit~%"))

(prove:ok (null (bt:thread-alive-p *server-thread*)))

(prove:finalize)
