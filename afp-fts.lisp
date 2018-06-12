;;;; afp-file-transfer-server.lisp

(in-package #:afp-fts)

(defparameter *prod-cv* (bt:make-condition-variable :name "prod-cond-variable"))
(defparameter *cons-cv* (bt:make-condition-variable :name "cons-cond-variable"))
(defparameter *counter-lock* (bt:make-lock "counter lock"))

(defparameter *task-queue* (list))

(defparameter *keep-alive* t)

(defun start-server (host port)
  "
   starts a new file transfer server at HOST:PORT. returns a thread instance
   representing server instance.
  "
  (let ((server-host host)
	(server-port port)
	(stdout *standard-output*))
    (bt:make-thread
     (lambda ()
       (format stdout "Starting server on ~A:~A~%" server-host server-port)
       (usocket:with-server-socket (server-socket (usocket:socket-listen server-host server-port))
	 (let ((conn (usocket:socket-accept server-socket :element-type 'character)))
	   (unwind-protect
		(loop :for line = (read-line (usocket:socket-stream conn))
		   :until (string= line "quit")
		   :do
		   (format stdout "received: ~A~%" line)
		   :finally (usocket:socket-close conn))))))
     :name "command-line thread")))
