;;;; afp-file-transfer-server.lisp

(in-package #:afp-fts)

(defparameter *prod-cv* (bt:make-condition-variable :name "prod-cond-variable"))
(defparameter *cons-cv* (bt:make-condition-variable :name "cons-cond-variable"))
(defparameter *counter-lock* (bt:make-lock "counter lock"))

(defparameter *conn-queue* (list))

(defparameter *keep-alive* t)

(defun send-file (conn fpath)
  "send a file along socket connection CONN given file at path FPATH."
  (with-open-file (strm fpath :if-does-not-exist nil)
    (loop :for line = (read-line strm nil)
       :until (null line)
       :do
       (format (usocket:socket-stream conn) "~A~%" line))))

;; TODO: need to refactor lambda into a named function and map to *standard-output* properly
(defun start-thread-pool (nthreads)
  (let ((thread-pool (list)))
    (dotimes (i nthreads)
      (let* ((stdout *standard-output*)
	     (worker-thread-name-stream (make-string-output-stream)))
	(format worker-thread-name-stream "worker ~A" i)
	(push (bt:make-thread
	       (lambda ()
		 (let ((conn nil)
		       (done nil)
		       (curr-thread-name (bt:thread-name (bt:current-thread))))
		   (unwind-protect
			(loop
			   :while (null done)
			   :do
			   (bt:with-lock-held (*counter-lock*)
			     (loop
				:while (and (not (null *keep-alive*)) (= (length *conn-queue*) 0))
				:do
				(format stdout
					"[~A]: No work in queue... Worker Thread falling sleep~%" curr-thread-name)
				(bt:condition-wait *cons-cv* *counter-lock*)
				(format stdout
					"[~A]: Worker Thread woken up... Checking to see if there's any work~%" curr-thread-name)))
			     (when (> (length *conn-queue*) 0)
			       (format stdout "[~A]: Popping connection off queue~%" curr-thread-name)
			       (setf conn (car (last *conn-queue*)))
			       (setf *conn-queue* (butlast *conn-queue*)))
			   (bt:condition-notify *prod-cv*)
			     (loop :for line = (read-line (usocket:socket-stream conn))
				:while (not (string= line "quit"))
				:do
				(let ((split-words (cl-ppcre:split "\\s+" line)))
				  (if (> (length split-words) 1)
				      (when (string= (elt split-words 0) "get")
					(send-file conn (elt split-words 1))
					(usocket:socket-close conn)
					(return))
				      (format stdout "[~A]: Received ~A~%" curr-thread-name line)))
				:finally (progn
					   (format stdout "[~A]: Connection Finished. Closing connection~%" curr-thread-name)
					   (usocket:socket-close conn)))
			   (bt:with-lock-held (*counter-lock*)
			     (format stdout "done: ~A~%" (null *keep-alive*))
			     (setf done (null *keep-alive*)))))))
	       :name (get-output-stream-string worker-thread-name-stream)) thread-pool)))
    thread-pool))

(defun start-server (host port)
  "
   starts a new file transfer server at HOST:PORT. returns a thread instance
   representing server instance.
  "
  (let ((server-host host)
	(server-port port)
	(stdout *standard-output*)
	(thread-pool (start-thread-pool 1)))
    (bt:make-thread
     (lambda ()
       (format stdout "Starting server on ~A:~A~%" server-host server-port)
       (let ((done nil))
	 (usocket:with-server-socket (server-socket (usocket:socket-listen server-host server-port ))
	   (loop
	      :while (null done)
	      :do
	      (bt:with-lock-held (*counter-lock*)
		(loop
		   :while (and (not (null *keep-alive*)) (= (length *conn-queue*) 3))
		   :do
		   (format stdout "queue full... Waiting until queue is freed up~%")
		   (bt:condition-wait *prod-cv* *counter-lock*)
		   (format stdout "Waiting for more connections~%")))
	      (format stdout "queue not full. Waiting for connections...~%")
	      (let ((conn (usocket:socket-accept server-socket :element-type 'character)))
		(format stdout "Received a connection. Adding to queue~%")
		(bt:with-lock-held (*counter-lock*)
		  (push conn *conn-queue*)))
	      (format stdout "waking up workers~%")
	      (bt:condition-notify *cons-cv*)
	      (bt:thread-yield)
	      (format stdout "checking to see if queue is full...~%")
	      (bt:with-lock-held (*counter-lock*)
		(setf done (null *keep-alive*)))))))
     :name "command-line thread")))
