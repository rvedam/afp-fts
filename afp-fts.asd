;;;; afp-file-transfer-server.asd

(asdf:defsystem #:afp-fts
  :description "Describe afp-file-transfer-server here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria #:bordeaux-threads #:usocket)
  :in-order-to ((test-op (test-op "afp-fts-test")))
  :components ((:file "package")
               (:file "afp-fts")))

