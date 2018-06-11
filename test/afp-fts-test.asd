;;; afp-fts-test
(asdf:defsystem #:afp-fts-test
  :description "Describe test here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :defsystem-depends-on (:prove-asdf)
  :depends-on (#:afp-fts #:prove)
  :components ((:test-file "afp-fts-test"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
