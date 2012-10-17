
(asdf:defsystem :intercom
  :name "Intercom"
  :author "Aad Versteden <aad@knowified.com>"
  :author "Karel Kremer <karel@knowified.com>"
  :version "0.0.1"
  :maintainer "Aad Versteden <aad@knowified.com>"
  :licence "MIT"
  :description "Intercom allows bidirectional message passing between javascript and lisp."
  :depends-on (jsown hunchentoot bordeaux-threads split-sequence)
  :serial t
  :components ((:file "packages")
               (:file "intercom")))
