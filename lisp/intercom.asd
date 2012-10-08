
(asdf:defsystem :intercom
  :name "Intercom"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0.0.1"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "Intercom allows bidirectional message passing between javascript and lisp."
  :depends-on (jsown hunchentoot bordeaux-threads split-sequence)
  :serial t
  :components ((:file "packages")
               (:file "intercom")))
