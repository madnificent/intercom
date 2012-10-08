
(asdf:defsystem :intercom-examples
  :name "Intercom"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0.0.1"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "Some example remote procedures for intercom"
  :depends-on (intercom jsown)
  :serial t
  :components ((:file "intercom-examples")))
