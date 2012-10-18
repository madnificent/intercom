
(defpackage :intercom
  (:use :cl :jsown)
  (:export :define-remote-procedure
           :message :activep
           :*remote-procedure-context*
           :*watchdog-timeout*))
