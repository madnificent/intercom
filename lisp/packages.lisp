
(defpackage :intercom
  (:use :cl :jsown :alexandria)
  (:export :define-remote-procedure
           :message :activep
           :*remote-procedure-context*
           :*watchdog-timeout*))
