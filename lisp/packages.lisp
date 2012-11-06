
(defpackage :intercom
  (:use :cl :jsown :alexandria)
  (:export :define-remote-procedure
           :message :activep
           :*remote-procedure-context* :*hydra-head-timeout* :*hydra-body-timeout*
           :screen-var :with-screen-db-lock :session-var :with-session-db-lock
           :threadable-lambda :auto-end-remote-procedure-p))
