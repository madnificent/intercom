
(defpackage :intercom
  (:use :cl :jsown :alexandria)
  (:export :define-remote-procedure
           :message :activep
           :*remote-procedure-context* :*hydra-head-timeout* :*hydra-body-timeout*
           :screen-var :with-screen-db-lock :session-var :with-session-db-lock
           :threadable-lambda :auto-end-remote-procedure-p
           :add-session-gc-callback :remove-session-gc-callback
           :add-screen-gc-callback :remove-screen-gc-callback))
