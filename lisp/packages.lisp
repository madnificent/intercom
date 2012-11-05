
(defpackage :intercom
  (:use :cl :jsown :alexandria)
  (:export :define-remote-procedure
           :message :activep
           :*remote-procedure-context* :*hydra-head-timeout* :*hydra-body-timeout*
           ;;---? should with-session-db-lock and with-screen-db-lock be exported too?
           :screen-var :session-var :threadable-lambda :auto-end-remote-procedure-p))
