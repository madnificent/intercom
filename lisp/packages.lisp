
(defpackage :intercom
  (:use :cl :jsown :alexandria)
  (:export :define-remote-procedure
           :translate-remote-procedure-name
           :message :activep  :with-error-as-fail
           :*remote-procedure-context* :*hydra-head-timeout* :*hydra-body-timeout* :*log-request-stream*
           :screen-var :with-screen-db-lock :session-var :with-session-db-lock
           :threadable-lambda :auto-end-remote-procedure-p
           :add-session-gc-callback :remove-session-gc-callback
           :add-screen-gc-callback :remove-screen-gc-callback
           :handle-talk-request))
