
(defpackage :intercom-examples
  (:use :cl :intercom))

(in-package :intercom-examples)

(define-remote-procedure test (count base diff)
  (loop repeat count
     for time = (+ base (random diff))
     do
       (sleep time)
       (message "bark" time))
  (message "cemetary" "senna died! :`("))

(define-remote-procedure echo (&key string count interval)
  (loop repeat count
     do (sleep (/ interval 1000)) ; we don't have a really good idea how good the sleep works
       (message "value" string))
  (message "ready" :true))

(define-remote-procedure eval (string)
  (message "ready" (eval (read string))))

(defun rand-between (a b)
  "returns a number between min and max"
  (+ (min a b) (random (abs (- a b)))))
(define-remote-procedure timeout (count min-ms max-ms end-key)
  (let ((start-time (get-internal-real-time)))
    (loop for nr from 0 below count
       for sleepytime = (rand-between (/ min-ms 1000) (/ max-ms 1000))
       do (message "current-time"
                   (jsown:new-js ("count" nr)
                                 ("timeout" (round (* sleepytime 1000)))
                                 ("total-time" (round (* (/ (- (get-internal-real-time) start-time)
                                                            internal-time-units-per-second)
                                                         1000))))))
    (message end-key "ok")))
