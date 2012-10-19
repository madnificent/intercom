
(in-package :intercom)

(defun s+ (&rest args)
  "pretty-prints and concatenates the resulting strings of each arg in <args>."
  (format nil "~{~A~}" args))

(defparameter *remote-procedures* (make-hash-table :test 'equal :synchronized t)
  "contains all intercom remote procedures, the keywords being the matched string and the values being the corresponding function.")

(defparameter *rid* nil
  "variable which contains the request id in functions which represent the execution of a remote procedure.")

(defparameter *store* nil
  "contains a key-value store for the variables which *should* be in the session")

(defparameter *watchdog-timeout* 600
  "if the javascript intercom side doesn't talk to us for more than *watchdog-timeout* seconds, we close down the active connections.")

(defparameter *hydra-body-id* nil
  "if we're currently in a hydra session, this contains the secret in the session.")

(defparameter *hydra-head-id* nil
  "represents the screen identifier of the current request")
(defmacro with-session-lock ((protection-symbol) &body body)
  "executes <body> in a piece of code in which the session is locked"
  `(progn
     (assert-session)
     (macrolet ((,protection-symbol (&body body)
                  `(bordeaux-threads:with-recursive-lock-held ((gethash 'intercom-session-lock *store*))
                     ,@body)))
       ,@body)))

(defmacro in-intercom-session (&body body)
  "executes a hunchentoot request in an environment in which *store* is bound to the current store."
  (let ((store-exists-p (gensym "store-exists-p")))
    `(progn (hunchentoot:start-session)
            (let ((,store-exists-p (hunchentoot:session-value 'store hunchentoot:*session*))
                  *store*)
              (unless ,store-exists-p
                (setf (hunchentoot:session-value 'store hunchentoot:*session*)
                      (make-hash-table :synchronized t)))
              (setf *store* (hunchentoot:session-value 'store hunchentoot:*session*))
              (unless ,store-exists-p
                (setf (gethash 'intercom-session-lock *store*)
                      (bordeaux-threads:make-lock "intercom session lock")))
              ,@body))))

(defun assert-session ()
  "asserts that we're currently running in an environment which is sane for intercom requests/executions"
  (assert *store*)
  (assert (gethash 'intercom-session-lock *store*)))

(defun intercom-var (variable)
  "returns <variable> from the session in *store*"
  (with-session-lock (!)
    (! (gethash variable *store*))))

(defun (setf intercom-var) (value variable)
  (with-session-lock (!)
    (! (setf (gethash variable *store*) value))))
(defun register-remote-procedure (name function)
  "registers the remote procedure for <name> to be <function>."
  (when (gethash name *remote-procedures*)
    (warn "overwriting remote procedure for ~A" name))
  (setf (gethash name *remote-procedures*) function))

(defun get-remote-procedure (name)
  "returns the remote procedure for <name> or nil if the procedure doesn't exist."
  (gethash name *remote-procedures*))

(defparameter *remote-procedure-context* nil
  "should contain an alist in which the keywords are special variables and the values
   are evaluated (in the creating thread) each time a remote procedure is built.
   this allows you to pass special variables.")

(defun thread-initial-bindings ()
  "calculates the initial bindings for the current thread.  this consists of whatever
   is available in bordeaux-threads:*initial-bindings*, but with what
   *remote-procedure* contains in front of it (in which the values are evaluated)."
  (concatenate 'list
               (loop for (k . v) in *remote-procedure-context*
                  collect (cons k (eval v)))
               bordeaux-threads:*default-special-bindings*))

(defun call-remote-procedure (rid name &rest args)
  "calls the remote prodecure with name <name> and <args> as the arguments with <rid> as reference.  assumes the special variable *store* exists"
  (assert (get-remote-procedure name))
  (bordeaux-threads:make-thread
   (let ((store *store*)
         (session hunchentoot:*session*))
     (lambda ()
       (let ((*store* store)
             (*rid* rid)
             (hunchentoot:*session* session))
         (start-rid *rid*)
         (unwind-protect
              (apply (get-remote-procedure name) args)
           (with-session-lock (!)
             (push rid (intercom-var 'rids-to-end)))))))
   :initial-bindings (thread-initial-bindings)
   :name name))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-remote-procedure-lambda-function (arguments body)
    "builds the s-expression representation for the lambda function which can be called for the definition of a remote procedure.  this handles the creation of the &key arguments."
    (let* ((arguments (split-sequence:split-sequence '&key arguments))
           (symbols (loop repeat (1- (length arguments)) collect (gensym "jsown-object"))))
      `(lambda (,@(first arguments) ,@symbols)
         ,@(if (rest arguments)
               `((let ,(loop for sym in symbols
                          for args in (rest arguments)
                          append (loop for arg in args
                                    collect `(,arg (and (find ,(string-downcase (string arg))
                                                              (jsown:keywords ,sym)
                                                              :test #'string=)
                                                        (jsown:val ,sym ,(string-downcase (string arg)))))))
                   ,@body))
               body)))))

(defmacro define-remote-procedure (name (&rest arguments) &body body)
  "defines a remote procedure with <name> as the name to be called and <arguments> as the assumed arguments.  if <name> is a symbol with only non- lower-case-p characters, then it is converted to lowercase."
  (flet ((translate-remote-procedure-name (name)
           (if (and (symbolp name)
                    (not (some #'lower-case-p (string name))))
               (string-downcase (string name))
               (string name))))
    `(register-remote-procedure
      ,(translate-remote-procedure-name name)
      ,(make-remote-procedure-lambda-function arguments body))))
(defun rid-active-p (rid &optional (my-active-rids (intercom-var 'rids)))
  "returns non-nil iff <rid> is active for the current user.  by use of the variable my-active-rids,
  the currently active rids can be overridden.  !only use when you know what you're doing!"
  (or (string= rid "")
      (find rid my-active-rids :test #'string=)))

(defun start-rid (rid)
  "sets <rid> to be active"
  (with-session-lock (!)
    (unless (rid-active-p rid)
      (! (push rid (intercom-var 'rids))))))

(defun remove-rid (rid)
  "removes the <rid> from the list of active rids"
  (with-session-lock (!)
    (! (alexandria:removef (intercom-var 'rids) rid :test #'string=))))

(defun in-active-remote-procedure-p ()
  "returns non-nil if we are currently in a remote procedure with an active rid."
  (and *store* *rid*
       (rid-active-p *rid*)
       (channel-activep)))

(defun activep ()
  "returns non-nil if we are currently in an active remote procedure.
  alias for in-active-remote-procedure-p."
  (in-active-remote-procedure-p))
(defun message (type body)
  "sends a message to the client"
  (with-session-lock (!)
    (if (in-active-remote-procedure-p)
        (let ((message (jsown:new-js
                         ("type" type)
                         ("rid" *rid*)
                         ("body" body))))
          (! (push message (intercom-var 'messages))))
        (warn "can't send messages if not in an active remote procedure"))))

(defun fetch-and-clear-messages ()
  "fetches and clears the messages in the mailbox"
  (with-session-lock (!)
    (let (messages my-active-rids)
      (!
       ;; fetch the list of messages
       (setf messages (intercom-var 'messages))
       (setf (intercom-var 'messages) nil)
       ;; correctly change the active rids
       (setf my-active-rids (intercom-var 'rids))
       (let ((rids-to-end (intercom-var 'rids-to-end)))
         (setf (intercom-var 'rids)
               (remove-if (lambda (rid)
                            (find rid rids-to-end :test #'string=))
                          (intercom-var 'rids))))
       (setf (intercom-var 'rids-to-end) nil))
      (delete-if-not (lambda (message)
                       (rid-active-p (jsown:val message "rid") my-active-rids))
                     (reverse messages)))))

(defun generate-id ()
  "we generate an id by taking the universal time and augmenting it by some random number"
  (let ((random-binary-digits 35)
        (universal-time-binary-digits 25))
    ;; let's only care about the last 20 digits of universal time, this gives us roughly one year to cycle
    (+ (* (mod (get-universal-time) (expt 2 universal-time-binary-digits))
          (expt 2 random-binary-digits))
       (random (expt 2 random-binary-digits)))))

(defun ensure-hydra-body (&optional refreshp)
  "creates a new session and session cookie, unless one was given to us that still exists"
  ;;---! this should check that that the session cookie really is a session and set it up
  (let ((hydra-cookie (hunchentoot:cookie-in "hydra")))
    (setf *hydra-body-id*
          (or hydra-cookie (s+ (generate-id))))
    ;;---! and setup the hydra structures in memory 
    (unless hydra-cookie
      nil ;;---! ensure hydra-session is setup in memory
      )
    (when (or refreshp (not hydra-cookie))
      (hunchentoot:set-cookie "hydra"
                              :value *hydra-body-id*
                              :http-only t
                              :expires (+ (get-universal-time)
                                          (* 60 60 24 30))))))
(defun ensure-hydra-head ()
  "ensures *hydra-head-id* is available.  also ensures the *hydra-body-id* is set.
  refreshes *hydra-body-id* when no hhid was found in the current request."
  (let* ((hhid (hunchentoot:get-parameter "hhid"))
         (*hydra-head-id* (s+ (or hhid (generate-id)))))
    (ensure-hydra-body hhid)
    ;;---! setup the datastructures for both the head and the body!
    (unless hhid
      (let ((*rid* ""))
        (message "hhid" *hydra-head-id*)))))

(hunchentoot:define-easy-handler (talk :uri "/talk") ()
  (in-intercom-session
    (watchdog)
    (ensure-hydra-head)
    (setf (hunchentoot:content-type*) "application/json")
    (let ((open (hunchentoot:parameter "open"))
          (close (hunchentoot:parameter "close")))
      (when open
        (dolist (request (jsown:parse open))
          (perform-intercom-request request))) ;; [{rid,method,args}]
      (when close
        (dolist (rid (jsown:parse close))
          (perform-close-request rid)))) ;; rids
    (jsown:to-json (fetch-and-clear-messages))))

(defun watchdog ()
  "indicates the client has phoned home"
  (setf (intercom-var 'watchdog)
        (get-universal-time)))

(defun channel-activep ()
  "returns non-nil iff the last message we received from the client isn't too long ago"
  (>= (+ (intercom-var 'watchdog) *watchdog-timeout*)
      (get-universal-time)))

(defun perform-intercom-request (jsown-request)
  "performs an intercom request as described by <jsown-request>."
  (apply #'call-remote-procedure
         (jsown:val jsown-request "rid")
         (jsown:val jsown-request "name")
         (jsown:val jsown-request "args")))
(defun perform-close-request (rid)
  "closes the request for the rid."
  (remove-rid rid))
