
(in-package :intercom)

(defgeneric touch (object)
  (:documentation "touches an object, updating the modification time."))

(defun s+ (&rest args)
  "pretty-prints and concatenates the resulting strings of each arg in <args>."
  (format nil "~{~A~}" args))

(defmacro with-doublequotes ((&rest variables) &body body)
  "makes sure each variable in variables is bound to itself, essentially allowing you 
  to write ,, in a double backtick to get that symbol.  handy for gensym."
  `(let ,(loop for var in variables
            collect `(,var ',var))
     ,@body))

(defmacro esc ((macro-symbol &key (test ''null) (return nil)) &body body)
  (with-gensyms (form-var block-var val test-var return-var)
    `(block ,block-var
       (let ((,test-var ,test)
             (,return-var ,return))
         (macrolet ((,macro-symbol (,form-var)
                      (with-doublequotes (,block-var ,val ,test-var ,return-var)
                        `(let ((,,val ,,form-var))
                           (if (funcall ,,test-var ,,val)
                               (return-from ,,block-var ,,return-var)
                               ,,val)))))
           ,@body)))))

(defun generate-id ()
  "we generate an id by taking the universal time and augmenting it by some random number"
  (let ((random-binary-digits 35)
        (universal-time-binary-digits 25))
    ;; let's only care about the last 20 digits of universal time,
    ;; this gives us roughly one year to cycle
    (+ (* (mod (get-universal-time) (expt 2 universal-time-binary-digits))
          (expt 2 random-binary-digits))
       (random (expt 2 random-binary-digits)))))(defmacro assert-nonempty-string (place)
  "asserts that <place> contains a non-empty string."
  `(assert (and (stringp ,place)
                (> (length ,place) 0))
           (,place)
           "~A must contain a nonempty string.  it contains ~A" ',place ,place))

(defmacro assert-eql-compatible (place)
  "asserts that place is an eql-compatible place.
  this means it must be one of:
  1. a symbol
  2. a character
  3. a number"
  `(assert (or (symbolp ,place)
               (numberp ,place)
               (characterp ,place))
           (,place)
           "~A must contain an object which may be eql-compatible when copied.  it contains ~A."
           ',place ,place))

(defmacro assert-hydra-head (place)
  "asserts that <place> contains a hydra-head"
  `(assert (hydra-head-p ,place)
           (,place)
           "~A must contain an object of type hydra-head.  it contains ~A." ',place ,place))

(defmacro assert-hydra-body (place)
  "asserts that <place> contains a hydra-body"
  `(assert (hydra-body-p ,place)
           (,place)
           "~A must contain an object of type hydra-body.  it contains ~A." ',place ,place))

(defmacro assert-hydra-session-validation (place)
  "asserts that <place> contains a hydra-session"
  `(assert (session-validation-p ,place)
           (,place)
           "~A must contain an object of type session-validation.  it contains ~A."
           ',place ,place))(defstruct key-value-store
  (lock (bordeaux-threads:make-recursive-lock "key-value-lock"))
  (hash (make-hash-table)))

(defmacro with-key-value-store-lock (store &body body)
  "executes body in an environment in which <store> is locked."
  `(bordeaux-threads:with-recursive-lock-held ((key-value-store-lock ,store))
     ,@body))

(defun kv-store-read (key store)
  "reads the key from store"
  (declare (type key-value-store store))
  (assert-eql-compatible key)
  (with-key-value-store-lock store
    (gethash key (key-value-store-hash store))))

(defun (setf kv-store-read) (value key store)
  "sets <key> in <store> to <value>"
  (declare (type key-value-store store))
  (assert-eql-compatible key)
  (with-key-value-store-lock store
    (setf (gethash key (key-value-store-hash store))
          value)))

(defparameter *remote-procedures*
  #+sbcl (make-hash-table :test 'equal :synchronized t)
  #-sbcl (make-hash-table :test 'equal)            
  "contains all intercom remote procedures, the keywords being the matched string and the values
  being the corresponding function.")

(defparameter *rid* nil
  "variable which contains the request id in functions which represent the execution of
  a remote procedure.")

(defparameter *hydra-body* nil
  "contains the hydra-body once we have a hydra-body in the current request")

(defparameter *hydra-head* nil
  "contains the hydra-head once we have one in the current request")

(defparameter *hydra-head-id* nil
  "represents the screen identifier of the current request")

(defparameter *hydra-head-timeout* 120
  "the time we have before we assume the head is detached.")

(defparameter *hydra-body-timeout* 86400
  "the time we have before we assume the session has ended.")

(defparameter *hydra-auth-store* (make-hash-table :test 'equal)
  "a hash-table linking each \"hydra\" cookie value to the authentication which belongs to it.")

(defparameter *hydra-auth-lock* (bordeaux-threads:make-lock "hydra-auth-lock")
  "this lock is used when accessing the hydra-auth-store")

(defparameter *current-thread-start-internal-runtime* nil
  "contains the internal runtime when this RPC call was started.")

(defparameter *auto-kill-rid* nil
  "when this is set to non-nil in a given thread (which is the default for
  call-remote-procedure, then the ending of the call to call-remote-procedure
  signals the end of the rid.  you can turn it on with #'auto-end-remote-procedure
  and turn it off with #'dont-auto-end-remote-procedure.")

(defparameter *log-request-stream* nil
  "when non-nil, this should be a character stream.  an s-expression based log is written to the
   stream containing the ping-times, the requests and the responses for each user.  the format is
   as follows: 
   - ping :: (ping utime hydra-body-id hydra-head-id ip-address)
   - request :: (req utime hydra-body-id hydra-head-id request-id request-name arg1..argn)
   - responses :: (res utime hydra-body-id hydra-head-id request-id time message-type message-value)
   - closes :: (close utime hydra-body-id hydra-head-id request-id)")(defmacro with-session-db-lock ((&optional (session '*hydra-body*)) &body body)
  "executes <body> with a lock on the datastore of hydra-body.
    this should be used when the new value is based on previous values in the session."
  `(with-key-value-store-lock (hydra-body-data ,session)
     ,@body))

(defmacro with-screen-db-lock ((&optional (session '*hydra-head*)) &body body)
  "executes <body> with a lock on the datastore of hydra-head.
      this should be used when the new value is based on previous values in the session."
  `(with-key-value-store-lock (hydra-head-data ,session)
     ,@body))

(defmacro with-local-screen-lock ((protection-symbol) &body body)
  "executes <body> in a piece of code in which the head's data is locked"
  `(progn
     (assert-session)
     (macrolet ((,protection-symbol (&body body)
                  `(with-screen-db-lock ()
                     ,@body)))
       ,@body)))

(defmacro in-intercom-session (&body body)
  "executes a hunchentoot request in an environment in which the special local variables are
  bound to be special and local.
  this contains:
  - *hydra-body*
  - *hydra-head*
  - *hydra-head-id*"
  `(let (*hydra-body* *hydra-head* *hydra-head-id*)
     ,@body))

(defun assert-session ()
  "asserts that we're currently running in an environment which is sane for intercom
  requests/executions"
  (assert-hydra-body *hydra-body*)
  (assert-hydra-head *hydra-head*))(defun register-remote-procedure (name function)
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

(defmacro threadable-lambda ((&rest arglist) &body body)
  "creates a lambda which can be threaded.  it locally binds the variables which
  are needed by intercom."
  (with-gensyms (hydra-body hydra-head rid time log-stream)
    `(let ((,hydra-body *hydra-body*)
           (,hydra-head *hydra-head*)
           (,time *current-thread-start-internal-runtime*)
           (,rid *rid*)
           (,log-stream *log-request-stream*))
       (lambda (,@arglist)
         ,(when (eq (caar body) 'declare)
                (car body))
         (let ((*hydra-body* ,hydra-body)
               (*hydra-head* ,hydra-head)
               (*rid* ,rid)
               (*current-thread-start-internal-runtime* ,time)
               (*log-request-stream* ,log-stream))
           ,@(if (eq (caar body) 'declare)
                 (rest body)
                 body))))))

(defun auto-end-remote-procedure-p ()
  "returns non-nil iff the end of the current remote-procedure indicates that the
  rid should be killed.  setfable place defaults to t."
  *auto-kill-rid*)

(defun (setf auto-end-remote-procedure-p) (value)
  "sets the killing of the remote-procedure to <value>.  non-nil indicates that
  the remote-procedure should be killed (the default), nil indicates the inverse."
  (setf *auto-kill-rid* value))

(defun call-remote-procedure (rid name &rest args)
  "calls the remote prodecure with name <name> and <args> as the arguments with <rid> as
  reference.  assumes the special variables *hydra-head* and *hydra-body* exist and
  respectively contain a hydra-head and a hydra-body."
  (assert (get-remote-procedure name))
  (bordeaux-threads:make-thread
   (let ((*rid* rid))
     (threadable-lambda ()
       (let ((*current-thread-start-internal-runtime* (get-internal-run-time))
             (*auto-kill-rid* t))
         (start-rid *rid*)
         (unwind-protect
              (apply (get-remote-procedure name) args)
           (when (auto-end-remote-procedure-p)
             (with-local-screen-lock (!)
               (! (push rid (screen-var 'rids-to-end)))))))))
   :initial-bindings (thread-initial-bindings)
   :name name))(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-remote-procedure-lambda-function (arguments body)
    "builds the s-expression representation for the lambda function which can be called for
    the definition of a remote procedure.  this handles the creation of the &key arguments."
    (let* ((arguments (split-sequence:split-sequence '&key arguments))
           (symbols (loop repeat (1- (length arguments)) collect (gensym "jsown-object"))))
      (labels (;; outputs code to bind <arg> to the value it has in <jsown-obj-var>.
               (make-jsown-key-sym-binding (jsown-obj-var arg)
               
                 (let ((jsown-key (string-downcase (string arg))))
                   `(,arg (and (find ,jsown-key
                                     (jsown:keywords ,jsown-obj-var)
                                     :test #'string=)
                               (jsown:val ,jsown-obj-var ,jsown-key)))))
               ;; makes the body of all let bindings for the &key arguments, the others
               ;; handled in the arguments the lambda function accepts
               (make-jsown-val-let-bindings ()
                 (loop for sym in symbols
                    for args in (rest arguments)
                    append (loop for arg in args
                              collect (make-jsown-key-sym-binding sym arg)))))
        `(lambda (,@(first arguments) ,@symbols)
           ,@(if (rest arguments)
                 `((let ,(make-jsown-val-let-bindings)
                     ,@body))
                 body))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun translate-remote-procedure-name (name)
    "translates the remote procedure name <name> to the remote procedure name as can be called
    from the javascript world."
    (if (and (symbolp name)
             (not (some #'lower-case-p (string name))))
        (string-downcase (string name))
        (string name))))

(defmacro define-remote-procedure (name (&rest arguments) &body body)
  "defines a remote procedure with <name> as the name to be called and <arguments> as the
   assumed arguments.  if <name> is a symbol with only non- lower-case-p characters,
   then it is converted to lowercase."
  `(register-remote-procedure
    ,(translate-remote-procedure-name name)
    ,(make-remote-procedure-lambda-function arguments body)))(defmacro with-error-as-fail (&body body)
  "executes <body> in an environment in which all errors are catched and sent as a message
  with type \"fail\" to the user."
  `(handler-case
       (progn ,@body)
     (error (err)
       (message "fail" (format nil "~A" err)))))(defun rid-active-p (rid &optional (my-active-rids (screen-var 'rids)))
  "returns non-nil iff <rid> is active for the current user.  by use of the variable my-active-rids,
  the currently active rids can be overridden.  !only use when you know what you're doing!"
  (or (string= rid "")
      (find rid my-active-rids :test #'string=)))

(defun start-rid (rid)
  "sets <rid> to be active"
  (with-local-screen-lock (!)
    (unless (rid-active-p rid)
      (! (push rid (screen-var 'rids))))))

(defun remove-rid (rid)
  "removes the <rid> from the list of active rids"
  (with-local-screen-lock (!)
    (! (alexandria:removef (screen-var 'rids) rid :test #'string=))))

(defun in-active-remote-procedure-p ()
  "returns non-nil if we are currently in a remote procedure with an active rid."
  (and *hydra-body* *hydra-head*
       *rid*
       (rid-active-p *rid*)
       (hydra-head-active-p *hydra-head*)
       (hydra-body-active-p *hydra-body*)))

(defun activep ()
  "returns non-nil if we are currently in an active remote procedure.
  alias for in-active-remote-procedure-p."
  (in-active-remote-procedure-p))(defun message (type body)
  "sends a message to the client"
  (with-local-screen-lock (!)
    (if (in-active-remote-procedure-p)
        (let ((message (jsown:new-js
                         ("type" type)
                         ("rid" *rid*)
                         ("time" (if *current-thread-start-internal-runtime*
                                     (coerce (/ (- (get-internal-run-time)
                                                   *current-thread-start-internal-runtime*)
                                                internal-time-units-per-second)
                                             'float)
                                     "infinity"))
                         ("body" body))))
          (! (push message (screen-var 'messages))))
        (warn "can't send messages if not in an active remote procedure"))))

(defun fetch-and-clear-messages ()
  "fetches and clears the messages in the mailbox"
  (with-local-screen-lock (!)
    (let (messages my-active-rids)
      (!
       ;; fetch the list of messages
       (setf messages (screen-var 'messages))
       (setf (screen-var 'messages) nil)
       ;; correctly change the active rids
       (setf my-active-rids (screen-var 'rids))
       (let ((rids-to-end (screen-var 'rids-to-end)))
         (setf (screen-var 'rids)
               (remove-if (lambda (rid)
                            (find rid rids-to-end :test #'string=))
                          (screen-var 'rids))))
       (setf (screen-var 'rids-to-end) nil))
      (delete-if-not (lambda (message)
                       (rid-active-p (jsown:val message "rid") my-active-rids))
                     (reverse messages)))))

(defmacro with-hydra-auth-store-lock (&body body)
  "executes <body> in an environment in which *hydra-auth-store* is locked."
  `(bordeaux-threads:with-lock-held (*hydra-auth-lock*)
     ,@body))

(defun store-hydra-validation (session-validation)
  "stores the hydra session-validation so it can be found back."
  (assert-hydra-session-validation session-validation)
  (assert-hydra-body (session-validation-hydra-body session-validation))
  (with-hydra-auth-store-lock
    (push session-validation
          (gethash (session-validation-hydra-id session-validation)
                   *hydra-auth-store*))))

(defun retrieve-hydra-validations (hydra-id)
  "returns all hydra session-varlidation instance which belong to the given hydra-id"
  (assert-nonempty-string hydra-id)
  (with-hydra-auth-store-lock
    (gethash hydra-id
             *hydra-auth-store*)))

(defun remove-hydra-validation (session-validation)
  "removes the session-validation <session-validation> from the known validations."
  (assert-hydra-session-validation session-validation)
  (with-hydra-auth-store-lock
    ;;---! do something smart with a counter in the hydra-body here so we know the hydra-body
    ;;     should be terminated too
    (remhash (session-validation-hydra-id session-validation)
             *hydra-auth-store*)))(defun ensure-hydra ()
  "ensures the hydra is set up.  this means that:
  - after this function execution:
    - *hydra-head* is bound to the hydra's head
    - *hydra-body* is bound to the hydra's body
    - *hydra-auth-store* contains an authentication for continued storage
  - after this request:
    - the user has a \"hydra\" cookie which links to this session."
  (unless (discover-hydra)
    (let ((hydra-body (build-active-hydra-body)))
      (ensure-hydra-head hydra-body)
      (build-active-authentication hydra-body))))

(defun build-active-hydra-body ()
  "builds a new hydra-body and sets it as the current hydra-body."
  (setf *hydra-body* (make-hydra-body)))

(defun build-active-authentication (hydra-body)
  "builds a new authentication, which identifies hydra-body, and stores it
  in the necessary structures."
  (let ((id (s+ (generate-id))))
    (store-hydra-validation (make-session-validation hydra-body id))
    ;;---! this cookie is never updated.  it should be updated every so often so we can
    ;;     keep the session alive
    (hunchentoot:set-cookie "hydra"
                            :value id
                            :http-only t
                            :expires (+ (get-universal-time)
                                        (* 60 60 24 30)))))

(defun discover-hydra ()
  "discover-hydra returns non-nil iff we had a session-cookie through which we could
  find a hydra session-validation which is valid for our current session.
  this function sets up all special variables for the hydra to be happy.  it also
  touches the hydra-body and the relevant hydra-head so we're active."
  (when (discover-hydra-body)
    (ensure-hydra-head *hydra-body*)))

(defun discover-hydra-body ()
  "returns non-nil iff we had a session-cookie through which we could
  find a hydra session-validation which is valid for our current session.
  this function sets up all special variables for the hydra-body,
  but leaves the hydra-head for another solution to figure out."
  (esc (^)
    (setf *hydra-body*
          (session-validation-hydra-body
           (let ((cookie (^ (hunchentoot:cookie-in "hydra"))))
             (^ (find-if (rcurry #'valid-session-p cookie)
                         (retrieve-hydra-validations cookie))))))
    (touch *hydra-body*)
    *hydra-body*))

(defun ensure-hydra-head (hydra-body)
  "ensures the hydra-head exists and is set in the variable *hydra-head*.
  assumes *hydra-body* is set.  returns the current hydra-head."
  (multiple-value-bind (hhid need-to-send-hhid-p)
      (ensure-hhid)
    (let ((hydra-head (find hhid (hydra-body-heads hydra-body)
                            :test #'string= :key #'hydra-head-id)))
      (if hydra-head
          (progn
            (touch hydra-head)
            (setf *hydra-head* hydra-head))
          (let ((new-head (make-hydra-head :id hhid)))
            (setf *hydra-head* new-head)
            (push new-head (hydra-body-heads hydra-body)))))
    (when need-to-send-hhid-p
      (send-current-hhid)))
  *hydra-head*)

(defun ensure-hhid ()
  "returns the hhid if one was given as a get-variable, or creates a new hhid.
  does *not* put the hhid on the message stack.
  returns (values hhid newp).  if newp is t, a message should be sent to the
  client (see (send-current-hhid)) so the client knows the hhid."
  (let (resend-p)
    (values
     (or (let ((special *hydra-head-id*))
           special)
         (let ((get (hunchentoot:get-parameter "hhid")))
           (setf *hydra-head-id* get)
           get)
         (let ((new (s+ (generate-id))))
           (setf *hydra-head-id* new)
           (setf resend-p t)
           new))
     resend-p)))

(defun send-current-hhid ()
  "sends the current hhid to the client by using the correct intercom message.
  requires that *hydra-head* and *hydra-head-id* are set correctly."
  (assert-hydra-head *hydra-head*)
  (assert-nonempty-string *hydra-head-id*)
  (let ((*rid* ""))
    (message "hhid" *hydra-head-id*)))(defstruct hydra-body
  (data (make-key-value-store))
  (atime (get-universal-time))
  (heads nil)
  (gc-callbacks nil)
  (garbage-collected-body-p nil))

(defmethod touch ((hydra hydra-body))
  (setf (hydra-body-atime hydra)
        (get-universal-time)))

(defun gc-hydra-body (hydra-body)
  "garbage-collects a hydra-body"
  (dolist (head (hydra-body-heads hydra-body))
    (gc-hydra-head head))
  (dolist (callback (hydra-body-gc-callbacks hydra-body))
    (funcall callback))
  (setf (hydra-body-garbage-collected-body-p hydra-body) t))

(defun session-var (key &optional (session *hydra-body*))
  "returns the value of <key> which belongs to <session>, or nil if it didn't exist.
  the second value is non-nil iff <key> was found in <session>."
  (kv-store-read key (hydra-body-data session)))

(defun (setf session-var) (value key &optional (session *hydra-body*))
  "sets the value of ,key> which belongs to <session> to <value>."
  (setf (kv-store-read key (hydra-body-data session)) value))

(defun attach-head (hydra-body hydra-head)
  "attaches <hydra-head> to <hydra-body>"
  (assert-hydra-body hydra-body)
  (assert-hydra-head hydra-head)
  (push hydra-head (hydra-body-heads hydra-body)))

(defun hydra-body-active-p (hydra)
  "returns non-nil iff the <hydra> hasn't been touched for too long of a time."
  (and (not (hydra-body-garbage-collected-body-p hydra))
       (> (+ (hydra-body-atime hydra) *hydra-body-timeout*)
          (get-universal-time))))

(defun add-session-gc-callback (function &optional (session *hydra-body*))
  "adds <function> to the list of functions to call on garbage collection of <session>."
  (with-session-db-lock ()
    (push function (hydra-body-gc-callbacks session))))

(defun remove-session-gc-callback (function &optional (session *hydra-body*))
  "removes <function> from the list of functions to call on the garbage collection of <session>."
  (with-session-db-lock ()
    (removef (hydra-body-gc-callbacks session)
             function)))
(defstruct hydra-head
  (id nil)
  (data (make-key-value-store))
  (atime (get-universal-time))
  (gc-callbacks nil)
  (garbage-collected-body-p nil))

(defmethod touch ((hydra hydra-head))
  (setf (hydra-head-atime hydra)
        (get-universal-time)))

(defun gc-hydra-head (hydra-head)
  "garbage-collects a hydra-head"
  (dolist (callback (hydra-head-gc-callbacks hydra-head))
    (funcall callback))
  (setf (hydra-head-garbage-collected-body-p hydra-head) t))

(defun screen-var (key &optional (screen *hydra-head*))
  "returns the value of <key> which belongs to <screen>, or nil if it didn't exist.
    the second value is non-nil iff <key> was found in <screen>."
  (kv-store-read key (hydra-head-data screen)))

(defun (setf screen-var) (value key &optional (screen *hydra-head*))
  "sets the value of ,key> which belongs to <screen> to <value>."
  (setf (kv-store-read key (hydra-head-data screen)) value))

(defun hydra-head-active-p (hydra)
  "returns non-nil iff the <hydra> hasn't been touched for too long of a time."
  (and (not (hydra-head-garbage-collected-body-p hydra))
       (> (+ (hydra-head-atime hydra) *hydra-head-timeout*)
          (get-universal-time))))

(defun add-screen-gc-callback (function &optional (screen *hydra-head*))
  "adds <function> to the list of functions to call on garbage collection of <screen>."
  (with-session-db-lock ()
    (push function (hydra-body-gc-callbacks session))))

(defun remove-screen-gc-callback (function &optional (screen *hydra-head*))
  "removes <function> from the list of functions to call on the garbage collection of <screen>."
  (with-session-db-lock ()
    (removef (hydra-body-gc-callbacks session)
             function)))
(defstruct (session-validation (:constructor mk-session-validation))
  (hydra-id "" :type string)
  (host "" :type string)
  (user-agent "" :type string)
  (hydra-body nil :type (or hydra-body null)))

(defun valid-session-p (session-validation cookie-identifier)
  "validates the session-validation for the current request"
  (and (string= cookie-identifier (session-validation-hydra-id session-validation))
       (string= (hunchentoot:host) (session-validation-host session-validation))
       (string= (hunchentoot:user-agent) (session-validation-user-agent session-validation))))

(defun make-session-validation (hydra-body cookie-identifier)
  "constructs a new session-validation object for the current session."
  (mk-session-validation :hydra-id cookie-identifier
                         :hydra-body hydra-body
                         :host (hunchentoot:host)
                         :user-agent (hunchentoot:user-agent)))

(hunchentoot:define-easy-handler (talk :uri "/talk") ()
  (in-intercom-session
    (ensure-hydra)
    (setf (hunchentoot:content-type*) "application/json")
    (ping-logging)
    (let ((open (hunchentoot:parameter "open"))
          (close (hunchentoot:parameter "close")))
      (when open
        (let ((open-requests (jsown:parse open)))
          (request-logging open-requests)
          (dolist (request open-requests)
            (perform-intercom-request request)))) ;; [{rid,method,args}]
      (when close
        (let ((close-requests (jsown:parse close)))
          (close-request-logging close-requests)
          (dolist (rid close-requests)
            (perform-close-request rid))))) ;; rids
    (let ((messages (fetch-and-clear-messages)))
      (response-logging messages)
      (jsown:to-json messages))))(defparameter *log-stream-lock* (bordeaux-threads:make-lock "intercom-log-stream-lock")
  "this lock should be held when writing to the log stream.")

(defun make-log-message-string (type &rest args)
  "constructs a log message string for type and the followed arguments."
  (with-output-to-string (out)
    (write `(,type
             ,(get-universal-time)
             ,(hunchentoot:cookie-in "hydra")
             ,(hydra-head-id *hydra-head*)
             ,@args)
           :stream out :readably t :right-margin 180)))

(defmacro maybe-log-request (&body body)
  "executes <body> in an environment where the <log*> function is defined iff *log-request-stream*
   is non-nil.  the <log*> function takes the type of content to log, followed by the content itself
   by &rest and it splices hydra-body-id hydra-head-id in that list."
  (alexandria:with-gensyms (strings-var)
    `(when *log-request-stream*
       (let (,strings-var)
         (flet ((log* (type &rest args)
                  (push (princ-to-string (apply #'make-log-message-string type args))
                        ,strings-var)))
           ,@body
           (when ,strings-var
             (bordeaux-threads:with-lock-held (*log-stream-lock*)
               (format *log-request-stream* "~{~A~%~}" (reverse ,strings-var)))))))))

(defun ping-logging ()
  "handles the logging of the ping request"
  (maybe-log-request
    (log* :ping (hunchentoot:real-remote-addr))))

(defun request-logging (requests)
  "handles the logging of the new requests"
  (maybe-log-request
    (dolist (r requests)
      (apply #'log* :req
             (handler-case (jsown:val r "rid")
               (error () nil))
             (handler-case (jsown:val r "name")
               (error () nil))
             (handler-case (jsown:val r "args")
               (error () nil))))))

(defun close-request-logging (requests)
  "handles the logging of close requests"
  (maybe-log-request
    (dolist (rid requests)
      (log* :close rid))))

(defun response-logging (responses)
  "handles the logging of the responses"
  (maybe-log-request
    (dolist (r responses)
      (log* :res
            (jsown:val r "rid")
            (jsown:val r "time")
            (jsown:val r "type")
            (jsown:val r "body")))))


(defun hash-keys (hash)
  "returns a list of all hash-keys in <hash>"
  (loop for k being the hash-keys of hash collect k))

(defun gc-hydra-bodies ()
  "garbage collect the head hydras.  this removes the session-validation objects
  and removes the head heads."
  (bordeaux-threads:with-lock-held (*hydra-auth-lock*)
    (loop for k in (hash-keys *hydra-auth-store*)
       for validations =
         (remove-if-not (lambda (session-validation)
                          (let* ((hydra-body (session-validation-hydra-body
                                              session-validation))
                                 (activep (hydra-body-active-p hydra-body)))
                            (unless activep
                              ;; we need to decide what the throw away at this
                              ;;  time to ensure we don't forget to gc
                              (gc-hydra-body hydra-body))
                            activep))
                        (gethash k *hydra-auth-store*))
       if validations
       do 
         (setf (gethash k *hydra-auth-store*)
               validations)
         (mapcar (compose #'gc-hydra-heads #'session-validation-hydra-body)
                 validations)
       else
       do
         (remhash k *hydra-auth-store*))))

(defun gc-hydra-heads (hydra-body)
  "detaches the dead heads from <hydra-body>."
  ;;---! assumes hydra-body is locked by us
  (assert-hydra-body hydra-body)
  (let* ((new-heads (remove-if-not (lambda (head)
                                     (let ((activep (hydra-head-active-p head)))
                                       ;; we need to inline the garbage collection
                                       ;;  otherwise we may miss one somehow (though unlikely)
                                       (unless activep
                                         (gc-hydra-head head))
                                       activep))
                                   (hydra-body-heads hydra-body))))
    (setf (hydra-body-heads hydra-body)
          new-heads)))

(bordeaux-threads:make-thread
 (let ((store *hydra-auth-store*)
       (lock *hydra-auth-lock*))
  (lambda ()
    (let ((*hydra-auth-store* store)
          (*hydra-auth-lock* lock))
      (loop do
           (sleep 1800) ;; we run every 30 minutes
           (gc-hydra-bodies)))))
 :name "hydras garbage collection thread")

(defun perform-intercom-request (jsown-request)
  "performs an intercom request as described by <jsown-request>."
  (apply #'call-remote-procedure
         (jsown:val jsown-request "rid")
         (jsown:val jsown-request "name")
         (jsown:val jsown-request "args")))(defun perform-close-request (rid)
  "closes the request for the rid."
  (remove-rid rid))
