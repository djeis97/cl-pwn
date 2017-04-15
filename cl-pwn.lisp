;;;; cl-pwn.lisp

(in-package #:cl-pwn)

;;; "cl-pwn" goes here. Hacks and glory await!

(defun remote (host port)
  (socket-stream (socket-connect host port)))


;;; Timeouts

(defparameter *default-timeout* 0)
(defconstant +timeout-max+ (expt 2 20))

(deftype timeout-spec ()
  `(or (member :default :forever) (real 0)))

(defclass timeout ()
  ((timeout :documentation "Seconds before timeout")
   (stop :initform nil :documentation "Internal time when countdown finishes"))
  (:default-initargs
   :timeout :default))

(defun timeout-spec->seconds (timeout-spec)
  (declare (timeout-spec timeout-spec))
  (case timeout-spec
    (:default *default-timeout*)
    (:forever +timeout-max+)
    (t timeout-spec)))

(defmethod initialize-instance :after ((ti timeout) &key timeout &allow-other-keys)
  (setf (slot-value ti 'timeout) (timeout-spec->seconds timeout)))

(defgeneric timeout-of (ti))
(defmethod timeout-of ((ti timeout))
  (with-slots (timeout stop) ti
    (cond
      (stop
       (/ (max (- stop (get-internal-real-time)) 0)
          internal-time-units-per-second))
      ((= timeout +timeout-max+) :forever)
      (t timeout))))
(defgeneric (setf timeout-of) (val ti))
(defmethod (setf timeout-of) (val (ti timeout))
  (declare (timeout-spec val))
  (with-slots (stop timeout) ti
    (assert (null stop))
    (setf timeout (timeout-spec->seconds val))))

(defmethod countdown-active (ti)
  (with-slots (stop) ti
    (or (null stop) (< (get-internal-real-time) stop))))

(defmethod call-with-countdown-dispatch (thunk (ti timeout) new-timeout)
  (with-slots (timeout stop) ti
    (let* ((old-timeout timeout)
           (old-stop stop)
           (new-stop (+ (get-internal-real-time)
                        (* new-timeout internal-time-units-per-second))))
      (unwind-protect
           (progn
             (psetf timeout new-timeout
                    stop (if old-stop
                             (min new-stop old-stop)
                             new-stop))
             (funcall thunk))
        (psetf timeout old-timeout
               stop old-stop)))))

(declaim (inline call-with-countdown))
(defun call-with-countdown (thunk obj &optional (new-timeout :default))
  (with-slots (timeout) obj
    (if (or (eql new-timeout :forever)
            (and (eql new-timeout :default)
                 (= timeout +timeout-max+)))
        (funcall thunk)
        (call-with-countdown-dispatch thunk obj
                                      (if (eql new-timeout :default)
                                          timeout
                                          new-timeout)))))

(defmacro with-countdown ((obj &optional (new-timeout :default)) &body body)
  `(call-with-countdown (lambda () ,@body) ,obj ,new-timeout))

(defmethod call-with-local-dispatch (thunk (ti timeout) new-timeout)
  (with-slots (timeout stop) ti
    (let ((old-timeout timeout)
          (old-stop stop))
      (unwind-protect
           (progn
             (psetf (timeout-of ti) new-timeout
                    stop nil)
             (funcall thunk))
        (psetf (timeout-of ti) old-timeout
               stop old-stop)))))

(declaim (inline call-with-local))
(defun call-with-local (thunk obj new-timeout)
  (if (or (eql new-timeout :default)
          (eql new-timeout (slot-value obj 'timeout)))
      (funcall thunk)
      (call-with-local-dispatch thunk obj new-timeout)))

(defmacro with-local ((obj new-timeout) &body body)
  `(call-with-local (lambda () ,@body) ,obj ,new-timeout))


;;; tubes

(defclass tube ()
  ((default-timeout
    :initarg default-timeout
    :initform nil)
   (newline
    :initarg newline
    :initform (coerce #\linefeed 'string))))
