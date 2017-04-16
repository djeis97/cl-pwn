;;;; cl-pwn.lisp

(in-package #:cl-pwn)

;;; "cl-pwn" goes here. Hacks and glory await!

(defun remote (host port &rest args &key (external-format :utf-8))
  (let ((sock (socket-stream
               (apply 'socket-connect host port :element-type '(unsigned-byte 8)
                      args))))
    (make-instance 'tube :stream sock
                         :external-format (make-external-format external-format))))

(defun process (command &rest args &key (external-format :utf-8))
  (let ((proc (apply 'uiop:launch-program command :input :stream
                                                  :output :stream
                                                  :error :output
                                                  :element-type '(unsigned-byte 8)
                                                  args)))
    (make-instance 'tube :stream (make-two-way-stream (uiop:process-info-output proc)
                                                      (uiop:process-info-input proc))
                         :external-format (make-external-format external-format))))


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

(defclass tube (timeout flexi-io-stream)
  ((newline
    :initarg :newline
    :initform (string #\Newline))
   (input-stream-stack :initform nil))
  (:default-initargs
   :element-type 'character
   :external-format :utf-8))

(defmethod close :after ((s tube) &key abort)
  (with-slots (input-stream-stack) s
    (dolist (ps input-stream-stack)
      (close ps :abort abort))))

(defmethod stream-file-position ((st tube)))
(defmethod (setf stream-file-position) (val (st tube)))

(defun peak-char-no-hang (&optional
                            (stream *standard-input*)
                            (eof-error-p t)
                            eof-val recursive-p)
  (declare (ignore recursive-p))
  (let ((c (read-char-no-hang stream nil :eof t)))
    (cond
      ((characterp c)
       (unread-char c stream)
       c)
      ((eql c :eof)
       (if eof-error-p
           (error "EOF on stream ~A" stream)
           eof-val)))))

(defun clean-stream-stack (tube)
  "Removes any streams from the input stack at eof"
  (with-slots (input-stream-stack) tube
    (setf input-stream-stack (remove-if-not #'listen input-stream-stack))))

(defmethod stream-read-byte ((st tube))
  (with-slots (input-stream-stack) st
    (clean-stream-stack st)
    (if input-stream-stack
        (stream-read-byte (car input-stream-stack))
        (call-next-method))))

(defmethod stream-clear-input :after ((st tube))
  (with-slots (input-stream-stack) st
    (setf input-stream-stack nil)))

(defmethod stream-listen ((st tube))
  (with-slots (input-stream-stack) st
    (clean-stream-stack st)
    (if input-stream-stack
        (stream-listen (car input-stream-stack))
        (call-next-method))))

(defmethod stream-unread-char ((st tube) char)
  (error "Cannot unread chars, unfortunately"))

(defmethod stream-read-char ((st tube))
  (with-slots (input-stream-stack) st
    (clean-stream-stack st)
    (if input-stream-stack
        (stream-read-char (car input-stream-stack))
        (call-next-method))))

(defmethod unread-byte (byte (st tube))
  (error "Cannot unread bytes, unfortunately"))

(defmethod stream-read-sequence ((st tube) seq start end &key)
  (with-slots (input-stream-stack) st
    (clean-stream-stack st)
    (do ((stack input-stream-stack (cdr input-stream-stack))
         (idx start (read-sequence seq (car stack) :start idx :end end)))
        ((or (null stack) (= end start))
         (call-next-method st seq idx end)))))

(defmethod stream-peek-char ((st tube))
  (clean-stream-stack st)
  (with-slots (input-stream-stack) st
    (if input-stream-stack
        (stream-peek-char (car input-stream-stack))
        (call-next-method))))

(defmethod peek-byte ((st tube) &optional peek-type (eof-error-p t) eof-value)
  (clean-stream-stack st)
  (with-slots (input-stream-stack) st
    (if input-stream-stack
        (peek-byte (car input-stream-stack) peek-type eof-error-p eof-value)
        (call-next-method))))

(defmethod unread-sequence ((st tube) seq)
  (with-slots (input-stream-stack) st
    (with-accessors ((ext-format flexi-stream-external-format)) st
      (let ((octets (etypecase seq
                      (string (string-to-octets seq :external-format ext-format))
                      (vector seq)
                      (sequence (coerce seq 'vector)))))
        (push (make-flexi-stream (make-in-memory-input-stream octets)
                                 :external-format ext-format)
              input-stream-stack)))))

(defmethod stream-read-line ((st tube))
  (with-slots (input-stream-stack newline) st
    (let ((a (make-array 0 :element-type 'character
                           :adjustable t
                           :fill-pointer 0))
          (completed nil))
      (unwind-protect
           (loop :do (vector-push-extend (read-char st) a)
                 :until (search newline a)
                 :finally (progn (loop :repeat (length newline) :do (vector-pop a))
                                 (setf completed t)
                                 (return a)))
        (unless completed (unread-sequence st a))))))

(defmethod stream-write-char ((st tube) (char (eql #\Newline)))
  (stream-terpri st))

(defmethod stream-terpri ((st tube))
  (with-slots (newline stream) st
    (stream-write-string st newline)))
