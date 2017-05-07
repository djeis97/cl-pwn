(in-package #:cl-pwn/tubes)



(defclass tube-mixin (timeout)
  ((newline :initarg :newline
            :initform (string #\Newline)
            :accessor tube-newline)))



(defclass input-tube (tube-mixin)
  ((input-stream-stack :initform nil)))
(defmethod close :after ((st input-tube) &key abort)
  (with-slots (input-stream-stack) st
    (dolist (ps input-stream-stack)
      (close ps :abort abort))))
(defmethod clean-stream-stack ((st input-tube))
  "Removes any streams from the input stack at eof"
  (with-slots (input-stream-stack) st
    (let ((inpss input-stream-stack))
      (setf input-stream-stack (remove-if-not #'listen inpss))
      (and inpss (eql inpss input-stream-stack)))))
(defmethod stream-clear-input :after ((st input-tube))
  (setf (slot-value st 'input-stream-stack) nil))
(defmethod stream-listen :around ((st input-tube))
  (with-slots (input-stream-stack) st
    (clean-stream-stack st)
    (if input-stream-stack
        (stream-listen (car input-stream-stack))
        (call-next-method))))
(defmethod stream-read-sequence :around ((st input-tube) seq start end &key)
  (with-slots (input-stream-stack char-unreadable byte-unreadable) st
    (clean-stream-stack st)
    (setf char-unreadable nil byte-unreadable nil)
    (do ((stack input-stream-stack (cdr input-stream-stack))
         (idx start (read-sequence seq (car stack) :start idx :end end)))
        ((or (null stack) (= end start))
         (call-next-method st seq idx end)))))



(defclass char-input-tube (input-tube)
  (char-unreadable))
(defmethod stream-element-type ((st char-input-tube))
  (if (next-method-p)
      (call-next-method)
      'character))
(defmethod stream-read-char ((st char-input-tube))
  (if (next-method-p)
      (call-next-method)
      (error "tube-mixin not combined with stream")))
(defmethod stream-read-char :around ((st char-input-tube))
  (with-slots (input-stream-stack char-unreadable) st
    (clean-stream-stack st)
    (prog1
        (if input-stream-stack
            (read-char (car input-stream-stack))
            (call-next-method))
      (setf char-unreadable t))))
(defmethod stream-read-line ((st char-input-tube))
  (error "Tube should never get here"))
(defmethod stream-read-line :around ((st char-input-tube))
  (with-slots (char-unreadable input-stream-stack newline) st
    (setf char-unreadable nil)
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
(defmethod clean-stream-stack ((st char-input-tube))
  (with-slots (char-unreadable) st
    (let ((cleaned (call-next-method)))
      (unless cleaned
        (setf char-unreadable nil))
      cleaned)))
(defmethod stream-clear-input :after ((st char-input-tube))
  (with-slots (input-stream-stack char-unreadable byte-unreadable) st
    (setf input-stream-stack nil)))
(defmethod stream-unread-char :around ((st char-input-tube) char)
  (with-slots (input-stream-stack char-unreadable) st
    (prog1
        (if char-unreadable
            (if input-stream-stack
                (unread-char char (car input-stream-stack))
                (call-next-method))
            (error "Tried to unread a char when this is impossible"))
      (setf char-unreadable nil))))
(defmethod stream-peek-char :around ((st char-input-tube))
  (clean-stream-stack st)
  (with-slots (input-stream-stack) st
    (if input-stream-stack
        (stream-peek-char (car input-stream-stack))
        (call-next-method))))
(defmethod unread-sequence ((st char-input-tube) seq)
  (with-slots (input-stream-stack char-unreadable) st
    (setf char-unreadable nil)
    (let ((string (etypecase seq
                    (string seq)
                    (sequence (coerce seq 'string)))))
      (push (make-string-input-stream string)
            input-stream-stack))))
(defmethod stream-read-char-no-hang :around ((st char-input-tube))
  (with-slots (input-stream-stack char-unreadable) st
    (clean-stream-stack st)
    (prog1
        (if input-stream-stack
            (read-char-no-hang (car input-stream-stack))
            (call-next-method))
      (setf char-unreadable t))))



(defclass byte-input-tube (input-tube)
  (byte-unreadable))
(defmethod stream-element-type ((st byte-input-tube))
  (if (next-method-p)
      (call-next-method)
      '(unsigned-byte 8)))
(defmethod clean-stream-stack ((st byte-input-tube))
  (with-slots (byte-unreadable) st
    (let ((cleaned (call-next-method)))
      (unless cleaned
        (setf byte-unreadable nil))
      cleaned)))
(defmethod peak-byte :around ((st byte-input-tube) &optional
                                                     peek-type
                                                     (eof-error-p t)
                                                     eof-value)
  (clean-stream-stack st)
  (with-slots (input-stream-stack) st
    (if input-stream-stack
        (peek-byte (car input-stream-stack) peek-type eof-error-p eof-value)
        (call-next-method))))
(defmethod stream-clear-input :after ((st byte-input-tube))
  (setf (slot-value st 'byte-unreadable) nil))
(defmethod stream-read-byte :around ((st byte-input-tube))
  (with-slots (input-stream-stack byte-unreadable) st
    (clean-stream-stack st)
    (prog1
        (if input-stream-stack
            (stream-read-byte (car input-stream-stack))
            (call-next-method))
      (setf byte-unreadable t))))
(defmethod unread-byte (byte (st byte-input-tube))
  (if (next-method-p)
      (call-next-method)
      (error "tube-mixin not combined with stream")))
(defmethod unread-byte :around (byte (st byte-input-tube))
  (with-slots (input-stream-stack byte-unreadable) st
    (prog1
        (if byte-unreadable
            (if input-stream-stack
                (unread-byte byte (car input-stream-stack))
                (call-next-method))
            (error "Tried to unread a byte when this is impossible"))
      (setf byte-unreadable nil))))
(defmethod unread-sequence ((st byte-input-tube) seq)
  (with-slots (input-stream-stack char-unreadable byte-unreadable) st
    (setf byte-unreadable nil)
    (let ((octets (etypecase seq
                    (vector seq)
                    (sequence (coerce seq 'vector)))))
      (push (make-in-memory-input-stream octets)
            input-stream-stack))))



(defclass output-tube (tube-mixin) ())



(defclass char-output-tube (output-tube) ())
(defparameter *recursive-newline-write* nil)
(defmethod stream-write-char :around ((st char-output-tube) (char (eql #\Newline)))
  (if *recursive-newline-write*
      (call-next-method)
      (stream-terpri st)))
(defmethod stream-terpri :around ((st char-output-tube))
  (with-slots (newline stream) st
    (let ((*recursive-newline-write* t))
      (stream-write-string st newline))))



(defclass byte-output-tube (output-tube) ())



(defclass bivalent-input-tube (char-input-tube byte-input-tube) ())
(defmethod stream-read-line :around ((st bivalent-input-tube))
  (prog1
      (call-next-method)
    (with-slots (byte-unreadable) st
      (setf byte-unreadable nil))))
(defmethod stream-read-char :around ((st bivalent-input-tube))
  (prog1
      (call-next-method)
    (with-slots (byte-unreadable) st
      (setf byte-unreadable nil))))
(defmethod stream-read-byte :around ((st bivalent-input-tube))
  (prog1
      (call-next-method)
    (with-slots (char-unreadable) st
      (setf char-unreadable nil))))
(defmethod unread-sequence ((st bivalent-input-tube) seq)
  (with-slots (input-stream-stack char-unreadable byte-unreadable) st
    (setf char-unreadable nil byte-unreadable nil)
    (let ((octets (etypecase seq
                    (string (string-to-octets seq :external-format :utf-8))
                    (vector seq)
                    (sequence (coerce seq 'vector)))))
      (push (make-flexi-stream (make-in-memory-input-stream octets)
                               :external-format :utf-8)
            input-stream-stack))))



(defclass bivalent-output-tube (char-output-tube byte-output-tube) ())



(defclass bivalent-io-tube (bivalent-output-tube bivalent-input-tube) ())



(defclass wrapping-char-input-tube (char-input-tube)
  ((wrapped-stream :initarg :stream))
  (:default-initargs
   :stream (make-string-input-stream "")))
(defmethod stream-clear-input ((st wrapping-char-input-tube))
  (clear-input (slot-value st 'wrapped-stream)))
(defmethod close ((st wrapping-char-input-tube) &key abort)
  (close (slot-value st 'wrapped-stream) :key abort))
(defmethod stream-read-sequence ((st wrapping-char-input-tube) seq start end &key)
  (read-sequence (slot-value st 'wrapped-stream) seq :start start :end end))
(defmethod stream-peek-char ((st wrapping-char-input-tube))
  (peek-char nil st nil :eof t))
(defmethod stream-read-char-no-hang ((st wrapping-char-input-tube))
  (read-char-no-hang (slot-value st 'wrapped-stream) nil :eof t))
(defmethod stream-listen ((st wrapping-char-input-tube))
  (listen (slot-value st 'wrapped-stream)))
(defmethod stream-unread-char ((st wrapping-char-input-tube) char)
  (unread-char char (slot-value st 'wrapped-stream)))



(defclass wrapping-char-output-tube (char-output-tube)
  ((wrapped-stream :initarg :stream))
  (:default-initargs
   :stream (make-string-output-stream)))
(defmethod stream-clear-output ((st wrapping-char-output-tube))
  (clear-output (slot-value st 'wrapped-stream)))
(defmethod stream-finish-output ((st wrapping-char-output-tube))
  (finish-output (slot-value st 'wrapped-stream)))
(defmethod stream-force-output ((st wrapping-char-output-tube))
  (force-output (slot-value st 'wrapped-stream)))
(defmethod stream-write-sequence ((st wrapping-char-output-tube) seq start end &key)
  (write-sequence seq (slot-value st 'wrapped-stream) :start start :end end))
(defmethod stream-advance-to-column ((st wrapping-char-output-tube) column)
  (format (slot-value st 'wrapped-stream) "~vT" column))
(defmethod stream-fresh-line ((st wrapping-char-output-tube))
  (fresh-line (slot-value st 'wrapped-stream)))
(defmethod stream-line-column ((st wrapping-char-output-tube))
  nil)
(defmethod stream-start-line-p ((st wrapping-char-output-tube))
  nil)
(defmethod stream-write-char ((st wrapping-char-output-tube) char)
  (write-char char (slot-value st 'wrapped-stream)))
(defmethod stream-write-string ((st wrapping-char-output-tube) string &optional start end)
  (write-string string (slot-value st 'wrapped-stream) :start start :end end))


(defclass flexi-input-tube (bivalent-input-tube flexi-input-stream) ())
(defmethod unread-sequence ((st flexi-input-tube) seq)
  (with-slots (input-stream-stack char-unreadable byte-unreadable) st
    (setf char-unreadable nil byte-unreadable nil)
    (with-accessors ((ext-format flexi-stream-external-format)) st
      (let ((octets (etypecase seq
                      (string (string-to-octets seq :external-format ext-format))
                      (vector seq)
                      (sequence (coerce seq 'vector)))))
        (push (make-flexi-stream (make-in-memory-input-stream octets)
                                 :external-format ext-format)
              input-stream-stack)))))
(defmethod (setf flexi-stream-external-format) :after (encoding (st flexi-input-tube))
  (with-slots (input-stream-stack) st
    (loop :for inp :in input-stream-stack :do
      (setf (flexi-stream-external-format inp) encoding))))



(defclass flexi-output-tube (bivalent-output-tube flexi-output-stream) ())



(defclass flexi-io-tube (bivalent-io-tube flexi-io-stream) ())


(defmethod tube-stream ((st wrapping-char-input-tube))
  (slot-value st 'stream))
(defmethod tube-stream ((st wrapping-char-output-tube))
  (slot-value st 'stream))
(defmethod tube-stream ((st flexi-stream))
  (flexi-stream-stream st))

(defclass process-tube (flexi-io-tube)
  ((process-info :initarg :process)))
(defmethod close :after ((st process-tube) &key abort)
  (with-slots (process-info) st
    (when (uiop:process-alive-p process-info)
      (uiop:terminate-process process-info :urgent abort))))

