
(ql:quickload :uiop)
(ql:quickload :bordeaux-threads)
(ql:quickload :cl-pwn)
(ql:quickload :iterate)
(require 'sb-bsd-sockets)
(require 'sb-posix)

(defvar *term-socket-path*
  (make-pathname :directory (list :absolute "tmp" (write-to-string
                                                   (sb-posix:getpid)))
                 :name "lisp-term-socket"
                 :type "sock"
                 :defaults *default-pathname-defaults*))
(defvar *term-socket*
  (let ((sock (make-instance 'sb-bsd-sockets:local-socket :type :stream))
        (sock-path *term-socket-path*))
    (ensure-directories-exist sock-path)
    (sb-bsd-sockets:socket-bind sock (namestring sock-path))
    sock))

(defparameter *term-socket-lock* (bt:make-lock "term-socket-lock"))

(defun launch-connected-xterm ()
  (uiop:launch-program
   (list "xterm" "-e" "socat" "-,raw,echo=0"
         (format nil"UNIX-CONNECT:~A" (namestring *term-socket-path*)))))

(defun connect-xterm ()
  (sb-bsd-sockets:socket-listen *term-socket* 1)
  (let* ((process (launch-connected-xterm))
         (sock (sb-bsd-sockets:socket-accept *term-socket*))
         (stream (sb-bsd-sockets:socket-make-stream
                  sock :input t
                       :output t
                       :buffering :none
                       :element-type '(unsigned-byte 8)
                       :auto-close t)))
    (make-instance 'cl-pwn/tubes:process-tube
                   :process process
                   :stream stream
                   :external-format (flexi-streams:make-external-format :ascii))))

(defun tube-connect (a b &optional close-b-on-exit)
  (let ((a-stream (cl-pwn/tubes:tube-stream a))
        (b-stream (cl-pwn/tubes:tube-stream b)))
    (labels ((pump ()
               (unwind-protect
                    (handler-case
                        (iter:iter
                          (iter:for byte := (read-byte a-stream))
                          (when (eql byte :eof)
                            (return (values)))
                          (write-byte byte b-stream)
                          (finish-output b-stream))
                      (stream-error (e)
                        (declare (ignore e))
                        (values)))
                 (if close-b-on-exit (close b :abort t)))))
      (bt:make-thread #'pump))))


(defclass pty-process-tube (cl-pwn/tubes:flexi-io-tube)
  ((process :initarg :process)))
(defmethod close :after ((st pty-process-tube) &key abort)
  (declare (ignore abort))
  (with-slots (process) st
    (when (sb-ext:process-alive-p process)
      (sb-ext:process-close process))))

(defun pty-process (command &rest args &key (external-format :utf-8))
  (let ((proc (apply 'sb-ext:run-program command nil
                     :input :stream
                     :output :stream
                     :error :output
                     :search t
                     :wait nil
                     :pty t
                     :env '((:term . "xterm"))
                     args)))
    (make-instance 'pty-process-tube
                   :stream (sb-ext:process-pty proc)
                   :process proc
                   :external-format (flexi-streams:make-external-format external-format))))

(defun interact-with-xterm (flexi-tube)
  (let ((xterm (connect-xterm)))
    (tube-connect flexi-tube xterm t)
    (tube-connect xterm flexi-tube)
    xterm))


