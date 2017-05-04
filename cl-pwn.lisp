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


;;; tubes

