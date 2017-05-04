;;;; package.lisp

(defpackage #:cl-pwn/timeout
  (:use #:cl)
  (:export
   #:*default-timeout*
   #:+timeout-max+
   #:timeout-spec
   #:timeout
   #:timeout-spec->seconds
   #:timeout-of
   #:countdown-active
   #:call-with-countdown-dispatch
   #:call-with-countdown
   #:with-countdown
   #:call-with-local-dispatch
   #:call-with-local
   #:with-local))
(defpackage #:cl-pwn
  (:use #:cl #:usocket #:trivial-gray-streams #:flexi-streams)
  (:export :tube))
