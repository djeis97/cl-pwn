;;;; cl-pwn.asd

(asdf:defsystem #:cl-pwn
  :description "Describe cl-pwn here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:alexandria #:usocket #:trivial-gray-streams #:flexi-streams #:uiop)
  :serial t
  :components ((:file "package")
               (:file "cl-pwn")))

