;;;; ti.asd

(asdf:defsystem #:ti
  :description "Describe ti here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (#:lisp-unit)
  :components (
               (:file "amap")
               (:file "expr")
               (:file "types")
               (:file "ti")
               (:file "amap-tests")
               (:file "ti-tests")))

