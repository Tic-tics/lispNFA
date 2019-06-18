;;;; statemachine.asd

(asdf:defsystem #:statemachine
  :description "a system for emulating state machines"
  :author "Aaron Schmidlkofer <ajs130430@gmail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:jonathan #:fset #:cl-dot)
  :components ((:file "package")
               (:file "jsonlib")
               (:file "statemachine")
			   (:file "closmachine")))
