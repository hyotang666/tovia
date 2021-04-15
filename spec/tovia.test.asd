; vim: ft=lisp et
(in-package :asdf)
(defsystem "tovia.test"
  :version
  "0.0.0"
  :depends-on
  (:jingoh "tovia")
  :components
  ((:file "tovia"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :tovia args)))