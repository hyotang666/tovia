; vim: ft=lisp et
(in-package :asdf)
(defsystem "tovia"
  :description "2D Top View Action game framework."
  :version
  "0.10.0"
  :depends-on
  (
   "cl-opengl" ; opengl bindings.
   "fude-gl" ; opengl framework.
   "sdl2" ; windowing.
   "uiop" ; utilities.
   "alexandria" ; utilities.
   "closer-mop" ; Wrapper for Metaobject Protocol.
   "3d-vectors" ; Vector operations.
   "3d-matrices" ; Matrix operations.
   "quaspar" ; collision detection.
   )
  :pathname
  "src/"
  :components
  ((:file "tovia")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "tovia").
(defmethod component-depends-on ((o test-op) (c (eql (find-system "tovia"))))
  (append (call-next-method) '((test-op "tovia.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "tovia")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when system
    (load-system system)
    (defmethod perform :after ((o load-op) (c (eql (find-system "tovia"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (handler-case (symbol-call :jingoh.documentizer :import c)
                      (error (condition)
                             (warn "Fails to import documentation of ~S.~%~A"
                                   (coerce-name c)
                                   (princ-to-string condition))))))))
