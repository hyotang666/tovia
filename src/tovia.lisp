(in-package :cl-user)

(defpackage :tovia
  (:use :cl)
  (:export))

(in-package :tovia)

;;;; SPECIALS

(defparameter *height* 9 "Window aspect. From gameboy spec.")

(defparameter *width* 10 "Window aspect. From gameboy spec.")

(defparameter *pixel-size* 4)

(defparameter *box-size* 16)

(defparameter *title* "Top View Action.")

(defvar *scene*
  (lambda (win)
    (error
      "No scene bound for window ~S. ~:@_Do (setf tovia:*scene* your-func) or eval (tovia:main your-func)."
      win)))

(defun boxel () (* *box-size* *pixel-size*))

;;;; SEQUENCE-TRANSITION

(define-condition sequence-transition () ((next :reader next :initarg :next)))

(defmacro sequence-handler-bind ((var init-form) &body body)
  `(let ((,var ,init-form))
     (tagbody
      :top
       (handler-bind ((sequence-transition
                       (lambda (condition)
                         (setf ,var (next condition))
                         (go :top))))
         ,@body))))

;;;; MAIN

(defun main (&optional (scene #'hello-tovia))
  (setq *scene* scene)
  (uiop:nest
    (sdl2:with-init (:everything))
    (sdl2:with-window (win :flags '(:shown :opengl)
                           :w (* *width* (boxel))
                           :h (* *height* (boxel))
                           :title *title*))
    (sdl2:with-gl-context (context win)
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha))
    (fude-gl:with-shader ())
    (fude-gl:with-textures ())
    (sequence-handler-bind (fun *scene*)
      (funcall fun win))))

;;;; TRANSITIONS
;; TEST

(defun hello-tovia (win)
  (uiop:nest
    (fude-gl:with-text-renderer (text :win win :size 32))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (text "Hello tovia." :x :center :y :center))))