(in-package :cl-user)

(defpackage :tovia
  (:use :cl)
  (:export #:*title*
           #:*scene*
           #:main
           #:*width*
           #:*height*
           #:boxel
           #:defsprite
           #:sprite
           #:4-directional
           #:list-all-sprites))

(in-package :tovia)

;;;; SPECIALS

(defparameter *height* 9 "Window aspect. From gameboy spec.")

(defparameter *width* 10 "Window aspect. From gameboy spec.")

(defparameter *pixel-size* 4)

(defparameter *box-size* 16)

(defparameter *title* "Top View Action.")

(defvar *scene* 'test)

(defun boxel () (* *box-size* *pixel-size*))

;; STEPPER

(defclass stepper () (first step) (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((o stepper) &key spec)
  (setf (slot-value o 'first) spec
        (slot-value o 'step) spec)
  (c2mop:set-funcallable-instance-function
   o
   (lambda ()
     (prog1 (car (slot-value o 'step))
       (setf (slot-value o 'step) (cdr (slot-value o 'step)))))))

(defun make-stepper (spec) (make-instance 'stepper :spec spec))

;; TIMER

(defun n-bits-max (n)
  (values (read-from-string (format nil "#B~V,,,'1A" n #\1))))

(defclass timer ()
  ((time :initform (n-bits-max 7) :reader time<-timer)
   (count :initform 0 :accessor count<-timer)
   (turn :initform 0 :accessor turn<-timer))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod initialize-instance :after ((o timer) &key speed)
  (c2mop:set-funcallable-instance-function
   o
   (lambda ()
     (incf (count<-timer o) speed)
     (multiple-value-bind (turn rem)
         (floor (count<-timer o) (time<-timer o))
       (if (plusp turn)
           (setf (count<-timer o) rem
                 (turn<-timer o) turn)
           (setf (count<-timer o) rem
                 (turn<-timer o) 0))))))

(defun make-timer (speed) (make-instance 'timer :speed speed))

;;;; SPLITE
;; SHADER

(fude-gl:defshader sprite-shader 330 (fude-gl:xy fude-gl:st)
  (:vertex ((coord :vec2) &uniform (projection :mat4) (model :mat4))
    (declaim (ftype (function nil (values)) main))
    (defun main ()
      "coord = st;"
      "gl_Position = projection * model * vec4(xy, 0.0, 1.0);"))
  (:fragment ((color :vec4) &uniform (tex :|sampler2D|))
    (declaim (ftype (function nil (values)) main))
    (defun main () "color = texture(tex, coord);")))

;; VERTICES

(fude-gl:defvertices sprite-quad
    (coerce
      (vector -0.5 0.5 0.0 0.0 ; Top left
              -0.5 -0.5 0.0 0.0 ; Bottom left
              0.5 0.5 0.0 0.0 ; Top right
              0.5 -0.5 0.0 0.0) ; Bottom right
      '(array single-float (*)))
  :draw-mode :triangle-strip
  :buffer `(:usage :dynamic-draw)
  :shader 'sprite-shader)

;; CLASSES

(deftype direction () '(member :n :s :e :w :nw :ne :sw :se))

(defclass sprite ()
  ((coord :initform (3d-vectors:vec3 0 0 0)
          :type 3d-vectors:vec3
          :reader coord)
   (projection :initarg :projection
               :type 3d-matrices:mat4
               :reader projection
               :allocation :class)
   (model :initform (let* ((boxel (boxel)) (half (/ boxel 2)))
                      (fude-gl:model-matrix half half boxel boxel))
          :type 3d-matrices:mat4
          :reader model)
   (unit :initform 1 :initarg :unit :reader unit)
   (texture :initarg :texture :reader texture)
   (stepper :type function :reader stepper)
   (timer :type function :initform (make-timer 10) :reader timer)))

(defmethod initialize-instance :after
           ((o sprite)
            &key (projection (alexandria:required-argument :projection))
            (win (alexandria:required-argument :win))
            (stepper (alexandria:circular-list 0 1 2 1)))
  (setf (slot-value o 'projection) (funcall projection win)
        (slot-value o 'stepper) (make-stepper stepper)))

(defclass directional ()
  ((last-direction :initform :s :type direction :accessor last-direction)))

(defclass 4-directional (sprite directional) ())

(defun updatep (sprite) (not (zerop (funcall (timer sprite)))))

(defmethod fude-gl:draw :before ((o 4-directional))
  (when (updatep o)
    (let* ((unit (unit o))
           (step (funcall (stepper o)))
           (left (float (* unit step)))
           (right (+ left unit))
           (bottom
            (float
              (ecase (last-direction o)
                ((:n :nw :ne) 0)
                ((:s :sw :se) (* 3 unit))
                ((:w) (* 2 unit))
                ((:e) unit))))
           (top (+ bottom unit))
           (vertices
            (fude-gl:buffer-source
              (fude-gl:buffer (fude-gl:find-vertices 'sprite-quad)))))
      #.(flet ((asign (args) ; as macrolet but easy to debug (i.e. expand).
                 `(setf ,@(loop :for index :in '(2 3 6 7 10 11 14 15)
                                :for value :in args
                                :collect `(gl:glaref vertices ,index)
                                :collect value))))
          (asign '(left top left bottom right top right bottom)))
      (fude-gl:send :buffer 'sprite-quad :method #'gl:buffer-sub-data))))

(defmethod fude-gl:draw ((o 4-directional))
  (fude-gl:with-uniforms (projection view model (tex :unit 0))
      (fude-gl:shader 'sprite-quad)
    (setf projection (projection o)
          model (3d-matrices:nmtranslate (model o) (coord o))
          tex (texture o))
    (fude-gl:draw 'sprite-quad)))

;;;; DEFSPRITE

(defvar *sprites* (make-hash-table :test #'eq))

(defun list-all-sprites () (alexandria:hash-table-keys *sprites*))

(defun sprite (name win)
  (or (funcall (gethash name *sprites* (constantly nil)) win)
      (error "Missing sprite ~S. Eval (list-all-sprites)." name)))

(defmacro defsprite (name type &body args)
  (let ((win (gensym "WINDOW")))
    `(progn
      (setf (gethash ',name *sprites*)
              (lambda (,win) (make-instance ',type :win ,win ,@args)))
      'name)))

(defun pprint-defsprite (stream exp)
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; pprint-logical-block.
                      "~W~1I~^ ~@_" ; Operator.
                      "~W~^ ~@_" ; name.
                      "~W~^ ~_" ; type.
                      "~@{" ; &body
                      "~W~^ ~@_~W~^ ~_" ; k-v pair.
                      "~}" ; end of &body
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (member defsprite)) 'pprint-defsprite)

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

(defun main (&optional (scene *scene*))
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
    (sequence-handler-bind (fun scene)
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
