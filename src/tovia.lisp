(in-package :cl-user)

(defpackage :tovia
  (:use :cl)
  (:export #:*title*
           #:*scene*
           #:main
           #:*width*
           #:*height*
           #:*colliders*
           #:collidep
           #:boxel
           #:*pixel-size*
           #:defsprite
           #:sprite
           #:4-directional
           #:effect
           #:player
           #:x
           #:y
           #:front
           #:move
           #:list-all-sprites
           #:add
           #:delete-lives
           #:tracker
           #:key-down-p
           #:key-tracker-time
           #:update-keystate
           #:current
           #:keypress-case))

(in-package :tovia)

;;;; SPECIALS

(defparameter *height* 9 "Window aspect. From gameboy spec.")

(defparameter *width* 10 "Window aspect. From gameboy spec.")

(defparameter *pixel-size* 4)

(defparameter *box-size* 16)

(defparameter *title* "Top View Action.")

(defvar *scene* 'hello-tovia)

(defun boxel () (* *box-size* *pixel-size*))

(defvar *colliders*)

;;;; PARAMETER

(defun n-bits-max (n)
  (values (read-from-string (format nil "#B~V,,,'1A" n #\1))))

(defstruct (parameter (:constructor parameter)
                      (:constructor make-parameter
                       (num &aux (max num) (current num))))
  (system-max (n-bits-max 8) :type (integer 0 *) :read-only t)
  (max 100 :type integer)
  (current 100 :type integer))

(declaim (ftype function current max-of))

(setf (symbol-function 'current) #'parameter-current
      (symbol-function 'max-of) #'parameter-max)

(defun (setf current) (new parameter)
  (setf (parameter-current parameter) (min (parameter-max parameter) new)))

(defun (setf max-of) (new parameter)
  (setf (parameter-max parameter) (min (parameter-system-max parameter) new)))

;;;; KEY-TRACKER

(defstruct key-tracker
  (state (make-array 256 :element-type 'bit) :type bit-vector)
  (time (parameter :max 30 :current 0) :type parameter :read-only t))

(defun keystate (tracker character)
  (aref (key-tracker-state tracker) (char-code character)))

(defun update-keystate (tracker character state)
  (setf (aref (key-tracker-state tracker) (char-code character))
          (ecase state (:down 1) (:up 0))))

(defun key-down-p (tracker character)
  (= 1 (aref (key-tracker-state tracker) (char-code character))))

(defmethod print-object ((o key-tracker) stream)
  (print-unreadable-object (o stream :type t)))

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

(defun last-step (stepper) (car (slot-value stepper 'step)))

;; TIMER

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
  (:fragment ((color :vec4) &uniform (tex :|sampler2D|) (alpha :float))
    (declaim (ftype (function nil (values)) main))
    (defun main () "color = alpha * texture(tex, coord);")))

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

(defclass sprite (quaspar:lqtree-storable)
  ((projection :initarg :projection
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
   (timer :type function :reader timer))
  (:default-initargs :x 0 :y 0 :w (boxel) :h (boxel)))

(defmethod initialize-instance :after
           ((o sprite)
            &key (projection (alexandria:required-argument :projection))
            (win (alexandria:required-argument :win))
            (stepper (alexandria:circular-list 0 1 2 1)) (timer 10))
  (setf (slot-value o 'projection) (funcall projection win)
        (slot-value o 'stepper) (make-stepper stepper)
        (slot-value o 'timer) (make-timer timer)))

(defmethod quaspar:x ((o sprite)) (quaspar:x (quaspar:rect o)))

(defmethod quaspar:y ((o sprite)) (quaspar:y (quaspar:rect o)))

(defclass directional ()
  ((last-direction :initform :s :type direction :accessor last-direction)))

(defclass being () ((life :initform (parameter) :reader life :type parameter)))

(defclass 4-directional (sprite directional being) ())

(defclass player (4-directional)
  ((key-tracker :initform (make-key-tracker)
                :type key-tracker
                :reader tracker)))

(defclass effect (sprite)
  ((life :initform (parameter) :reader life :type parameter)))

;;;; DRAW

(defun updatep (sprite) (not (zerop (funcall (timer sprite)))))

(defmacro asignf (vertices &rest args)
  `(setf ,@(loop :for index :in '(2 3 6 7 10 11 14 15)
                 :for v :in args
                 :collect `(gl:glaref ,vertices ,index)
                 :collect v)))

(defmethod fude-gl:draw :before ((o 4-directional))
  (let* ((unit (unit o))
         (step
          (if (updatep o)
              (funcall (stepper o))
              (last-step (stepper o))))
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
    (asignf vertices left top left bottom right top right bottom))
  (fude-gl:send :buffer 'sprite-quad :method #'gl:buffer-sub-data))

(defmethod fude-gl:draw ((o 4-directional))
  (setf (fude-gl:uniform (fude-gl:shader 'sprite-quad) "alpha") 1.0)
  (call-next-method))

(defmethod fude-gl:draw :before ((o effect))
  (let ((step
         (if (updatep o)
             (funcall (stepper o))
             (last-step (stepper o)))))
    (if (null step)
        (return-from fude-gl:draw (setf (current (life o)) 0))
        (destructuring-bind
            (x y)
            step
          (let* ((unit (unit o))
                 (left (float (* unit x)))
                 (right (+ left unit))
                 (bottom (float (* unit y)))
                 (top (+ bottom unit))
                 (vertices
                  (fude-gl:buffer-source
                    (fude-gl:buffer (fude-gl:find-vertices 'sprite-quad)))))
            (asignf vertices left top left bottom right top right bottom))
          (fude-gl:send :buffer 'sprite-quad :method #'gl:buffer-sub-data)))))

(defmethod fude-gl:draw ((o effect))
  (setf (fude-gl:uniform (fude-gl:shader 'sprite-quad) "alpha") 0.875)
  (call-next-method))

(defmethod fude-gl:draw ((o sprite))
  (fude-gl:with-uniforms (projection model (tex :unit 0))
      (fude-gl:shader 'sprite-quad)
    (setf projection (projection o)
          model
            (let* ((boxel (boxel)) (half (/ boxel 2)))
              (fude-gl:model-matrix (+ half (quaspar:x o))
                                    (+ half (quaspar:y o)) boxel boxel))
          tex (texture o))
    (fude-gl:draw 'sprite-quad)))

;;;; MOVE

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun <keypress-pred> (key)
    (etypecase key
      ((cons (eql or)) `(or ,@(mapcar #'<keypress-pred> (cdr key))))
      ((cons (eql and)) `(and ,@(mapcar #'<keypress-pred> (cdr key))))
      (atom
       `(sdl2:keyboard-state-p
          ,(sdl2:scancode-key-to-value
             (intern (format nil "SCANCODE-~A" key) :keyword)))))))

(defmacro keypress-case (&body clause+)
  `(cond
     ,@(mapcar
         (lambda (clause)
           (if (eq 'otherwise (car clause))
               `(t ,@(cdr clause))
               `(,(<keypress-pred> (car clause)) ,@(cdr clause))))
         clause+)))

(flet ((go-down (o)
         (quaspar:move o (quaspar:x o) (max 0 (- (quaspar:y o) *pixel-size*))
                       *colliders*))
       (go-up (o win)
         (quaspar:move o (quaspar:x o)
                       (min
                         (- (nth-value 1 (sdl2:get-window-size win)) (boxel))
                         (+ (quaspar:y o) *pixel-size*))
                       *colliders*))
       (go-left (o)
         (quaspar:move o (max 0 (- (quaspar:x o) *pixel-size*)) (quaspar:y o)
                       *colliders*))
       (go-right (o win)
         (quaspar:move o
                       (min (- (sdl2:get-window-size win) (boxel))
                            (+ (quaspar:x o) *pixel-size*))
                       (quaspar:y o) *colliders*)))
  (defun move (o win)
    (keypress-case
      (:down (go-down o) (setf (last-direction o) :s)
       (keypress-case
         (:left (go-left o) (setf (last-direction o) :sw))
         (:right (go-right o win) (setf (last-direction o) :se))))
      (:up (go-up o win) (setf (last-direction o) :n)
       (keypress-case
         (:left (go-left o) (setf (last-direction o) :nw))
         (:right (go-right o win) (setf (last-direction o) :ne))))
      (:right (go-right o win) (setf (last-direction o) :e)
       (keypress-case
         (:up (go-up o win) (setf (last-direction o) :ne))
         (:down (go-down o) (setf (last-direction o) :se))))
      (:left (go-left o) (setf (last-direction o) :w)
       (keypress-case
         (:up (go-up o win) (setf (last-direction o) :nw))
         (:down (go-down o) (setf (last-direction o) :sw)))))))

;;;; COLLIDERS

(defmacro with-colliders
          ((&key (win (alexandria:required-argument :win))) &body body)
  `(let ((*colliders*
          (multiple-value-call #'quaspar:make-lqtree
            (sdl2:get-window-size ,win)
            4)))
     ,@body))

(defun add (storable) (quaspar:add storable *colliders*))

(defun del (storable) (quaspar:delete storable *colliders*))

(defun delete-lives ()
  (quaspar:do-lqtree (o *colliders*)
    (when (and (typep o 'effect) (zerop (current (life o))))
      (del o))))

(defun collidep (a b)
  (flet ((vertices (o)
           (let* ((half-w (/ (quaspar:w (quaspar:rect o)) 2))
                  (half-h (/ (quaspar:h (quaspar:rect o)) 2))
                  (x (quaspar:x (quaspar:rect o)))
                  (y (quaspar:y (quaspar:rect o)))
                  (left (- x half-w))
                  (right (+ x half-w))
                  (top (- y half-h))
                  (bottom (+ y half-h)))
             (values left right top bottom))))
    (multiple-value-bind (a-left a-right a-top a-bottom)
        (vertices a)
      (multiple-value-bind (b-left b-right b-top b-bottom)
          (vertices b)
        (and (or (< b-left a-left b-right) (< b-left a-right b-right))
             (or (< b-top a-top b-bottom) (< b-top a-bottom b-bottom)))))))

(defun front (player)
  (ecase (last-direction player)
    (:n (values (quaspar:x player) (+ (quaspar:y player) (boxel))))
    (:s (values (quaspar:x player) (- (quaspar:y player) (boxel))))
    (:w (values (- (quaspar:x player) (boxel)) (quaspar:y player)))
    (:e (values (+ (quaspar:x player) (boxel)) (quaspar:y player)))
    (:nw
     (values (- (quaspar:x player) (boxel)) (+ (quaspar:y player) (boxel))))
    (:ne
     (values (+ (quaspar:x player) (boxel)) (+ (quaspar:y player) (boxel))))
    (:sw
     (values (- (quaspar:x player) (boxel)) (- (quaspar:y player) (boxel))))
    (:se
     (values (+ (quaspar:x player) (boxel)) (- (quaspar:y player) (boxel))))))

;;;; DEFSPRITE

(defvar *sprites* (make-hash-table :test #'eq))

(defun list-all-sprites () (alexandria:hash-table-keys *sprites*))

(defun sprite (name win &rest args)
  (or (funcall (gethash name *sprites* (constantly nil)) win args)
      (error "Missing sprite ~S. Eval (list-all-sprites)." name)))

(defmacro defsprite (name type &body args)
  (let ((win (gensym "WINDOW")) (vargs (gensym "ARGS")))
    `(progn
      (setf (gethash ',name *sprites*)
              (lambda (,win ,vargs)
                (apply #'make-instance ',type :win ,win
                       (append ,vargs (list ,@args)))))
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
    (with-colliders (:win win))
    (fude-gl:with-shader ())
    (fude-gl:with-textures ())
    (sequence-handler-bind (fun scene)
      (funcall fun win))))

;;;; TRANSITIONS
;; HELLO-TOVIA

(defun hello-tovia (win)
  (uiop:nest
    (fude-gl:with-text-renderer (text :win win :size 32))
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (text "Hello tovia." :x :center :y :center))))
