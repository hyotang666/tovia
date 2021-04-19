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
           #:react
           #:boxel
           #:*pixel-size*
           #:defsprite
           #:sprite
           #:npc
           #:response?
           #:melee
           #:projectile
           #:last-direction
           #:damager
           #:knock-backer
           #:who
           #:player
           #:being
           #:life
           #:deadp
           #:x
           #:y
           #:front
           #:move
           #:target-direction
           #:list-all-sprites
           #:add
           #:delete-lives
           #:sequence-transition
           #:tracker
           #:key-down-p
           #:key-tracker-time
           #:keystate ; setfable
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
  (last-pressed-cursor :down :type (member :down :up :left :right))
  (time (parameter :max 30 :current 0) :type parameter :read-only t))

(defun keyword-scancode (keyword)
  (sdl2:scancode-key-to-value
    (intern (format nil "SCANCODE-~A" keyword) :keyword)))

(defun keystate (tracker keyword)
  ;; TODO: DEIFNE-COMPILER-MACRO for compile time KEYWORD-SCANCODE.
  (aref (key-tracker-state tracker) (keyword-scancode keyword)))

(defun (setf keystate) (new tracker keyword)
  ;; TODO: Make this with DEFINE-SETF-EXPANDER for optimize.
  (setf (aref (key-tracker-state tracker) (keyword-scancode keyword))
          (ecase new (:down 1) (:up 0))))

(defun key-down-p (tracker keyword)
  ;; TODO: DEIFNE-COMPILER-MACRO for compile time KEYWORD-SCANCODE.
  (= 1 (aref (key-tracker-state tracker) (keyword-scancode keyword))))

(defun last-pressed-cursor (tracker) (key-tracker-last-pressed-cursor tracker))

(defun (setf last-pressed-cursor) (new tracker)
  (setf (key-tracker-last-pressed-cursor tracker) new))

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
  ((last-direction :initarg :direction
                   :initform :s
                   :type direction
                   :accessor last-direction)))

(defclass being ()
  ((life :initform (parameter) :reader life :type parameter)
   (move-coeff :accessor move-coeff :type list :initform nil)
   (response :initarg :response :reader response :type timer)))

(defmethod initialize-instance :after
           ((o being) &key (response (n-bits-max 7)))
  (setf (slot-value o 'response) (make-timer response)))

(defun coeff (init coeff)
  (reduce #'funcall coeff :initial-value init :from-end t :key #'cdr))

(defmethod move-coeff (o) nil)

(defun response? (being) (< 0 (funcall (response being))))

(defclass no-directional (sprite) ())

(defclass 4-directional (sprite directional) ())

(defclass 8-directional (sprite directional) ())

(defclass npc (4-directional being) ())

(defclass player (4-directional being)
  ((key-tracker :initform (make-key-tracker)
                :type key-tracker
                :reader tracker)))

(defclass phenomenon ()
  ((life :initform (parameter) :reader life :type parameter)
   (effects :initarg :effects :reader effects :type list)
   (who :initarg :who :reader who :type being)))

(defmethod initialize-instance :after
           ((o phenomenon)
            &key effects (win (alexandria:required-argument :win)))
  (setf (slot-value o 'effects)
          (loop :for constructor :in effects
                :collect (funcall constructor win))))

(defclass melee (phenomenon no-directional) ())

(defclass projectile (phenomenon 8-directional) ())

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

(defmethod fude-gl:draw :before ((o 8-directional))
  (let* ((unit (unit o))
         (step
          (if (updatep o)
              (funcall (stepper o))
              (last-step (stepper o))))
         (left
          (float
            (* unit
               (ecase (last-direction o)
                 ((:n :s :e :w) step)
                 ((:nw :ne :sw :se) (+ 3 step))))))
         (right (+ left unit))
         (bottom
          (float
            (ecase (last-direction o)
              ((:n :ne) 0)
              ((:s :sw) (* 3 unit))
              ((:w :nw) (* 2 unit))
              ((:e :se) unit))))
         (top (+ bottom unit))
         (vertices
          (fude-gl:buffer-source
            (fude-gl:buffer (fude-gl:find-vertices 'sprite-quad)))))
    (asignf vertices left top left bottom right top right bottom))
  (fude-gl:send :buffer 'sprite-quad :method #'gl:buffer-sub-data))

(defmethod fude-gl:draw ((o being))
  (setf (fude-gl:uniform (fude-gl:shader 'sprite-quad) "alpha") 1.0)
  (call-next-method))

(defmethod fude-gl:draw :before ((o no-directional))
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

(defmethod fude-gl:draw ((o phenomenon))
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

(defgeneric move (subject window &key))

(flet ((go-down (o)
         (quaspar:move o (quaspar:x o)
                       (max 0
                            (- (quaspar:y o)
                               (coeff *pixel-size* (move-coeff o))))
                       *colliders*))
       (go-up (o win)
         (quaspar:move o (quaspar:x o)
                       (min
                         (- (nth-value 1 (sdl2:get-window-size win)) (boxel))
                         (+ (quaspar:y o) (coeff *pixel-size* (move-coeff o))))
                       *colliders*))
       (go-left (o)
         (quaspar:move o
                       (max 0
                            (- (quaspar:x o)
                               (coeff *pixel-size* (move-coeff o))))
                       (quaspar:y o) *colliders*))
       (go-right (o win)
         (quaspar:move o
                       (min (- (sdl2:get-window-size win) (boxel))
                            (+ (quaspar:x o)
                               (coeff *pixel-size* (move-coeff o))))
                       (quaspar:y o) *colliders*)))
  (defmethod move ((o being) (win sdl2-ffi:sdl-window) &key direction animate)
    (ecase direction
      (:n (go-up o win))
      (:s (go-down o))
      (:e (go-right o win))
      (:w (go-left o))
      (:nw (go-up o win) (go-left o))
      (:ne (go-up o win) (go-right o win))
      (:sw (go-down o) (go-left o))
      (:se (go-down o) (go-right o win)))
    (when animate
      (setf (last-direction o) direction))))

(flet ((go-down (o)
         (quaspar:move o (quaspar:x o)
                       (- (quaspar:y o) (coeff *pixel-size* (move-coeff o)))
                       *colliders*))
       (go-up (o)
         (quaspar:move o (quaspar:x o)
                       (+ (quaspar:y o) (coeff *pixel-size* (move-coeff o)))
                       *colliders*))
       (go-left (o)
         (quaspar:move o (- (quaspar:x o) (coeff *pixel-size* (move-coeff o)))
                       (quaspar:y o) *colliders*))
       (go-right (o)
         (quaspar:move o (+ (quaspar:x o) (coeff *pixel-size* (move-coeff o)))
                       (quaspar:y o) *colliders*)))
  (defmethod move
             ((o phenomenon) (win sdl2-ffi:sdl-window) &key direction animate)
    (declare (ignore win))
    (ecase direction
      (:n (go-up o))
      (:s (go-down o))
      (:e (go-right o))
      (:w (go-left o))
      (:nw (go-up o) (go-left o))
      (:ne (go-up o) (go-right o))
      (:sw (go-down o) (go-left o))
      (:se (go-down o) (go-right o)))
    (when animate
      (setf (last-direction o) direction))))

(defmethod move
           ((o player) (win sdl2-ffi:sdl-window) &key (animate t) direction)
  (flet ((update (tracker cursor)
           (if (not (eq cursor (last-pressed-cursor tracker)))
               ;; First time to press cursor.
               (setf (last-pressed-cursor tracker) cursor
                     (keystate tracker cursor) :down)
               (if (key-down-p tracker cursor)
                   ;; Keep on pressing.
                   nil
                   ;; Once :up but secondary press cursor.
                   (setf (keystate tracker cursor) :down
                         (move-coeff o)
                           (acons :dush (lambda (x) (* 2 x))
                                  (move-coeff o)))))))
    (let* ((direction
            (or direction
                (keypress-case
                  (:down (update (tracker o) :down)
                   (keypress-case
                     (:left :sw)
                     (:right :se)
                     (otherwise :s)))
                  (:up (update (tracker o) :up)
                   (keypress-case
                     (:left :nw)
                     (:right :ne)
                     (otherwise :n)))
                  (:right (update (tracker o) :right)
                   (keypress-case
                     (:up :ne)
                     (:down :se)
                     (otherwise :e)))
                  (:left (update (tracker o) :left)
                   (keypress-case
                     (:up :nw)
                     (:down :sw)
                     (otherwise :w)))
                  (otherwise
                    (setf (keystate (tracker o) :up) :up
                          (keystate (tracker o) :down) :up
                          (keystate (tracker o) :left) :up
                          (keystate (tracker o) :right) :up
                          (move-coeff o)
                            (delete :dush (move-coeff o) :key #'car))
                    nil)))))
      (when direction
        (call-next-method o win :direction direction :animate animate)))))

(defmethod move ((o npc) (win sdl2-ffi:sdl-window) &key direction (animate t))
  (call-next-method o win :direction
   (or direction (aref #(:n :w :w :e :nw :ne :sw :se) (random 8))) :animate
   animate))

(defmethod move ((o projectile) (win sdl2-ffi:sdl-window) &key)
  (call-next-method o win :direction (last-direction o)))

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

(defun deadp (o) (<= (current (life o)) 0))

(defun delete-lives ()
  (quaspar:do-lqtree (o *colliders*)
    (when (deadp o)
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

(defun in-sight-p (a b distance)
  (<
    (sqrt
      (+ (expt (- (quaspar:x a) (quaspar:x b)) 2)
         (expt (- (quaspar:y a) (quaspar:y b)) 2)))
    distance))

(defun compass (lat1 lon1 lat2 lon2)
  (labels ((radian (degree)
             (* degree #.(/ pi 180)))
           (bearing (radians)
             (rem (+ 360 (degrees radians)) 360))
           (degrees (radians)
             (/ (* radians 180) pi)))
    (let ((d-lon (radian (- lon2 lon1)))
          (d-phi
           (log
             (/ (tan (+ (/ (radian lat2) 2) #.(/ pi 4)))
                (tan (+ (/ (radian lat1) 2) #.(/ pi 4)))))))
      (when (< pi (abs d-lon))
        (setq d-lon
                (if (< 0 d-lon)
                    (- (- #.(* 2 pi) d-lon))
                    (+ #.(* 2 pi) d-lon))))
      (bearing (atan d-lon (realpart d-phi))))))

(defun target-direction (subject object)
  (let ((bearing
         (let ((w (* *width* (boxel))) (h (* *height* (boxel))))
           ;; Normarize coordinates otherwise unexpected behavior
           ;; especially with 360.
           (compass (/ (quaspar:x subject) w) (/ (quaspar:y subject) h)
                    (/ (quaspar:x object) w) (/ (quaspar:y object) h)))))
    (if (<= bearing 22.5)
        :e
        (if (<= bearing 67.5)
            :ne
            (if (<= bearing 112.5)
                :n
                (if (<= bearing 157.5)
                    :nw
                    (if (<= bearing 202.5)
                        :w
                        (if (<= bearing 247.5)
                            :sw
                            (if (<= bearing 292.5)
                                :s
                                (if (<= bearing 337.5)
                                    :se
                                    :e))))))))))

(defgeneric react (subject object) (:method (s o))) ; Do nothing.

(defmethod react ((subject phenomenon) (object being))
  (unless (eq (who subject) object)
    (dolist (effect (effects subject)) (funcall effect subject object))))

(defmethod react ((subject projectile) (object being))
  (call-next-method)
  (setf (current (life subject)) 0))

(defun damager (damage)
  (constantly
    (lambda (subject object)
      (declare (ignore subject))
      (decf (current (life object)) damage))))

(defun knock-backer (powor)
  (lambda (win)
    (lambda (subject object)
      (declare (ignore subject))
      (let ((*pixel-size* powor))
        (move object win
              :direction (turn-direction (last-direction object))
              :animate nil)))))

(let ((turns (make-hash-table :test #'eq)))
  (flet ((def (a b)
           (setf (gethash a turns) b)))
    (def :n :s)
    (def :ne :sw)
    (def :e :w)
    (def :se :nw)
    (def :s :n)
    (def :sw :ne)
    (def :w :e)
    (def :nw :se))
  (defun turn-direction (direction)
    (or (gethash direction turns) (error "Unknown direction ~S." direction))))

(defmethod react ((subject 4-directional) (object phenomenon))
  (react object subject))

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
      (quaspar:clear-lqtree *colliders*)
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