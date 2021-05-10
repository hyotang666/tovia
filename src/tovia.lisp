(in-package :cl-user)

(defpackage :tovia
  (:use :cl)
  (:export ;;;; Specials
           #:*title*
           #:*scene*
           #:*width*
           #:*height*
           #:*pixel-size*
           #:*box-size*
           #:*colliders*
           #:*coeffs*)
  (:export ;;;; PARAMETER
           #:parameter ; type name, constructor
           #:make-parameter ; constructor
           #:current ; accessor
           #:max-of ; accessor
           #:parameter-ratio ; helper
           )
  (:export ;;;; TIMER
           #:timer ; type name
           #:make-timer ; constructor
           )
  (:export ;;;; KEY-TRACKER
           #:key-tracker ; type-name, reader
           #:key-tracker-time ; reader
           #:keystate ; accessor
           #:last-pressed ; accessor
           #:key-down-p
           #:command-input-p
           #:discrete-time)
  (:export ;;;; SPRITE
           #:defsprite ; dsl-macro
           #:list-all-sprites ; dev-helper
           #:sprite ; class-name
           #:x
           #:y)
  (:export ;;;; DIRECTIONAL
           #:no-directional
           #:last-direction)
  (:export ;;;; BEING
           #:being ; class-name
           #:life ; reader
           #:coeff-of ; reader
           #:find-coeff
           #:append-coeff
           #:delete-coeff
           #:response?
           ;; REACTIONS
           #:add-reaction
           #:rem-reaction
           ;; RESERVED-ACTIONS
           #:reserved-actions
           #:reserve-actions
           #:action-reserved-p
           #:do-reserved-action
           ;; Coeff protocols
           #:apply-coeff
           ;; Subclasses
           #:npc
           #:player)
  (:export ;;;; HAVING
           #:having ; class-name
           #:inventory ; reader
           #:add-item ; helpers.
           #:consume)
  (:export ;;;; PHENOMENON
           #:phenomenon ; class name
           #:who ; reader
           #:victimp
           ;; Reaction protocols.
           #:damager
           #:knock-backer
           ;; Subclasses
           #:effect
           #:melee
           #:projectile
           #:radiation)
  (:export ;;;; TRIGGER
           #:trigger ; class name.
           )
  (:export ;;;; STATUS-EFFECT
           #:status-effect ; class-name
           #:guard-effect)
  (:export ;;;; COLLISION
           #:collidep
           #:add
           #:del
           #:delete-lives
           #:deadp)
  (:export ;;;; GENERIC-FUNCTIONS
           #:move
           #:react)
  (:export ;;;; helpers
           #:keypress-case
           #:keypressp
           #:boxel
           #:front
           #:do-beings
           #:in-sight-p
           #:in-sight-beings
           #:nearest
           #:distance
           #:forwardablep
           #:target-direction
           #:turn-direction
           #:walk-random)
  (:export #:main #:sequence-transition #:defsound #:play #:pnd-random))

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

(defun parameter-ratio (parameter) (/ (current parameter) (max-of parameter)))

;;;; KEY-TRACKER

(defclass key-tracker ()
  ((state :initform (make-array 256 :element-type 'bit)
          :type bit-vector
          :reader state)
   (last-pressed-cursor :initform nil
                        :type (member nil :down :up :left :right)
                        :accessor last-pressed-cursor)
   (life :initform (parameter) :type parameter :reader life)
   (last-pressed :initform (alexandria:circular-list (list nil) (list nil)
                                                     (list nil))
                 :type list
                 :accessor key-tracker-last-pressed)
   (time :initform (parameter :max 30 :current 0)
         :type parameter
         :reader key-tracker-time)))

(define-compiler-macro keyword-scancode (&whole whole keyword)
  (declare (notinline keyword-scancode))
  (if (constantp keyword)
      (keyword-scancode (eval keyword))
      whole))

(defun keyword-scancode (keyword)
  (sdl2:scancode-key-to-value
    (intern (format nil "SCANCODE-~A" keyword) :keyword)))

(defun last-pressed (tracker) (car (key-tracker-last-pressed tracker)))

(defun (setf last-pressed) (new tracker)
  (setf (key-tracker-last-pressed tracker)
          (rplaca (cdr (key-tracker-last-pressed tracker))
                  (rplacd
                    (rplaca (cadr (key-tracker-last-pressed tracker)) new)
                    (get-internal-real-time)))))

(defun keystate (tracker keyword)
  ;; TODO: DEIFNE-COMPILER-MACRO for compile time KEYWORD-SCANCODE.
  (aref (state tracker) (keyword-scancode keyword)))

(defun (setf keystate) (new tracker keyword)
  (when (and (eq :down new)
             ;; To ignore cursor.
             (not (find keyword '(:down :up :left :right))))
    (setf (last-pressed tracker) keyword))
  (setf (aref (state tracker) (keyword-scancode keyword))
          (ecase new (:down 1) (:up 0))))

(defun key-down-p (tracker keyword)
  ;; TODO: DEIFNE-COMPILER-MACRO for compile time KEYWORD-SCANCODE.
  (= 1 (aref (state tracker) (keyword-scancode keyword))))

(defun command-input-p
       (command tracker delta &key (time (get-internal-real-time)))
  ;; FIXME: Too much consing. Doubly linked list is better.
  (let ((first (key-tracker-last-pressed tracker)))
    (labels ((rec (cache acc)
               (if (not (eq first cache))
                   (rec (cdr cache) (cons (car cache) acc))
                   (do* ((coms (reverse command) ; Don't NREVERSE!
                               (cdr coms))
                         (input (car coms) (car coms))
                         (cache (cons (car first) acc) (cdr cache))
                         (now time))
                        ((endp coms) t)
                     (when (endp cache)
                       (return nil))
                     (destructuring-bind
                         (key . past)
                         (car cache)
                       (unless (and (eq input key)
                                    (funcall delta (- now (setf now past))))
                         (return nil)))))))
      (rec (cdr first) nil))))

(define-compiler-macro discrete-time (a b)
  (let ((a
         (if (constantp a)
             (* (eval a) internal-time-units-per-second)
             `(* ,a internal-time-units-per-second)))
        (b
         (if (constantp b)
             (* (eval b) internal-time-units-per-second)
             `(* ,b internal-time-units-per-second))))
    `(lambda (delta) (< ,a delta ,b))))

(defun discrete-time (a b)
  (flet ((discrete-time (delta)
           (< (* a internal-time-units-per-second) delta
              (* b internal-time-units-per-second))))
    #'discrete-time))

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

;;;; SPRITE
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

(defclass having () ((inventory :accessor inventory :initform nil)))

(defun add-item (item having &optional (num 1))
  (incf (getf (inventory having) item 0) num)
  item)

(defun consume (item having)
  (if (= 0 (decf (getf (inventory having) item) 1))
      (remf (inventory having) item))
  item)

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

(defclass mortal () ((life :initarg :life :reader life :type parameter)))

(defmethod initialize-instance :after ((o mortal) &key (life 100) current)
  (setf (slot-value o 'life)
          (if current
              (parameter :current current :max life)
              (make-parameter life))))

(defclass being (mortal)
  ((coeff :reader coeff
          :type hash-table
          :initform (make-hash-table :test #'eq))
   (reserved-actions :initform nil
                     :initarg :reserved-actions
                     :accessor reserved-actions
                     :type list)
   (reactions :initarg :reactions
              :initform (make-hash-table)
              :type hash-table
              :reader reactions)
   (response :initarg :response :reader response :type timer)))

(defmethod initialize-instance :after
           ((o being) &key (response (n-bits-max 7)) reactions)
  (setf (slot-value o 'response) (make-timer response)
        (slot-value o 'reactions) (make-hash-table :test #'eq))
  (loop :for (k v) :on reactions :by #'cddr
        :do (add-reaction k v o)))

(defparameter *coeffs* nil)

(defun coeff-of (name o) (gethash name (coeff o)))

(defun (setf coeff-of) (new name o) (setf (gethash name (coeff o)) new))

(defun append-coeff (coeff &rest args)
  (labels ((rec (list acc)
             (if (endp list)
                 acc
                 (rec (cddr list) (acons (car list) (cadr list) acc)))))
    (rec args coeff)))

(defun find-coeff (name coeff) (assoc name coeff))

(defun delete-coeff (name coeff) (delete name coeff :key #'car :count 1))

(defun apply-coeff (init coeff)
  (reduce #'funcall (append *coeffs* coeff)
          :initial-value init
          :key #'cdr
          :from-end t))

(defun reserve-actions (being &rest args)
  ;; FIXME: QUEUE is better.
  (alexandria:appendf (reserved-actions being) args))

(defun action-reserved-p (name being) (assoc name (reserved-actions being)))

(defun do-reserved-action (being window)
  (funcall (cdr (pop (reserved-actions being))) being window))

(defun add-reaction (name reaction being)
  (let ((exists? (gethash name (reactions being))))
    (when exists?
      (warn "Replace reaction ~S for ~S" name being))
    (setf (gethash name (reactions being)) reaction)))

(defun rem-reaction (name being) (remhash name (reactions being)))

(defun response? (being) (< 0 (funcall (response being))))

(defclass no-directional (sprite directional) ())

(defclass 4-directional (sprite directional) ())

(defclass 8-directional (sprite directional) ())

(defclass npc (4-directional being) ())

(defclass player (4-directional being)
  ((key-tracker :initform (make-instance 'key-tracker)
                :initarg :key-tracker
                :type key-tracker
                :reader key-tracker)))

(defclass phenomenon (mortal)
  ((victims :initform (make-hash-table) :type hash-table :reader victims)
   (effects :initarg :effects :reader effects :type list)
   (who :initarg :who :reader who :type being)))

(defmethod initialize-instance :after
           ((o phenomenon)
            &key effects (win (alexandria:required-argument :win)))
  (setf (slot-value o 'effects)
          (loop :for constructor :in effects
                :collect (funcall constructor win))))

(defun add-victim (being phenomenon)
  (setf (gethash being (victims phenomenon)) t))

(defun victimp (being phenomenon) (values (gethash being (victims phenomenon))))

(defclass effect (phenomenon no-directional) ())

(defclass melee (phenomenon 8-directional) ())

(defclass projectile (phenomenon 8-directional) ())

(defclass radiation (phenomenon no-directional) ())

(defclass status-effect (mortal no-directional) ())

(defclass guard-effect (mortal 8-directional) ())

(defclass trigger (mortal no-directional)
  ((effects :initarg :effects :reader effects :type list)))

;;;; DRAW

(defun updatep (sprite) (not (zerop (funcall (timer sprite)))))

(defmacro asignf (vertices &rest args)
  `(setf ,@(loop :for index :in '(2 3 6 7 10 11 14 15)
                 :for v :in args
                 :collect `(gl:glaref ,vertices ,index)
                 :collect v)))

(defmethod fude-gl:draw :before ((o 4-directional))
  ;; Responds sprite animation.
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
  ;; Responds sprite animation.
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
              ((:w :se) (* 2 unit))
              ((:e :nw) unit))))
         (top (+ bottom unit))
         (vertices
          (fude-gl:buffer-source
            (fude-gl:buffer (fude-gl:find-vertices 'sprite-quad)))))
    (asignf vertices left top left bottom right top right bottom))
  (fude-gl:send :buffer 'sprite-quad :method #'gl:buffer-sub-data))

(defmethod fude-gl:draw :before ((o no-directional))
  ;; Responds sprite animation.
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

(defmethod fude-gl:draw :before ((o being))
  ;; Responds setting alpha blending.
  (setf (fude-gl:uniform (fude-gl:shader 'sprite-quad) "alpha") 1.0))

(defmethod fude-gl:draw :before ((o phenomenon))
  ;; Responds setting alpha blending.
  (setf (fude-gl:uniform (fude-gl:shader 'sprite-quad) "alpha") 0.875))

(defmethod fude-gl:draw :before ((o status-effect))
  ;; Responds setting alpha blending.
  (setf (fude-gl:uniform (fude-gl:shader 'sprite-quad) "alpha") 0.875))

(defmethod fude-gl:draw ((o sprite))
  ;; Responds actual drawing.
  (fude-gl:with-uniforms (projection model (tex :unit 0))
      (fude-gl:shader 'sprite-quad)
    (setf projection (projection o)
          model
            (let* ((boxel (boxel)) (half (/ boxel 2)))
              (fude-gl:model-matrix (+ half (quaspar:x o))
                                    (+ half (quaspar:y o)) boxel boxel))
          tex (texture o))
    (fude-gl:draw 'sprite-quad)))

(defmethod fude-gl:draw :after ((o being))
  (let ((effects (coeff-of :status-effect o)))
    (setf (coeff-of :status-effect o)
            (loop :for e :in effects
                  :if (<= 0 (decf (current (life (cdr e)))))
                    :do (setf (quaspar:x (quaspar:rect (cdr e))) (quaspar:x o)
                              (quaspar:y (quaspar:rect (cdr e))) (quaspar:y o))
                        (fude-gl:draw (cdr e))
                    :and :collect e))))

;;;; MOVE

(define-compiler-macro keypressp (&whole whole keyword)
  (if (constantp keyword)
      `(sdl2:keyboard-state-p
         ,(sdl2:scancode-key-to-value (keyword-scancode (eval keyword))))
      whole))

(defun keypressp (keyword)
  (sdl2:keyboard-state-p
    (sdl2:scancode-key-to-value (keyword-scancode keyword))))

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

(defun forwardablep (subject window direction)
  (flet ((up? ()
           (< (quaspar:y subject)
              (- (nth-value 1 (sdl2:get-window-size window)) (boxel))))
         (down? ()
           (< 0 (quaspar:y subject)))
         (right? ()
           (< (quaspar:x subject) (- (sdl2:get-window-size window) (boxel))))
         (left? ()
           (< 0 (quaspar:x subject))))
    (ecase direction
      (:n (up?))
      (:s (down?))
      (:e (right?))
      (:w (left?))
      (:nw (or (up?) (left?)))
      (:ne (or (up?) (right?)))
      (:sw (or (down?) (left?)))
      (:se (or (down?) (right?))))))

(defgeneric move (subject window &key))

(flet ((go-down (o)
         (quaspar:move o (quaspar:x o)
                       (max 0
                            (- (quaspar:y o)
                               (apply-coeff *pixel-size* (coeff-of :move o))))
                       *colliders*))
       (go-up (o win)
         (quaspar:move o (quaspar:x o)
                       (min
                         (- (nth-value 1 (sdl2:get-window-size win)) (boxel))
                         (+ (quaspar:y o)
                            (apply-coeff *pixel-size* (coeff-of :move o))))
                       *colliders*))
       (go-left (o)
         (quaspar:move o
                       (max 0
                            (- (quaspar:x o)
                               (apply-coeff *pixel-size* (coeff-of :move o))))
                       (quaspar:y o) *colliders*))
       (go-right (o win)
         (quaspar:move o
                       (min (- (sdl2:get-window-size win) (boxel))
                            (+ (quaspar:x o)
                               (apply-coeff *pixel-size* (coeff-of :move o))))
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
         (quaspar:move o (quaspar:x o) (- (quaspar:y o) *pixel-size*)
                       *colliders*))
       (go-up (o)
         (quaspar:move o (quaspar:x o) (+ (quaspar:y o) *pixel-size*)
                       *colliders*))
       (go-left (o)
         (quaspar:move o (- (quaspar:x o) *pixel-size*) (quaspar:y o)
                       *colliders*))
       (go-right (o)
         (quaspar:move o (+ (quaspar:x o) *pixel-size*) (quaspar:y o)
                       *colliders*)))
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
                     (current (life tracker)) 5
                     (keystate tracker cursor) :down)
               (if (key-down-p tracker cursor)
                   ;; Keep on pressing.
                   nil
                   ;; Once :up but secondary press cursor.
                   (setf (keystate tracker cursor) :down
                         (coeff-of :move o)
                           (acons :dush (lambda (x) (* 2 x))
                                  (coeff-of :move o)))))))
    (let* ((direction
            (or direction
                (keypress-case
                  (:down (update (key-tracker o) :down)
                   (keypress-case
                     (:left :sw)
                     (:right :se)
                     (otherwise :s)))
                  (:up (update (key-tracker o) :up)
                   (keypress-case
                     (:left :nw)
                     (:right :ne)
                     (otherwise :n)))
                  (:right (update (key-tracker o) :right)
                   (keypress-case
                     (:up :ne)
                     (:down :se)
                     (otherwise :e)))
                  (:left (update (key-tracker o) :left)
                   (keypress-case
                     (:up :nw)
                     (:down :sw)
                     (otherwise :w)))
                  (otherwise
                    (let ((tracker (key-tracker o)))
                      (setf (keystate tracker :up) :up
                            (keystate tracker :down) :up
                            (keystate tracker :left) :up
                            (keystate tracker :right) :up
                            (current (life tracker))
                              (1- (current (life tracker)))
                            (last-pressed-cursor tracker)
                              (if (<= (current (life tracker)) 0)
                                  nil
                                  (last-pressed-cursor tracker))
                            (coeff-of :move o)
                              (delete :dush (coeff-of :move o) :key #'car))
                      nil))))))
      (when direction
        (call-next-method o win :direction direction :animate animate)))))

(defmethod move ((o npc) (win sdl2-ffi:sdl-window) &key direction (animate t))
  (call-next-method o win :direction
   (or direction (aref #(:n :w :w :e :nw :ne :sw :se) (random 8))) :animate
   animate))

(defmethod move ((o projectile) (win sdl2-ffi:sdl-window) &key)
  (call-next-method o win :direction (last-direction o)))

(defmethod move ((o radiation) (win sdl2-ffi:sdl-window) &key)
  (call-next-method o win :direction (last-direction o)))

(defun walk-random (s &optional (range *box-size*))
  (apply #'reserve-actions s
         (loop :with direction
                     = (aref #(:s :n :w :e :nw :ne :sw :se) (random 8))
               :repeat range
               :collect (cons :move-box (lambda (s w)
                                          (move s w :direction direction))))))

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
        (and (or (<= b-left a-left b-right) (<= b-left a-right b-right))
             (or (<= b-top a-top b-bottom) (<= b-top a-bottom b-bottom)))))))

(defun distance (a b)
  (sqrt
    (+ (expt (- (quaspar:x a) (quaspar:x b)) 2)
       (expt (- (quaspar:y a) (quaspar:y b)) 2))))

(defun in-sight-p (a b distance)
  (let ((v
         (sqrt
           (+ (expt (- (quaspar:x a) (quaspar:x b)) 2)
              (expt (- (quaspar:y a) (quaspar:y b)) 2)))))
    (values (< v distance) v)))

(defmacro do-beings ((var) &body body)
  `(quaspar:do-lqtree (,var *colliders*)
     (unless (typep ,var 'phenomenon)
       ,@body)))

(defun in-sight-beings (subject distance &optional (*colliders* *colliders*))
  ;; FIXME: cl-utilities:with-collecters is better.
  (uiop:while-collecting (collect)
    (do-beings (thing)
      (unless (eq subject thing)
        (multiple-value-bind (see? distance)
            (in-sight-p subject thing distance)
          (when see?
            (collect (cons distance thing))))))))

(defun nearest (list)
  (destructuring-bind
      (distance . being)
      (reduce
        (lambda (champ challenger)
          (if (< (car champ) (car challenger))
              champ
              challenger))
        list)
    (values being distance)))

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

(defgeneric react (subject object)
  (:method :around ((subject phenomenon) (object being))
    (when (and (not (eq (who subject) object)) (not (victimp object subject)))
      (call-next-method)))
  (:method ((subject phenomenon) (object being))
    (add-victim object subject)
    (dolist (effect (effects subject)) (funcall effect subject object)))
  (:method ((subject trigger) (object being))
    (dolist (effect (effects subject)) (funcall effect subject object)))
  (:method ((object being) (subject phenomenon)) (react subject object))
  (:method ((object being) (subject projectile)) (react subject object))
  (:method ((object being) (subject radiation)) (react subject object))
  (:method ((object being) (subject trigger)) (react subject object))
  (:method :after ((subject projectile) (object being))
    (setf (current (life subject)) 0))
  (:method :after ((subject radiation) (object being))
    (setf (current (life subject)) 0))
  (:method ((a being) (b being))
    (flet ((doit (a b)
             (loop :for fun :being :each :hash-value :of (reactions a)
                   :do (funcall fun a b))))
      (doit a b)
      (doit b a)))
  (:method (s o)) ; The default, do nothing.
  )

(declaim ;; Reaction protocols.
         ;; These functions must return function which is initializer.
         ;; (See DEFSPRITE.)
         ;; The initializers that is funcalled object construct time
         ;; must return function which is reactor.
         ;; (See REACT.)
         ;; The reactors that is funcalled collision detected time
         ;; destructively modify arguments.
         (ftype (function *
                 (values (function (sdl2-ffi:sdl-window)
                          (values (function (phenomenon being)
                                   (values &optional))
                                  &optional))
                         &optional))
                damager
                knock-backer))

(defun damager (damage &optional (coeff (constantly damage)))
  (constantly
    (lambda (subject object)
      (decf (current (life object)) (funcall coeff subject object damage))
      (values))))

(defun knock-backer (powor)
  (lambda (win)
    (lambda (subject object)
      (let ((*coeffs* (acons :move (constantly powor) *coeffs*)))
        (move object win :direction (last-direction subject) :animate nil)
        (values)))))

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

;;;; DEFSOUND

(defparameter *sounds* (make-hash-table :test #'eq))

(defun list-all-sounds () (alexandria:hash-table-keys *sounds*))

(defun sound (name)
  (or (gethash name *sounds*)
      (error "Unknown sound ~S: Eval (list-all-sounds)." name)))

(defmacro defsound (name pathname)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *sounds*) (truename ,pathname))))

(defmacro with-sounds ((var) &body body)
  `(symbol-macrolet ((,var org.shirakumo.fraf.harmony:*server*))
     (org.shirakumo.fraf.harmony:maybe-start-simple-server)
     (unwind-protect (progn ,@body) (org.shirakumo.fraf.harmony:stop ,var))))

(define-compiler-macro play (&whole whole name &rest args)
  (declare (ignore args))
  (when (constantp name)
    (sound (eval name)))
  whole)

(defun play (name &key repeat (mixer :effect))
  (org.shirakumo.fraf.harmony:play (sound name)
                                   :name name
                                   :mixer mixer
                                   :if-exists :restart
                                   :repeat repeat))

(defun pnd-random (ceil)
  "Pseud Normal Distribution."
  (let #.(multiple-value-bind (list length)
             (loop :for f :upfrom 0.0 :to (* 2 pi) :by 0.1
                   :for v = (1- (/ (1+ (- (cos f))) 2))
                   :collect v :into list
                   :sum v :into sum
                   :finally (return
                             (values (mapcar (lambda (x) (/ x sum)) list)
                                     (length list))))
           `((list ',list) (length ,length)))
    (loop :repeat (1+ (random length))
          :for f :in list
          :sum f :into acc
          :finally (return (* acc ceil)))))

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
    (with-sounds (server))
    (with-colliders (:win win))
    (fude-gl:with-shader ())
    (fude-gl:with-text (win))
    (fude-gl:with-textures ())
    (sequence-handler-bind (fun scene)
      (quaspar:clear-lqtree *colliders*)
      (funcall fun win))))

;;;; TRANSITIONS
;; HELLO-TOVIA

(defun hello-tovia (win)
  (uiop:nest
    (sdl2:with-event-loop (:method :poll)
      (:quit ()
        t))
    (:idle nil)
    (fude-gl:with-clear (win (:color-buffer-bit))
      (fude-gl:render-text "Hello tovia." :x :center :y :center :win win))))