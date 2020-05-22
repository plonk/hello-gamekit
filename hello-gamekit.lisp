(cl:defpackage :hello-gamekit
  (:use :cl)
  (:export hello-gamekit))

(cl:in-package :hello-gamekit)

;;;
;;; Some state we need
;;;
(defvar *canvas-width* 800)
(defvar *canvas-height* 600)
(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *head-grabbed-p* nil)
(defvar *curve* (make-array 4 :initial-contents (list (gamekit:vec2 300 300)
                                                      (gamekit:vec2 375 300)
                                                      (gamekit:vec2 425 300)
                                                      (gamekit:vec2 500 300))))

(gamekit:register-resource-package :keyword
                                   (asdf:system-relative-pathname :hello-gamekit "assets/"))


(gamekit:define-image :snake-head "snake-head.png")
(gamekit:define-sound :snake-grab "snake-grab.ogg")

;;;
;;; Main class we use to run our application
;;;
(gamekit:defgame hello-gamekit () ()
  (:viewport-width *canvas-width*)
  (:viewport-height *canvas-height*)
  (:viewport-title "Hello Gamekit!"))


;;;
;;; Every time system starts, we need to rebind actions
;;;
(defmethod gamekit:post-initialize ((app hello-gamekit))
  (gamekit:bind-cursor (lambda (x y)
                       "When left mouse button is pressed, update snake's head position"
                       (when *head-grabbed-p*
                         (let ((head-position (aref *curve* 3)))
                           (setf (gamekit:x head-position) x
                                 (gamekit:y head-position) y)))))

  (gamekit:bind-button :mouse-left :pressed
                       (lambda ()
                         (gamekit:play :snake-grab)
                         (setf *head-grabbed-p* t)))

  (gamekit:bind-button :mouse-left :released
                       (lambda () (setf *head-grabbed-p* nil))))


(defun real-time-seconds ()
  "Return seconds since certain point of time"
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun update-position (position time)
  (let* ((subsecond (nth-value 1 (truncate time)))
         (angle (* 2 pi subsecond)))
    (setf (gamekit:y position) (+ 300 (* 100 (sin angle))))))


(defmethod gamekit:act ((app hello-gamekit))
  (update-position (aref *curve* 1) (real-time-seconds))
  (update-position (aref *curve* 2) (+ 0.1 (real-time-seconds))))

;;;
;;; All the drawing should happend in this method
;;;
(defmethod gamekit:draw ((app hello-gamekit))
  (let* ((xscale (/ (gamekit:viewport-width) (gamekit:image-width :snake-head)))
         (yscale (/ (gamekit:viewport-height) (gamekit:image-height :snake-head))))
    (cl-bodge.canvas:scale-canvas xscale yscale)
    (gamekit:draw-image (gamekit:vec2 0 0) :snake-head)
    ))
