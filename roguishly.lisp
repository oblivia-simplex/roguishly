(in-package :roguishly)

(print 'hi)


(defclass cell ()
  ((tile :accessor tile
         :initform #\SPACE)
   (items :accessor item
          :initform nil)
   (vis :accessor vis
        :initform nil)
   (mob :accessor mob
        :initform nil)))

(defclass zone ()
  ((you :accessor you-are)
   (zone-map :accessor zone-map
             :initform (make-hash-table :test #'equalp))))

;; zone map is hash of coords -> cells

(defclass thing ()
  ((x :accessor x)
   (y :accessor y)
   (icon :accessor icon
         :initform #\SPACE)))

(defclass mob (thing)
  ((x :accessor x
      :initform 0)
   (y :accessor y
      :initform 0)
   (icon :accessor icon)
   (inventory :accessor inventory
              :initform '())
   ($$$ :accessor $$$
        :initform 0)
   (action :accessor action
           :initform () ;; action is a symbolic expression that will be evalled.
           )))          ;; whenever the mob has a turn. it can be quite complex
                        ;; and incorporate timing, decision making, etc. 
;; this could be used as configurable idle or auto setting
;; when implemented in *you*

(defclass you (mob)
  ((icon :accessor icon
         :initform #\@)
   (glow :accessor glow
         :initform 1)))

(defvar *you* (make-instance 'you))
(defvar *zonemap*)

(defun passable-p (y x)
  (not (or (< x 0)
           (< y 0)
           (>= x *width*)
           (>= y *height*)
           (block-p (aref *zonemap* y x)))))

(defgeneric try-move (moveable y x))
(defgeneric left (mob))
(defgeneric right (mob))
(defgeneric up (mob))
(defgeneric down (mob))

(defmethod try-move ((you you) y x)
  ;; nb: fails silently if obstacle is hit
  (if (passable y x)
      (setf (x you) x
            (y you) y)
      (format t "PATH OBSTRUCTED AT Y: ~D, X: ~D~%" y x)))

;; refactor as macros?

(defmethod left ((you you))
  (let ((x (1- (x you)))
        (y (y you)))
    (try-move you y x)))

(defmethod right ((you you))
  (let ((x (1+ (x you)))
        (y (y you)))
    (try-move you y x)))

(defmethod up ((you you))
  (let ((x (x you))
        (y (1- (y you))))
    (try-move you y x)))

(defmethod down ((you you))
  (let ((x (x you))
        (y (1+ (y you))))
    (try-move you y x)))
  

(defparameter *height* 40)
(defparameter *width* 80)

(defun read-zone-map (filename)
  ;; note that curses usually passes coords as y,x, not x,y
  ;; i.e. row, column
  (let ((x 0)
        (y 0)
        (zm (make-array (list *height* *width*)
                        :element-type 'cell)))
    (loop for i below (* *height* *width*) do
         (setf (row-major-aref zm i) (make-instance 'cell)))
    (with-open-file (stream filename :direction :input)
      (loop for byte = (read-char stream nil nil)
         while byte do
           (format t "~c" byte)
           (setf (tile (aref zm y x)) byte)
           (if (char= byte #\NEWLINE)
               (progn
                 (setf x 0)
                 (incf y))
               (incf x))))
    (setf *zonemap* zm)))

(defparameter blocking-tiles
  '(#\| #\- #\# #\= #\+ #\\ #\/
    #\o #\O))

(defun block-p (cell)
  (member (tile cell) blocking-tiles))


(defun you-are-here (y x)
  (and (= (x *you*) x)
       (= (y *you*) y)))

(defun draw-tile (y x zm)
  (let* ((cell (aref zm y x))
         (tile (cond ((item cell) (icon (item cell)))
                    ((mob cell) (icon (mob cell)))
                    ((you-are-here y x) (icon *you*))
                    ((vis cell) (tile cell))
                    (:OTHERWISE #\SPACE))))
      (mvaddch y x tile)))
          

(defun display-map (&optional (zm *zonemap*))
  (erase)
  (loop for x below *width* do
       (loop for y below *height* do
            (draw-tile y x zm)))
  (refresh))


(defvar *controls*)

(defun read-controls-from-file ()
  (with-open-file (stream "controls.lisp" :direction :input)
    (setf *controls* (read stream))))


(defun illuminate (zm src)
  (let ((lx (max 0 (- (x src) (glow src))))
        (hx (min *width* (+ (x src) (glow src))))
        (ly (max 0 (- (y src) (glow src))))
        (hy (min *height* (+ (y src) (glow src)))))
    (loop for x from lx to hx do
         (loop for y from ly to hy do
              (setf (vis (aref zm y x)) 1)))))

;; ==== control loop ====


(defun list-of-mobs (&optional (zm *zonemap*))
  ;; not very efficient...
  (let ((moblist))
    (loop for y below *height* do
         (loop for x below *width* do
              (if (mob (aref zm y x))
                  (push (mob (aref zm y x)) moblist))))
    moblist))
                        
  

(defun update-zone (&optional (zm *zonemap*))
  (let ((moblist))
    (list-of-mobs zm)
    (loop for mob in moblist do
         (eval (action mob)))))

(defun control-loop ()
  (loop do
       (let ((move (cdr (assoc (code-char (getch)) *controls*))))
         (when move
           (eval move)
           (illuminate *zonemap* *you*)
           (update-zone *zonemap*)
           (display-map *zonemap*)))))


(defun run ()
  (read-controls-from-file)
  (read-zone-map "/tmp/map")
  (connect-console)
  (display-map)
  (control-loop))
