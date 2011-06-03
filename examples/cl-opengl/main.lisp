(require :asdf)

(require :cl-opengl)
(require :cl-glu)
(require :cl-glut)

(require :postmodern)
(use-package :postmodern)

(require :cl-ewkb)
(use-package :cl-ewkb)

;;--------------------------------------------------------

(defun init-db ()
  (if (not *database*)
      (connect-toplevel "michael" "michael" "xxx" "localhost")))

(defun draw-point-primitive (point)
  (cond
    ((point-primitive-p point)
     (gl:vertex
      (point-primitive-x point)
      (point-primitive-y point)))
    ((pointz-primitive-p point)
     (gl:vertex
      (point-primitive-x point)
      (point-primitive-y point)
      (pointz-primitive-z point)))
    ((pointm-primitive-p point)
     (gl:vertex
      (point-primitive-x point)
      (point-primitive-y point)
      0.0
      (pointm-primitive-m point)))
    ((pointzm-primitive-p point)
     (gl:vertex
      (point-primitive-x point)
      (point-primitive-y point)
      (pointz-primitive-z point)
      (pointzm-primitive-m point)))))

(defun draw-linear-ring (line)
  (map 'nil (lambda (point) (draw-point-primitive point))
       (linear-ring-points-primitive line)))

(defun draw-single-point (point)
  (draw-point-primitive (point-point-primitive point)))

(defun draw-point (point)
  (gl:with-primitives :points
    (draw-point-primitive (point-point-primitive point))))

(defun draw-linestring (line)
  (gl:with-primitives :line-strip
    (map 'nil (lambda (point) (draw-point-primitive point))
         (line-string-points-primitive line))))

(defun draw-polygon (polygon)
  "Draw only first line of polygon"
  (gl:with-primitives :polygon
    (draw-linear-ring (elt (polygon-linear-rings polygon) 0))))

(defun draw-multipoint (points)
  (gl:with-primitive :points
    (map 'nil (lambda (point) (draw-single-point point)) (multi-point-points points))))

(defun draw-multilinestring (lines)
  (map 'nil (lambda (line)
              (draw-linestring line))
       (multi-line-string-line-strings lines)))

(defun draw-multipolygon (polygons)
  (map 'nil (lambda (polygon)
              (draw-polygon polygon))
       (multi-polygon-polygons polygons)))

(defun draw-gisobject (object)
  (cond
    ((point-p object) (draw-point object))
    ((line-string-p object) (draw-linestring object))
    ((polygon-p object) (draw-polygon object))
    ((multi-point-p object) (draw-multipoint object))
    ((multi-line-string-p object) (draw-multilinestring object))
    ((multi-polygon-p object) (draw-multipolygon object))
    ((geometry-collection-p object) (draw-geometrycollection object))))

(defun draw-geometrycollection (geometries)
  (map 'nil (lambda (object)
              (draw-gisobject object))
       (geometry-collection-geometries geometries)))


(cffi:defcallback key :void ((key :uchar) (x :int) (y :int))
                  (case (code-char key)
                    (#\Esc (glut:leave-main-loop))
                    (#\q (glut:leave-main-loop))
                    (#\r (glut:post-redisplay))))

(cffi:defcallback draw :void ()
                  (gl:clear :color-buffer)
                  (draw-gisobject (decode (caar (query (:select (:ST_AsEWKB "POINT(0 0)"))))))
                  (draw-gisobject (decode (caar (query (:select (:ST_AsEWKB "LINESTRING(1 2 1, 2 3 4)"))))))
                  (gl:flush))

;;,
(defun init ()
  (gl:clear-color 0.0 0.0 0.05 0))

(defparameter nrange 50.0)

(cffi:defcallback reshape :void ((width :int) (height :int))
                  (if (= height 0)
                      (setf height 1))
                  (gl:viewport 0 0 width height)
                  (gl:matrix-mode :projection)
                  (gl:load-identity)
                  (if (<= width height)
                      (gl:ortho (- nrange) nrange (- (* nrange (/ height width))) 
                                (* nrange (/ height width)) (- nrange) nrange)
                      (gl:ortho  (- (* nrange (/ height width))) (* nrange (/ height width))
                                 (- nrange) nrange (- nrange) nrange)))

(defun main ()
  (init-db)
  (glut:init)
  (glut:init-display-mode :single :rgb)
  (glut:create-window "Stars")
  (init)
  (glut:display-func (cffi:callback draw))
  (glut:reshape-func (cffi:callback reshape))
  (glut:keyboard-func (cffi:callback key))
  (glut:main-loop))
