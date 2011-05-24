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

(defun draw-line-primitive (line)
    (map 'nil (lambda (point) (draw-point-primitive point))
        (line-primitive-points line)))

(defun draw-single-point (point)
    (draw-point-primitive (gisgeometry-object point)))

(defun draw-point (point)
    (gl:with-primitives :points
        (draw-point-primitive (gisgeometry-object point))))

(defun draw-linestring (line)
    (gl:with-primitives :line-strip
        (draw-line-primitive (gisgeometry-object line))))

(defun draw-polygon (polygon)
    "Draw only first line of polygon"
    (gl:with-primitives :polygon
        (draw-line-primitive (elt (gisgeometry-object polygon) 0))))

(defun draw-multipoint (points)
    (gl:with-primitive :points
        (map 'nil (lambda (point) (draw-single-point point)) (gisgeometry-object points))))

(defun draw-multilinestring (lines)
    (map 'nil (lambda (line)
                  (draw-linestring line))
        (gisgeometry-object lines)))

(defun draw-multipolygon (polygons)
    (map 'nil (lambda (polygon)
                  (draw-polygon polygon))
        (gisgeometry-object polygons)))

(defun draw-gisobject (object)
    (cond
        ((point-p object) (draw-point object))
        ((linestring-p object) (draw-linestring object))
        ((polygon-p object) (draw-polygon object))
        ((multipoint-p object) (draw-multipoint object))
        ((multilinestring-p object) (draw-multilinestring object))
        ((multipolygon-p object) (draw-multipolygon object))
        ((gisgeometry-p object) (draw-geometrycollection object))))

(defun draw-geometrycollection (geometries)
    (map 'nil (lambda (object)
                  (draw-gisobject object))
        (gisgeometry-object geometries)))


(cffi:defcallback key :void ((key :uchar) (x :int) (y :int))
    (case (code-char key)
        (#\Esc (glut:leave-main-loop))
        (#\q (glut:leave-main-loop))
        (#\r (glut:post-redisplay))))

(cffi:defcallback draw :void ()
    (gl:clear :color-buffer)
    
    ;; Letter P
    (dolist (row (query
                     (:select
                         (:raw "unnest(ARRAY[ST_AsBinary(ST_AsEWKB('GEOMETRYCOLLECTION(LINESTRING(-100 0 0, 100 0 0), LINESTRING(0 -100 0, 0 100 0), LINESTRING(0 0 -100, 0 0 100))'))
-- Letter O
,ST_AsBinary(ST_AsEWKB('LINESTRING(3.5 0, 2.2 1, 3 3, 4.2 5, 5.2 3, 4.5 1, 3.5 0)'))
-- letter S
,ST_AsBinary(ST_AsEWKB('LINESTRING(3.5 0, 2.2 1, 3 3, 4.2 5, 5.2 3, 4.5 1, 3.5 0)'))
])
"))))
        (draw-gisobject (decode (car row))))
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
            (- nrange) nrange (- nrange) nrange))
    )

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
