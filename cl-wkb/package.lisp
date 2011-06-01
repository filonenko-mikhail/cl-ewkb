(in-package :common-lisp-user)

(defpackage :cl-wkb
    (:nicknames :wkb)
    (:use :common-lisp :ieee-floats
        :flexi-streams)
    (:export #:ieee754-double

	   #:encode #:encode-to #:decode #:decode-from
	   
	   #:point-primitive #:x #:y #:z #:m
	   #:linear-ring #:points-primitive
	   
	   #:geometry
	   #:point #:line-string
	   #:polygon #:linear-rings
	   #:multi-point #:points
	   #:multi-line-string #:line-strings
	   #:multi-polygon #:polygons
	   #:geometry-collection #:geometries))

(in-package :cl-wkb)