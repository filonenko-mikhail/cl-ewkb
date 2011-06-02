(in-package :cl-wkb)

;;;; -----------------------------------------------------
;;;;
;;;; PRIMITIVE TYPES: Integers.
;;;;
(deftype uint8 ()
  '(unsigned-byte 8))
(deftype strictly-encoded-uint8 ()
  '(vector (unsigned-byte 8) 1))
(deftype encoded-uint8 ()
  '(or strictly-encoded-uint8 list))

(deftype uint32 ()
  '(unsigned-byte 32))
(deftype strictly-encoded-uint32 ()
  '(vector (unsigned-byte 8) 4))
(deftype encoded-uint32 ()
  '(or strictly-encoded-uint32 list))

(deftype uint64 ()
  '(unsigned-byte 64))
(deftype strictly-encoded-uint64 ()
  '(vector (unsigned-byte 8) 8))
(deftype encoded-uint64 ()
  '(or strictly-encoded-uint64 list))

;;; FIXME: eschew the bulk of the macro code in favor of more
;;; functional-style code, with the macros serving as thin
;;; syntactic-sugar.
(defmacro define-stream-encoder (name seq-function)
  (let ((value-var (gensym "VALUE-"))
        (endianness-var (gensym "ENDIANNESS-"))
        (buffer-var (gensym "BUFFER-"))
        (stream-var (gensym "STREAM-")))
    `(defun ,name (,value-var ,endianness-var ,stream-var)
       (let ((,buffer-var (,seq-function ,value-var ,endianness-var)))
         (write-sequence ,buffer-var ,stream-var)))))
(defmacro define-stream-decoder (name seq-function data-size-octets)
  (let ((endianness-var (gensym "ENDIANNESS-"))
        (stream-var (gensym "STREAM-"))
        (buffer-var (gensym "BUFFER-"))
        (read-len-var (gensym "READ-LEN-")))
    `(defun ,name (,endianness-var ,stream-var)
       (let* ((,buffer-var (make-array ,data-size-octets :element-type '(unsigned-byte 8)
                                       :initial-element 0))
              ;; FIXME: superfluous :END?
              (,read-len-var (read-sequence ,buffer-var ,stream-var :end ,data-size-octets)))
         (unless (= ,read-len-var ,data-size-octets)
           (error "Expected ~D octet~:P, got only ~D." ,data-size-octets ,read-len-var))
         (,seq-function ,buffer-var ,endianness-var)))))

(eval-when (:compile-toplevel)
  (let ((endiannesses `((:big-endian . ,(lambda (bit-offset bits)
                                                (1- (/ (- bits bit-offset) 8))))
                        (:little-endian . ,(lambda (bit-offset bits)
                                                   (declare (ignore bits))
                                                   (/ bit-offset 8))))))
    (defmacro def-uint-encoder (name bits lisp-type)
      "Defines a function with the given NAME that encodes an integer
of LISP-TYPE to a sequence of octets whose total number of bits equals
BITS.

The defined function takes two arguments: an integer, and an
endianness designator: :BIG-ENDIAN or :LITTLE-ENDIAN."
      (unless (and (plusp bits)
                   (zerop (mod bits 8)))
        (error "Can't define ~S: number of bits ~S must be positive and divisible by 8."
               name bits))
      (let ((int-var (gensym "INT-"))
            (endianness-var (gensym "ENDIANNESS-"))
            (out-var (gensym))
            (octets (/ bits 8)))
        `(defun ,name (,int-var ,endianness-var)
           (declare (type ,lisp-type ,int-var)
                    (type symbol ,endianness-var))
           (let ((,out-var (make-array '(,octets) :element-type '(unsigned-byte 8) :initial-element 0)))
             (ecase ,endianness-var
               ,@(loop for (endianness . octet-offset-fn) in endiannesses
                    collecting `(,endianness
                                 ,@(loop for bit-offset from 0 below bits by 8
                                      for octet-offset = (funcall octet-offset-fn bit-offset bits)
                                      collecting `(setf (elt ,out-var ,octet-offset)
                                                        (ldb (byte 8 ,bit-offset) ,int-var))))))
             ,out-var))))
    (defmacro def-uint-decoder (name bits lisp-type)
      (unless (and (plusp bits)
                   (zerop (mod bits 8)))
        (error "Can't define ~S: invalid number of bits ~S." name bits))
      (let ((octets-var (gensym "OCTETS-"))
            (endianness-var (gensym "ENDIANNESS-"))
            (out-var (gensym))
            (octets (/ bits 8)))
        `(defun ,name (,octets-var ,endianness-var)
           (declare (type (or (vector (unsigned-byte 8) ,octets)
                              list)
                          ,octets-var)
                    (type symbol ,endianness-var))
           (let ((,out-var (the ,lisp-type 0)))
             (declare (type ,lisp-type ,out-var))
             (ecase ,endianness-var
               ,@(loop for (endianness . octet-offset-fn) in endiannesses
                    collecting `(,endianness
                                 ,@(loop for bit-offset from 0 below bits by 8
                                      for octet-offset = (funcall octet-offset-fn bit-offset bits)
                                      collecting `(setf (ldb (byte 8 ,bit-offset) ,out-var)
                                                        (elt ,octets-var ,octet-offset))))))
             ,out-var))))))

(def-uint-encoder encode-uint8 8 uint8)
(define-stream-encoder encode-uint8-to encode-uint8)
(def-uint-decoder decode-uint8 8 uint8)
(define-stream-decoder decode-uint8-from decode-uint8 1)

(def-uint-encoder encode-uint32 32 uint32)
(define-stream-encoder encode-uint32-to encode-uint32)
(def-uint-decoder decode-uint32 32 uint32)
(define-stream-decoder decode-uint32-from decode-uint32 4)

(def-uint-encoder encode-uint64 64 uint64)
(define-stream-encoder encode-uint64-to encode-uint64)
(def-uint-decoder decode-uint64 64 uint64)
(define-stream-decoder decode-uint64-from decode-uint64 8)

;;;;
;;;; PRIMITIVE TYPES: Floating-point numbers.
;;;;

;;; For the sake of "efficiency," we're going to go out on a limb here
;;; and say this Lisp uses IEEE-754 internally, and that if a Lisp
;;; float type passes the most-negative / most-negative-normalized /
;;; most-positive tests for a double-precision 64-bit float, there
;;; will be no loss in precision in trying to express a
;;; double-precision 64-bit float in that Lisp float type.
;;; 
;;; This hack returns 'DOUBLE-FLOAT on SBCL x86_64.
(deftype ieee754-double ()
  (let* ((least-positive-64bit-float (expt 2 -1074))
         (least-negative-64bit-float (- least-positive-64bit-float))
         (least-positive-normalized-64bit-float (expt 2 -1022))
         (least-negative-normalized-64bit-float (- least-positive-normalized-64bit-float))
         (most-negative-64bit-float (* (1- (expt 1/2 53))
                                       (expt 2 1024)))
         (most-positive-64bit-float (- most-negative-64bit-float)))
    (flet ((cl-sym (&rest args)
             (intern (apply #'concatenate 'string args)
                     :common-lisp)))
      (loop ;; FIXME: CLISP 2.41 stack-overflows when taking the
         ;; rational of any of the six extreme long-float
         ;; constants.
         for type-abbr in '("SHORT" "SINGLE" "DOUBLE" #-CLISP "LONG")
         for type = (cl-sym type-abbr "-FLOAT")
         for type-name = (symbol-name type)
         ;; FIXME: CLISP 2.41 fails X3J13 issue
         ;; CONTAGION-ON-NUMERICAL-COMPARISONS (see CLHS 12.1.4.1).
         ;; When that bug is fixed, replace RATIONAL calls below
         ;; with their arguments.
         for least-positive = (rational (symbol-value (cl-sym "LEAST-POSITIVE-" type-name)))
         for least-negative = (rational (symbol-value (cl-sym "LEAST-NEGATIVE-" type-name)))
         for least-positive-normalized = (rational (symbol-value (cl-sym "LEAST-POSITIVE-NORMALIZED-" type-name)))
         for least-negative-normalized = (rational (symbol-value (cl-sym "LEAST-NEGATIVE-NORMALIZED-" type-name)))
         for most-positive = (rational (symbol-value (cl-sym "MOST-POSITIVE-" type-name)))
         for most-negative = (rational (symbol-value (cl-sym "MOST-NEGATIVE-" type-name)))
         when (and (<= least-positive least-positive-64bit-float)
                   (>= least-negative least-negative-64bit-float)
                   (<= least-positive-normalized least-positive-normalized-64bit-float)
                   (>= least-negative-normalized least-negative-normalized-64bit-float)
                   (>= most-positive most-positive-64bit-float)
                   (<= most-negative most-negative-64bit-float))
         return type
         finally (return 'rational)))))
(deftype strictly-encoded-ieee754-double ()
  '(vector (unsigned-byte 8) 8))
(deftype encoded-ieee754-double ()
  '(or strictly-encoded-ieee754-double list))

(defun encode-ieee754-double (float endianness)
  (declare (type ieee754-double float)
           (type symbol endianness))
  (the strictly-encoded-ieee754-double
    (encode-uint64 (ieee-floats:encode-float64 float) endianness)))
(define-stream-encoder encode-ieee754-double-to encode-ieee754-double)
(defun decode-ieee754-double (octets endianness)
  (declare (type encoded-ieee754-double octets)
           (type symbol endianness))
  (the ieee754-double
    (ieee-floats:decode-float64 (decode-uint64 octets endianness))))
(define-stream-decoder decode-ieee754-double-from decode-ieee754-double 8)

;;;;
;;;; API and COMMON PROCEDURES.
;;;;
(defclass point-primitive ()
  ((x :type ieee754-double :initarg :x :initform (error "Must specify X value.")
      :reader x)
   (y :type ieee754-double :initarg :y :initform (error "Must specify Y value.")
      :reader y)
   (z :type ieee754-double :initarg :z :initform 0.0d0
      :reader z)
   (m :type ieee754-double :initarg :m :initform 0.0d0
      :reader m)))

(defclass linear-ring ()
  ((points-primitive :type list :initarg :points-primitive :initform '()
		     :accessor points-primitive)))

(defclass geometry ()
  ((geomtype :type uint32 :initarg :geomtype :initform 0 :accessor geomtype)
   (srid :type uint32 :initarg :srid :initform 0 :accessor srid)))

(defclass point (geometry)
  ((point-primitive :type point-primitive :initarg :point-primitive :reader point-primitive)))

(defmethod initialize-instance :after ((point point) &key point-primitive x y (z 0.0d0) (m 0.0d0))
  (when (and (or (null x) (null y))
	     (null point-primitive))
    (error "Must specify :X and :Y values, or :POINT-PRIMITIVE."))
  (when (and (not (null point-primitive))
	     (or (not (null x))
		 (not (null y))))
    (error "Can't specify specify both :X and :Y values and :POINT-PRIMITIVE."))
  (when (null point-primitive)
    (setf (slot-value point 'point-primitive)
	  (make-instance 'point-primitive :x x :y y :z z :m m))))
(defmethod x ((point point))
  (x (point-primitive point)))
(defmethod y ((point point))
  (y (point-primitive point)))
(defmethod z ((point point))
  (z (point-primitive point)))
(defmethod m ((point point))
  (m (point-primitive point)))
(defmethod print-object ((point point) stream)
  (print-unreadable-object (point stream :type t)
    (format stream "~,5F, ~,5F ~,5F ~,5F" (x point) (y point) (z point) (m point))))

(defclass line-string (geometry)
  ((points-primitive :type list :initarg :points-primitive :initform '()
		     :accessor points-primitive)))
(defmethod print-object ((line-string line-string) stream)
  (print-unreadable-object (line-string stream :type t)
    (format stream "~{~A~^ ~}" (points-primitive line-string))))

(defclass polygon (geometry)
  ((linear-rings :type list :initarg :linear-rings :initform '()
		 :accessor linear-rings)))
(defmethod print-object ((polygon polygon) stream)
  (print-unreadable-object (polygon stream :type t)
    (format stream "~{~A~^ ~}" (linear-rings polygon))))

(defclass multi-point (geometry)
  ((points :type list :initarg :points :initform '()
	   :accessor points)))
(defmethod print-object ((multi-point multi-point) stream)
  (print-unreadable-object (multi-point stream :type t)
    (format stream "~{~A~^ ~}" (points multi-point))))

(defclass multi-line-string (geometry)
  ((line-strings :type list :initarg :line-strings :initform '()
		 :accessor line-strings)))
(defmethod print-object ((multi-line-string multi-line-string) stream)
  (print-unreadable-object (multi-line-string stream :type t)
    (format stream "~{~A~^ ~}" (line-strings multi-line-string))))

(defclass multi-polygon (geometry)
  ((polygons :type list :initarg :polygons :initform '()
	     :accessor polygons)))
(defmethod print-object ((multi-polygon multi-polygon) stream)
  (print-unreadable-object (multi-polygon stream :type t)
    (format stream "~{~A~^ ~}" (polygons multi-polygon))))

(defclass geometry-collection (geometry)
  ((geometries :type list :initarg :geometries :initform '()
	       :accessor geometries)))
(defmethod print-object ((geometry-collection geometry-collection) stream)
  (print-unreadable-object (geometry-collection stream :type t)
    (format stream "~{~A~^ ~}" (geometries geometry-collection))))

(defparameter +endiannesses+
  '((0 . :big-endian)
    (1 . :little-endian)))

(defparameter +wkb-z+ #x80000000)
(defparameter +wkb-m+ #x40000000)
(defparameter +wkb-srid+ #x20000000)

(defparameter +wkb-typemask+ #x0000000F)
(defparameter +wkb-types+
  '((1 . :point)
    (2 . :linestring)
    (3 . :polygon)
    (4 . :multi-point)
    (5 . :multi-linestring)
    (6 . :multi-polygon)
    (7 . :geometry-collection)))

(defun dimension (type)
  (if (zerop (logand +wkb-z+ type))
      (if (zerop (logand +wkb-m+ type))
          :2d
          :3dm)
      (if (zerop (logand +wkb-m+ type))
          :3dz
          :4d)))

(defgeneric generic-decode-primitive-point (type in endianness)
  (:documentation "Generic decode function for primitive point")
  (:method ((type (eql :2d)) in endianness)
    (make-instance 'point-primitive
                   :x (decode-ieee754-double-from endianness in)
                   :y (decode-ieee754-double-from endianness in)))
  (:method ((type (eql :3dm)) in endianness)
    (make-instance 'point-primitive
                   :x (decode-ieee754-double-from endianness in)
                   :y (decode-ieee754-double-from endianness in)
                   :m (decode-ieee754-double-from endianness in)))
  (:method ((type (eql :3dz)) in endianness)
    (make-instance 'point-primitive
                   :x (decode-ieee754-double-from endianness in)
                   :y (decode-ieee754-double-from endianness in)
                   :z (decode-ieee754-double-from endianness in)))
  (:method ((type (eql :4d)) in endianness)
    (make-instance 'point-primitive
                   :x (decode-ieee754-double-from endianness in)
                   :y (decode-ieee754-double-from endianness in)
                   :z (decode-ieee754-double-from endianness in)
                   :m (decode-ieee754-double-from endianness in))))

(defun decode-primitive-point (in type endianness)
  (generic-decode-primitive-point (dimension type) in endianness))

(defun decode-linear-ring (in type endianness)
  (let ((data '()))
    (dotimes (i (decode-uint32-from endianness in))
      (push (decode-primitive-point in type endianness) data))
    (make-instance 'linear-ring :points-primitive (nreverse data))))

(defun decode-from (in)
  "Function to decode geoobject from WKB/EWKB representation from stream."
  (let* ((endianness (cdr (assoc (decode-uint8-from :big-endian in) +endiannesses+ :test #'=)))
         (type (decode-uint32-from endianness in))
         (srid 0)
         (data '()))
    (unless (zerop (logand +wkb-srid+ type))
      (setf srid (decode-uint32-from endianness in)))
    (case (cdr (assoc (logand type +wkb-typemask+) +wkb-types+ :test #'=))
      (:point
       (make-instance 'point :geomtype type :srid srid
                      :point-primitive (decode-primitive-point in type endianness)))
      (:linestring
       (dotimes (i (decode-uint32-from endianness in))
         (push (decode-primitive-point in type endianness) data))
       (make-instance 'line-string :geomtype type :srid srid
                      :points-primitive  (nreverse data)))
      (:polygon
       (dotimes (i (decode-uint32-from endianness in))
         (push (decode-linear-ring in type endianness) data))
       (make-instance 'polygon :geomtype type :srid srid :linear-rings (nreverse data)))
      (:multi-point
       (dotimes (i (decode-uint32-from endianness in))
         (push (decode-from in) data))
       (make-instance 'multi-point :geomtype type :srid srid :points (nreverse data)))
      (:multi-linestring
       (dotimes (i (decode-uint32-from endianness in))
         (push (decode-from in) data))
       (make-instance 'multi-line-string :geomtype type :srid srid :line-strings (nreverse data)))
      (:multi-polygon
       (dotimes (i (decode-uint32-from endianness in))
         (push (decode-from in) data))
       (make-instance 'multi-polygon :geomtype type :srid srid :polygons (nreverse data)))
      (:geometry-collection
       (dotimes (i (decode-uint32-from endianness in))
         (push (decode-from in) data))
       (make-instance 'geometry-collection :geomtype type :srid srid :geometries (nreverse data))))))

(defun decode (octets)
  "Function to decode geoobject from WKB/EWKB representation from sequence."
  (flexi-streams:with-input-from-sequence (in octets)
    (decode-from in)))

(defgeneric generic-encode-primitive-point (type object out endianness)
  (:documentation "Generic decode function for primitive point")
  (:method ((type (eql :2d)) object out endianness)
    (encode-ieee754-double-to (slot-value object 'x) endianness out)
    (encode-ieee754-double-to (slot-value object 'y) endianness out))
  (:method ((type (eql :3dm)) object out endianness)
    (encode-ieee754-double-to (slot-value object 'x) endianness out)
    (encode-ieee754-double-to (slot-value object 'y) endianness out)
    (encode-ieee754-double-to (slot-value object 'm) endianness out))
  (:method ((type (eql :3dz)) object out endianness)
    (encode-ieee754-double-to (slot-value object 'x) endianness out)
    (encode-ieee754-double-to (slot-value object 'y) endianness out)
    (encode-ieee754-double-to (slot-value object 'z) endianness out))
  (:method ((type (eql :4d)) object out endianness)
    (encode-ieee754-double-to (slot-value object 'x) endianness out)
    (encode-ieee754-double-to (slot-value object 'y) endianness out)
    (encode-ieee754-double-to (slot-value object 'z) endianness out)
    (encode-ieee754-double-to (slot-value object 'm) endianness out)))

(defun encode-primitive-point (object out type endianness)
  (generic-encode-primitive-point (dimension type) object out endianness))

(defun encode-linear-ring (object out type endianness)
  (encode-uint32-to (length (slot-value object 'points-primitive)) endianness out)
  (map 'nil (lambda (point) (encode-primitive-point point out type endianness))
       (points-primitive object)))

(defun encode-to (object stream &optional (endianness :little-endian))
  "Function to encode geoobject to WKB/EWKB representation to binary stream. Endianness: :little-endian, :big-endian"
  (encode-uint8-to (car (rassoc endianness +endiannesses+ :test #'equal)) :big-endian stream)
  (let*
      ((type (geomtype object))
       (srid (srid object)))
    (encode-uint32-to type endianness stream)
    (unless (zerop (logand +wkb-srid+ type))
      (encode-uint32-to srid endianness stream))
    (case (cdr (assoc (logand type +wkb-typemask+) +wkb-types+ :test #'=))
      (:point
       (encode-primitive-point (point-primitive object) stream type endianness))
      (:linestring
       (encode-uint32-to (length (points-primitive object)) endianness stream)
       (map 'nil (lambda (point)
                   (encode-primitive-point point stream type endianness))
            (points-primitive object)))
      (:polygon
       (encode-uint32-to (length (linear-rings object)) endianness stream)
       (map 'nil (lambda (line)
                   (encode-linear-ring line stream type endianness))
            (linear-rings object)))
      (:multi-point
       (encode-uint32-to (length (points object)) endianness stream)
       (map 'nil (lambda (object)
                   (encode-to object stream endianness))
            (points object)))
      (:multi-linestring
       (encode-uint32-to (length (line-strings object)) endianness stream)
       (map 'nil (lambda (object)
                   (encode-to object stream endianness))
            (line-strings object)))
      (:multi-polygon
       (encode-uint32-to (length (polygons object)) endianness stream)
        (map 'nil (lambda (object)
                   (encode-to object stream endianness))
            (polygons object)))
      (:geometry-collection
       (encode-uint32-to (length (geometries object)) endianness stream)
       (map 'nil (lambda (object)
                   (encode-to object stream endianness))
            (geometries object))))))

(defun encode (object &optional (endianness :little-endian))
  "Function to encode geoobject to WKB/EWKB representation to sequence. Endianness: :little-endian, :big-endian"
  (flexi-streams:with-output-to-sequence (out)
    (encode-to object out endianness)))

