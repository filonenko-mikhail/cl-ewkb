;;(in-package :cl-ewkb)
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

(defmacro defstruct-and-export (structure &rest members)
  "Define a structure STRUCT with members MEMBERS and export the
   standard functions created. SPECIALS is a list of extra parameters eg
   ((:print-function pf)). Note double parentheses."
  (append
   `(progn
      ,(if (not (null members))
           (if (stringp (car members))
               `(defstruct ,structure ,(car members) ,@(cdr members))
               `(defstruct ,structure ,@members))
           `(defstruct ,structure))
      ,`(export ,`(quote ,(intern (concatenate 'string "MAKE-" (symbol-name (car structure))))))
      ,`(export ,`(quote ,(intern (concatenate 'string "COPY-" (symbol-name (car structure)))))))
   (if (not (null members))
       (if (stringp (car members))
           (mapcar  #'(lambda (member)
                        `(export ,`(quote ,(intern (concatenate 'string (symbol-name (car structure)) "-" (symbol-name (car member))))))) (cdr members))
           (mapcar  #'(lambda (member)
                        `(export ,`(quote ,(intern (concatenate 'string (symbol-name (car structure)) "-" (symbol-name (car member))))))) members)))
   (if (find :named structure)
       `((export ,`(quote ,(intern (concatenate 'string (symbol-name (car structure)) "-P" ))))
         (deftype ,(intern (symbol-name (car structure))) () '(satisfies ,(intern (concatenate 'string (symbol-name (car structure)) "-P" ))))
         ))))

(defstruct-and-export (point-primitive (:type vector)
                                       :named
                                       (:constructor make-point-primitive (x y)))
    "2d point data - struct contains x y coordinates."
  (x 0.0d0 :type ieee754-double)
  (y 0.0d0 :type ieee754-double))


(defstruct-and-export (pointz-primitive (:type vector)
                                        :named
                                        (:include point-primitive)
                                        (:constructor make-pointz-primitive (x y z)))
    "3dz point data - struct contains x y z coordinates."
  (z 0.0d0 :type ieee754-double))

(defstruct-and-export (pointm-primitive (:type vector)
                                        :named
                                        (:include point-primitive)
                                        (:constructor make-pointm-primitive (x y m)))
    "3dm point data - struct contains x y m coordinates."
  (m 0.0d0 :type ieee754-double))

(defstruct-and-export (pointzm-primitive (:type vector)
                                         :named
                                         (:include pointz-primitive)
                                         (:constructor make-pointzm-primitive (x y z m)))
    "4d point data - struct contains x y z m coordinates."
  (m 0.0d0 :type ieee754-double))

(defstruct-and-export (linear-ring (:type vector)
                                   :named
                                   (:constructor make-linear-ring (points-primitive)))
    (points-primitive nil :type vector))

(defstruct-and-export (geometry (:type vector)
                          :named
                          (:constructor make-gisgeometry (type srid)))
    (type 0 :type uint32)
    (srid 0 :type uint32))

(defstruct-and-export (point (:type vector)
                             :named
                             (:include geometry)
                             (:constructor make-point (type srid point-primitive)))
    (point-primitive nil :type vector))

(defstruct-and-export (line-string (:type vector)
                                   :named
                                   (:include geometry)
                                   (:constructor make-line-string (type srid points-primitive)))
    (points-primitive nil :type vector))

(defstruct-and-export (polygon (:type vector)
                               :named
                               (:include geometry)
                               (:constructor make-polygon (type srid linear-rings)))
    (linear-rings nil :type vector))

(defstruct-and-export (multi-point (:type vector)
                                  :named
                                  (:include geometry)
                                  (:constructor make-multi-point (type srid points)))
    (points nil :type vector))

(defstruct-and-export (multi-line-string (:type vector)
                                       :named
                                       (:include geometry)
                                       (:constructor make-multi-line-string (type srid line-strings)))
    (line-strings nil :type vector))

(defstruct-and-export (multi-polygon (:type vector)
                                    :named
                                    (:include geometry)
                                    (:constructor make-multi-polygon (type srid polygons)))
    (polygons nil :type vector))

(defstruct-and-export (geometry-collection (:type vector)
                                    :named
                                    (:include geometry)
                                    (:constructor make-geometry-collection (type srid geometries)))
    (geometries nil :type vector))

;;; FIXME: document these functions.
(defparameter +endiannesses+
  '((0 . :big-endian)
    (1 . :little-endian)))

(defparameter +wkb-z+ #x80000000)
(defparameter +wkb-m+ #x40000000)
(defparameter +wkb-srid+ #x20000000)

(defparameter +wkb-typemask+ #x0000000F)
(defparameter +wkb-types+
  '(
    (1 . :point)
    (2 . :line-string)
    (3 . :polygon)
    (4 . :multi-point)
    (5 . :multi-line-string)
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
    (make-point-primitive
     (decode-ieee754-double-from endianness in)
     (decode-ieee754-double-from endianness in)))
  (:method ((type (eql :3dm)) in endianness)
    (make-pointm-primitive
     (decode-ieee754-double-from endianness in)
     (decode-ieee754-double-from endianness in)
     (decode-ieee754-double-from endianness in)))
  (:method ((type (eql :3dz)) in endianness)
    (make-pointz-primitive
     (decode-ieee754-double-from endianness in)
     (decode-ieee754-double-from endianness in)
     (decode-ieee754-double-from endianness in)))
  (:method ((type (eql :4d)) in endianness)
    (make-pointzm-primitive
     (decode-ieee754-double-from endianness in)
     (decode-ieee754-double-from endianness in)
     (decode-ieee754-double-from endianness in)
     (decode-ieee754-double-from endianness in))))

(defun decode-primitive-point (in type endianness)
  (generic-decode-primitive-point (dimension type) in endianness))

(defun decode-linear-ring (in type endianness)
  (let ((data (make-array 0 :fill-pointer 0 :element-type 'vector :adjustable T)))
    (dotimes (i (decode-uint32-from endianness in))
      (vector-push-extend (decode-primitive-point in type endianness) data))
    (make-linear-ring data)))

(defun decode-from (in)
  "Function to decode geoobject from WKB/EWKB representation from stream."
  (let* ((endianness (cdr (assoc (decode-uint8-from :big-endian in) +endiannesses+ :test #'=)))
         (type (decode-uint32-from endianness in))
         (srid 0)
         (data (make-array 0 :fill-pointer 0 :element-type 'vector :adjustable T)))
    (unless (zerop (logand +wkb-srid+ type))
      (setf srid (decode-uint32-from endianness in)))
    (case (cdr (assoc (logand type +wkb-typemask+) +wkb-types+ :test #'=))
      (:point
       (make-point type srid (decode-primitive-point in type endianness)))
      (:line-string
       (dotimes (i (decode-uint32-from endianness in))
         (vector-push-extend (decode-primitive-point in type endianness) data))
       (make-line-string type srid data))
      (:polygon
       (dotimes (i (decode-uint32-from endianness in))
         (vector-push-extend (decode-linear-ring in type endianness) data))
       (make-polygon type srid data))
      (:multi-point
       (dotimes (i (decode-uint32-from endianness in))
         (vector-push-extend (decode-from in) data))
       (make-multi-point type srid data))
      (:multi-line-string
       (dotimes (i (decode-uint32-from endianness in))
         (vector-push-extend (decode-from in) data))
       (make-multi-line-string type srid data))
      (:multi-polygon
       (dotimes (i (decode-uint32-from endianness in))
         (vector-push-extend (decode-from in) data))
       (make-multi-polygon type srid data))
      (:geometry-collection
       (dotimes (i (decode-uint32-from endianness in))
         (vector-push-extend (decode-from in) data))
       (make-gisgeometry type srid data)))))

(defun decode (octets)
  "Function to decode geoobject from WKB/EWKB representation from sequence."
  (flexi-streams:with-input-from-sequence (in octets)
    (decode-from in)))

(defgeneric generic-encode-primitive-point (type object out endianness)
  (:documentation "Generic decode function for primitive point")
  (:method ((type (eql :2d)) object out endianness)
    (encode-ieee754-double-to (point-primitive-x object) endianness out)
    (encode-ieee754-double-to (point-primitive-y object) endianness out))
  (:method ((type (eql :3dm)) object out endianness)
    (encode-ieee754-double-to (pointm-primitive-x object) endianness out)
    (encode-ieee754-double-to (pointm-primitive-y object) endianness out)
    (encode-ieee754-double-to (pointm-primitive-m object) endianness out))
  (:method ((type (eql :3dz)) object out endianness)
    (encode-ieee754-double-to (pointz-primitive-x object) endianness out)
    (encode-ieee754-double-to (pointz-primitive-y object) endianness out)
    (encode-ieee754-double-to (pointz-primitive-z object) endianness out))
  (:method ((type (eql :4d)) object out endianness)
    (encode-ieee754-double-to (pointzm-primitive-x object) endianness out)
    (encode-ieee754-double-to (pointzm-primitive-y object) endianness out)
    (encode-ieee754-double-to (pointzm-primitive-z object) endianness out)
    (encode-ieee754-double-to (pointzm-primitive-m object) endianness out)))

(defun encode-primitive-point (object out type endianness)
  (generic-encode-primitive-point (dimension type) object out endianness))

(defun encode-linear-ring (object out type endianness)
  (encode-uint32-to (length (linear-ring-points-primitive object)) endianness out)
  (map 'nil
       (lambda (point) (encode-primitive-point point out type endianness))
       (linear-ring-points-primitive object)))

(defun encode-to (object stream &optional (endianness :little-endian))
  "Function to encode geoobject to WKB/EWKB representation to binary stream. Endianness: :little-endian, :big-endian"
  (encode-uint8-to (car (rassoc endianness +endiannesses+ :test #'equal)) :big-endian stream)
  (let*
      ((type (geometry-type object))
       (srid (geometry-srid object)))
    (encode-uint32-to type endianness stream)
    (unless (zerop (logand +wkb-srid+ type))
      (encode-uint32-to srid endianness stream))
    (case (cdr (assoc (logand type +wkb-typemask+) +wkb-types+ :test #'=))
      (:point
       (encode-primitive-point (point-point-primitive object) stream type endianness))
      (:line-string
       (encode-uint32-to (length (line-string-points-primitive object)) endianness stream)
       (map 'nil (lambda (point)
                   (encode-primitive-point point stream type endianness))
            (line-string-points-primitive object)))
      (:polygon
       (encode-uint32-to (length (polygon-linear-rings object)) endianness stream)
       (map 'nil (lambda (line)
                   (encode-linear-ring line stream type endianness))
            (polygon-linear-rings object)))
      (:multi-point
       (encode-uint32-to (length (multi-point-points object)) endianness stream)
       (map 'nil (lambda (object)
                   (encode-to object stream endianness))
            (multi-point-points object)))
      (:multi-line-string
       (encode-uint32-to (length (multi-line-string-line-strings object)) endianness stream)
       (map 'nil (lambda (object)
                   (encode-to object stream endianness))
            (multi-line-string-line-strings object)))
      (:multi-polygon
       (encode-uint32-to (length (multi-polygon-polygons object)) endianness stream)
       (map 'nil (lambda (object)
                   (encode-to object stream endianness))
            (multi-polygon-polygons object)))
      (:geometry-collection
       (encode-uint32-to (length (geometry-collection-geometries object)) endianness stream)
       (map 'nil (lambda (object)
                   (encode-to object stream endianness))
            (geometry-collection-geometries object))))))

(defun encode (object &optional (endianness :little-endian))
  "Function to encode geoobject to WKB/EWKB representation to sequence. Endianness: :little-endian, :big-endian"
  (flexi-streams:with-output-to-sequence (out)
    (encode-to object out endianness)))

