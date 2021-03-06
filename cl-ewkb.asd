(defpackage :cl-ewkb-system
  (:use :common-lisp :asdf))

(in-package :cl-ewkb-system)

(defsystem :cl-ewkb
  :version "0.2"
  :maintainer "Michael Filonenko <filonenko.mikhail@gmail.com>"
  :author "Michael Filonenko <filonenko.mikhail@gmail.com>"
  :licence "MIT"
  :description "cl-ewkb is a geospatial library, based on cl-wkb, that implements the OGC Well-Known Binary geographic geometry data model with PostGIS 3d, 4d extensions, and provides WKB and EWKB encoding and decoding functionality. cl-wkb author is J.P. Larocue."
  :depends-on (:ieee-floats :flexi-streams)
  :components 
  ((:module :cl-ewkb
            :components ((:file "package")
                         (:file "ewkb" :depends-on ("package"))))
   (:module :cl-wkb
            :components ((:file "package")
                         (:file "wkb" :depends-on ("package"))))))

(defpackage :cl-ewkb-tests-system
  (:use :common-lisp :asdf))

(in-package :cl-ewkb-tests-system)

(defsystem :cl-ewkb-tests
  :depends-on (:cl-ewkb :postmodern)
  :components
  ((:module :test
            :components ((:file "tests")))))
