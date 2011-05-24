(in-package :common-lisp-user)

(defpackage :cl-ewkb
    (:use :common-lisp :ieee-floats
        :flexi-streams)
    (:export #:encode
        #:encode-to
        #:decode
        #:decode-from
        ))

(in-package :cl-ewkb)