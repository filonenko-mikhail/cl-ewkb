(defpackage :cl-ewkb-tests
  (:use :common-lisp :postmodern :cl-ewkb) ;; or cl-wkb
  (:export #:test-cl-ewkb))

(in-package :cl-ewkb-tests)

(defparameter *test-connection* '("osm" "guest" "guest" "gis-lab.info"))

(defun prompt-connection (&optional (list *test-connection*))
  (flet ((ask (name pos)
           (format *query-io* "~a (enter to keep '~a'): " name (nth pos list))
           (finish-output *query-io*)
           (let ((answer (read-line *query-io*)))
             (unless (string= answer "") (setf (nth pos list) answer)))))
    (format *query-io* "~%To run this test, you must configure a database connection.~%")
    (ask "Database name" 0)
    (ask "User" 1)
    (ask "Password" 2)
    (ask "Hostname" 3)))

(defmacro with-gensyms ((&rest names) &body body)
    `(let ,(loop for n in names collect `(,n (gensym)))
         ,@body))

(defvar *test-name* nil)
(defmacro deftest (name parameters &body body)
    "Define a test function. Within a test function we can call
other test functuins or use 'check' to run individual test
cases."
    `(defun ,name ,parameters
         (let ((*test-name* (append *test-name* (list ',name))))
             ,@body)))
(defmacro check (&body forms)
    "Run each expression in 'forms' as a test case."
    `(combine-results
         ,@(loop for f in forms collect `(report-result ,f ',f))))
(defmacro combine-results (&body forms)
    "Combine the results (as booleans) of evaluating 'forms' in order."
    (with-gensyms (result)
        `(let ((,result t))
             ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
             ,result)))
(defun report-result (result form)
    "Report the results of a single test case. Called by 'check'."
    (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
    result)

(deftest gispoint-test ()
    (let ((gispoint (caar (query (:select (:ST_AsEWKB "POINT(0 0)")))))
             (gispointz (caar (query (:select (:ST_AsEWKB "POINT(0 0 1)")))))
             (gispointzm (caar (query (:select (:ST_AsEWKB "POINT(0 0 1 2)")))))
             (gispointm (caar (query (:select (:ST_AsEWKB "POINTM(0 0 2)")))))
             (gispointzm-srid (caar (query (:select (:ST_AsEWKB "SRID=4326;POINT(0 0 1 2)"))))))
        (check
            (equalp gispoint (encode (decode gispoint)))
            (equalp gispointz (encode (decode gispointz)))
            (equalp gispointm (encode (decode gispointm)))
            (equalp gispointzm (encode (decode gispointzm)))
            (equalp gispointzm-srid (encode (decode gispointzm-srid)))
            )))

(deftest gislinestring-test ()
    (let (
             (gislinestring (caar (query (:select (:ST_AsEWKB "LINESTRING(0 0, 1 1, 2 2)")))))
             (gislinestringz (caar (query (:select (:ST_AsEWKB "LINESTRING(0 0 0, 1 1 1, 2 2 2)")))))
             (gislinestringzm (caar (query (:select (:ST_AsEWKB "LINESTRING(0 0 1 2, 1 1 2 3, 2 2 3 4)")))))
             (gislinestringm (caar (query (:select (:ST_AsEWKB "LINESTRINGM(0 0 2, 1 1 3, 2 2 4)")))))
             (gislinestringzm-srid (caar (query (:select (:ST_AsEWKB "SRID=4326;LINESTRING(0 0 1 2, 1 1 2 3, 2 2 3 4)"))))))
        (check
            (equalp gislinestring (encode (decode gislinestring)))
            (equalp gislinestringz (encode (decode gislinestringz)))
            (equalp gislinestringzm (encode (decode gislinestringzm)))
            (equalp gislinestringm (encode (decode gislinestringm)))
            (equalp gislinestringzm-srid (encode (decode gislinestringzm-srid)))
            )))

(deftest gispolygon-test ()
    (let (
             (gispolygon (caar (query (:select (:ST_AsEWKB "POLYGON((0 0 ,4 0 ,4 4 ,0 4 ,0 0 ),(1 1 ,2 1 ,2 2 ,1 2 ,1 1 ))")))))
             (gispolygonz (caar (query (:select (:ST_AsEWKB "POLYGON((0 0 0,4 0 0,4 4 0,0 4 0,0 0 0),(1 1 0,2 1 0,2 2 0,1 2 0,1 1 0))")))))
             (gispolygonzm (caar (query (:select (:ST_AsEWKB "POLYGON((0 0 0 0,4 0 0 1,4 4 0 1,0 4 0 2,0 0 0 3),(1 1 0 4,2 1 0 5,2 2 0 6,1 2 0 7,1 1 0 8))")))))
             (gispolygonzm-srid (caar (query (:select (:ST_AsEWKB "SRID=4326;POLYGON((0 0 0 0,4 0 0 1,4 4 0 1,0 4 0 2,0 0 0 3),(1 1 0 4,2 1 0 5,2 2 0 6,1 2 0 7,1 1 0 8))"))))))
        (check
            (equalp gispolygon (encode (decode gispolygon)))
            (equalp gispolygonz (encode (decode gispolygonz)))
            (equalp gispolygonzm (encode (decode gispolygonzm)))
            (equalp gispolygonzm-srid (encode (decode gispolygonzm-srid)))
            )))

(deftest gismultipoint-test ()
    (let (
             (multi-gispoint (caar (query (:select (:ST_AsEWKB "MULTIPOINT(0 0, 1 1)")))))
             (multi-gispointz (caar (query (:select (:ST_AsEWKB "MULTIPOINT(0 0 1, 1 1 2)")))))
             (multi-gispointzm (caar (query (:select (:ST_AsEWKB "MULTIPOINT(0 0 1 2, 1 1 2 3)")))))
             (multi-gispointzm-srid (caar (query (:select (:ST_AsEWKB "SRID=4326;MULTIPOINT(0 0 1 2, 1 1 2 3)"))))))
        (check
            (equalp multi-gispoint (encode (decode multi-gispoint)))
            (equalp multi-gispointz (encode (decode multi-gispointz)))
            (equalp multi-gispointzm (encode (decode multi-gispointzm)))
            (equalp multi-gispointzm-srid (encode (decode multi-gispointzm-srid)))
            )))

(deftest gismultilinestring-test ()
    (let (
             (multi-gislinestring (caar (query (:select (:ST_AsEWKB "MULTILINESTRING((0 0 ,1 1 ,1 2 ),(2 3 ,3 2 ,5 4 ))")))))
             (multi-gislinestringz (caar (query (:select (:ST_AsEWKB "MULTILINESTRING((0 0 0,1 1 0,1 2 1),(2 3 1,3 2 1,5 4 1))")))))
             (multi-gislinestringzm (caar (query (:select (:ST_AsEWKB "MULTILINESTRING((0 0 0 1,1 1 0 2,1 2 1 3),(2 3 1 2,3 2 1 3,5 4 1 4))")))))
             (multi-gislinestringzm-srid (caar (query (:select (:ST_AsEWKB "SRID=4326;MULTILINESTRING((0 0 0 1,1 1 0 2,1 2 1 3),(2 3 1 2,3 2 1 3,5 4 1 4))"))))))
        (check
            (equalp multi-gislinestring (encode (decode multi-gislinestring)))
            (equalp multi-gislinestringz (encode (decode multi-gislinestringz)))
            (equalp multi-gislinestringzm (encode (decode multi-gislinestringzm)))
            (equalp multi-gislinestringzm-srid (encode (decode multi-gislinestringzm-srid)))
            )))

(deftest gismultipolygon-test ()
    (let (
             (multi-gispolygon (caar (query (:select (:ST_AsEWKB "MULTIPOLYGON(((0 0,4 0,4 4,0 4,0 0),(1 1,2 1,2 2,1 2,1 1)), ((-1 -1,-1 -2,-2 -2,-2 -1,-1 -1)))")))))
             (multi-gispolygonz (caar (query (:select (:ST_AsEWKB "MULTIPOLYGON(((0 0 1,4 0 2,4 4 3,0 4 4,0 0 5),(1 1 6,2 1 7,2 2 8,1 2 9,1 1 10)), ((-1 -1 -1,-1 -2 -3,-2 -2 -2,-2 -1 -1,-1 -1 -2)))")))))
             (multi-gispolygonzm (caar (query (:select (:ST_AsEWKB "MULTIPOLYGON(((0 0 1 1,4 0 2 2,4 4 3 3,0 4 4 4,0 0 5 5),(1 1 1 6,2 1 2 7,2 2 3 8,1 2 4 9,1 1 5 10)), ((-1 -1 5 -1,-1 6 -2 -3,-2 -2 -2 -2,-2 -5 -1 -1,-1 -1 -1 -2)))")))))
             (multi-gispolygonzm-srid (caar (query (:select (:ST_AsEWKB "SRID=4326;MULTIPOLYGON(((0 0 1 1,4 0 2 2,4 4 3 3,0 4 4 4,0 0 5 5),(1 1 1 6,2 1 2 7,2 2 3 8,1 2 4 9,1 1 5 10)), ((-1 -1 5 -1,-1 6 -2 -3,-2 -2 -2 -2,-2 -5 -1 -1,-1 -1 -1 -2)))"))))))
        (check
            (equalp multi-gispolygon (encode (decode multi-gispolygon)))
            (equalp multi-gispolygonz (encode (decode multi-gispolygonz)))
            (equalp multi-gispolygonzm (encode (decode multi-gispolygonzm)))
            (equalp multi-gispolygonzm-srid (encode (decode multi-gispolygonzm-srid)))
            )))

(deftest gisgeometrycollection-test ()
    (let (
             (geometrycollection (caar (query (:select (:ST_AsEWKB "GEOMETRYCOLLECTION(POINT(2 3),LINESTRING(2 3,3 4))"))))))
        (check
            (equalp geometrycollection (encode (decode geometrycollection))))))

(deftest test-cl-ewkb ()
  (prompt-connection)
  (with-connection *test-connection*
    (combine-results
      (gispoint-test)
      (gislinestring-test)
      (gispolygon-test)
      (gismultipoint-test)
      (gismultilinestring-test)
      (gismultipolygon-test)
      (gisgeometrycollection-test))))