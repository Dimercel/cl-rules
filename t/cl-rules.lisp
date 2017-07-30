(in-package :cl-user)
(defpackage cl-rules-test
  (:use :cl :prove)
  (:import-from :cl-rules
                :defparam
                :setparam
                :defcond
                :defrule
                :fire-rule))
(in-package :cl-rules-test)


(plan 15)

(defparam vertex-count 1)
(defparam angles nil)


(defcond vertex-count-is (param number)
  (= param number))

(defcond sum-of-angles-is-180 (angles)
  (= (apply '+ angles) 180))

(defcond one-angle-is (angles value)
  (find value angles))

(defcond one-angle-more-than (angles value)
  (some (lambda (x)
          (> x value))
        angles))

(defcond all-angles-less-than (angles value)
  (every (lambda (x)
           (< x value))
         angles))

(defcond all-angles-is (angles value)
  (every (lambda (x)
           (= x value))
         angles))

(defcond is-true (value)
  (not (null value)))


(defrule point
  (vertex-count-is vertex-count 1))

(defrule line-segment
  (vertex-count-is vertex-count 2))

(defrule triangle
  (vertex-count-is vertex-count 3)
  (sum-of-angles-is-180 angles))

(defrule right-triangle
  (vertex-count-is vertex-count 3)
  (sum-of-angles-is-180 angles)
  (one-angle-is angles 90))

(defrule obtuse-triangle
  (is-true (fire-rule 'triangle))
  (one-angle-more-than angles 90))

(defrule acute-triangle
  (is-true (fire-rule 'triangle))
  (all-angles-less-than angles 90))

(defrule square
  (vertex-count-is vertex-count 4)
  (all-angles-is angles 90))


(ok (fire-rule 'point))
(ok (not (fire-rule 'line-segment 'triangle 'square)))

(setparam vertex-count 2)
(setparam angles nil)

(ok (fire-rule 'line-segment))
(ok (not (fire-rule 'point 'triangle 'square)))

(setparam vertex-count 3)
(setparam angles '(60 60 60))

(ok (fire-rule 'triangle 'acute-triangle))
(ok (not (fire-rule 'right-triangle 'obtuse-triangle)))
(ok (not (fire-rule 'point 'line-segment 'square )))

(setparam vertex-count 3)
(setparam angles '(90 45 45))

(ok (fire-rule 'triangle 'right-triangle))
(ok (not (fire-rule 'obtuse-triangle 'acute-triangle)))
(ok (not (fire-rule 'point 'line-segment 'square )))

(setparam vertex-count 3)
(setparam angles '(120 30 30))

(ok (fire-rule 'triangle 'obtuse-triangle))
(ok (not (fire-rule 'right-triangle 'acute-triangle)))
(ok (not (fire-rule 'point 'line-segment 'square )))

(setparam vertex-count 4)
(setparam angles '(90 90 90 90))

(ok (fire-rule 'square))
(ok (not (fire-rule 'point 'line-segment 'triangle )))


(finalize)
