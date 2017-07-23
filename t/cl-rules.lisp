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


(plan 4)

(defparam *param1* 100.0)
(defparam *param2* -100.0)

(defcond more-than (arg1 arg2)
  (> arg1 arg2))

(defcond less-than (arg1 arg2)
  (< arg1 arg2))

(defrule param1-less-param2
    (less-than *param1* *param2*))

(defrule less-100
    (less-than *param1* 100.0)
  (less-than *param2* 100.0))

(ok (not (fire-rule 'param1-less-param2)))
(ok (not (fire-rule 'less-100)))
(ok (not (fire-rule '(param1-less-param2 less-100))))


(setparam *param1* 99)

(ok (fire-rule 'less-100))


(finalize)
