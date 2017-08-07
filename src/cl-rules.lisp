(in-package :cl-user)
(defpackage cl-rules
  (:use :cl)
  (:import-from :cl-rules.core
                :cond-reg-p
                :defcond
                :defparam
                :defrule
                :fire-rule
                :param-reg-p
                :param-val
                :rule-reg-p
                :setparam
                :with-rules)
  (:import-from :cl-rules.serialization
                :loads
                :save-to-file
                :save-to-str)
  (:export :cond-reg-p
           :defcond
           :defparam
           :defrule
           :fire-rule
           :param-reg-p
           :param-val
           :rule-reg-p
           :setparam
           :with-rules
           :loads
           :save-to-file
           :save-to-str))
(in-package :cl-rules)
