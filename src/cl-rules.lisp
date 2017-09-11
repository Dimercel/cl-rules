(in-package :cl-user)
(defpackage cl-rules
  (:use :cl)
  (:import-from :cl-rules.core
                :action-reg-p
                :cond-reg-p
                :defaction
                :defcond
                :defparam
                :defrule
                :eval-rule
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
  (:export :action-reg-p
           :cond-reg-p
           :defaction
           :defcond
           :defparam
           :defrule
           :eval-rule
           :fire-rule
           :loads
           :param-reg-p
           :param-val
           :rule-reg-p
           :setparam
           :save-to-file
           :save-to-str
           :with-rules))
(in-package :cl-rules)
