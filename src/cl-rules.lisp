(in-package :cl-user)
(defpackage cl-rules
  (:use :cl)
  (:import-from :cl-rules.core
                :setparam
                :defparam
                :defcond
                :defrule
                :fire-rule)
  (:import-from :cl-rules.serialization
                :unserialize
                :save-to-file)
  (:export :setparam
           :defparam
           :defcond
           :defrule
           :fire-rule
           :unserialize
           :save-to-file))
(in-package :cl-rules)
