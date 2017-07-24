
(in-package :cl-user)
(defpackage cl-rules.serialization
  (:use :cl)
  (:import-from :cl-yaml
                :parse
                :emit)
  (:import-from :cl-rules.core
                :cond-args
                :cond-name
                :make-cond
                :make-rule
                :param-reg-p
                :rule-by-name
                :rule-conditions
                :rule-name)
  (:export :unserialize
           :save-to-file))
(in-package :cl-rules.serialization)


(defun is-param-p (item)
  "Строка представляет собой
  указание на параметр?"
  (and (stringp item)
       (> (length item) 4)
       (equal (subseq item 0 2) "{{")
       (equal (subseq (reverse item) 0 2) "}}")))

(defun unserialize-param (param-str)
  (intern
   (string-upcase
    (string-right-trim
     "}}"
     (string-left-trim "{{" param-str)))))

(defun unserialize-cond (condition)
  (make-cond
   (first condition)
   (map 'list
        (lambda (x)
          (if (is-param-p x)
              (unserialize-param x)
              x))
        (rest condition))))

(defun unserialize-rule (name rules)
  (make-rule
   name
   (map 'list
        #'unserialize-cond
        (gethash name rules))))

(defun unserialize (str-or-path)
  (let ((result '())
        (data (gethash "rules" (parse str-or-path))))
    (maphash (lambda (name val)
               (declare (ignore val))
               (setf result
                     (cons
                      (unserialize-rule name
                                        data)
                      result)))
             data)
    result))

(defun serialize-param (name)
  (format nil "{{~a}}" (string-downcase (symbol-name name))))

(defun serialize-cond (condition)
  (cons
   (string-downcase (cond-name condition))
   (map 'list
        (lambda (x)
          (if (and (symbolp x) (param-reg-p x))
              (serialize-param x)
              (eval x)))
        (cond-args condition))))

(defun serialize-rule (rule ruleset)
  (setf (gethash (string-downcase (rule-name rule))
                 (gethash "rules" ruleset))
        (map 'list
             #'serialize-cond
             (rule-conditions rule)))
  ruleset)

(defun save-to-file (path &rest rule-names)
  (when (listp (first rule-names))
    (setf rule-names (first rule-names)))
  (let ((ruleset (make-hash-table :test 'equalp)))
    (setf (gethash "rules" ruleset)
          (make-hash-table :test 'equalp))
    (with-open-file (stream path :direction :output)
      (dolist (name rule-names)
        (serialize-rule (rule-by-name name) ruleset))
      (emit ruleset stream))))
