
(in-package :cl-user)
(defpackage cl-rules.serialization
  (:use :cl :cl-yaml)
  (:import-from :cl-rules.core
                :cond-args
                :cond-name
                :cond-reg-p
                :make-cond
                :make-rule
                :param-reg-p
                :register-rule
                :rule-by-name
                :rule-conditions
                :rule-name
                :rule-reg-p)
  (:import-from :alexandria
                :define-constant)
  (:export :loads
           :save-to-str
           :save-to-file))
(in-package :cl-rules.serialization)


(define-constant +root-key+ "rules" :test 'equalp)

(defun is-param-p (item)
  "Строка представляет собой
  указание на параметр?"
  (and (stringp item)
       (> (length item) 4)
       (equal (subseq item 0 2) "{{")
       (equal (subseq (reverse item) 0 2) "}}")))

(defun unserialize-param (param-str)
  (flet ((param-name (str)
           (string-right-trim
            "}}"
            (string-left-trim "{{" str))))
    (let ((name (param-name param-str)))
      (unless (param-reg-p name)
        (error (format nil "Parameter with name '~a' not registered!" name)))
      (intern (string-upcase name)))))

(defun unserialize-cond (condition)
  (let ((cond-name (first condition)))
    (unless (cond-reg-p cond-name)
      (error (format nil "Condition with name '~a' not registered!" cond-name)))
    (make-cond
     cond-name
     (map 'list
          (lambda (x)
            (if (is-param-p x)
                (unserialize-param x)
                x))
          (rest condition)))))

(defun unserialize-rule (name rules)
  (when (rule-reg-p name)
    (error (format nil "Rule with name '~a' already registered!" name)))
  (make-rule
   name
   (map 'list
        #'unserialize-cond
        (gethash name rules))))

(defun loads (str-or-path)
  (let ((result '())
        (data (gethash +root-key+
                       (cl-yaml:parse str-or-path))))
    (maphash (lambda (name val)
               (declare (ignore val))
               (setf result
                     (cons
                      (unserialize-rule name
                                        data)
                      result)))
             data)
    (let ((names (map 'list 'rule-name result)))
      ;; регистрируем все загруженные правила
      (map 'list 'register-rule result)
      names)))

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
                 (gethash +root-key+ ruleset))
        (map 'list
             #'serialize-cond
             (rule-conditions rule)))
  ruleset)

(defun save-to-str (&rest rule-names)
  (when (listp (first rule-names))
    (setf rule-names (first rule-names)))
  (let ((ruleset (make-hash-table :test 'equalp)))
    (setf (gethash +root-key+ ruleset)
          (make-hash-table :test 'equalp))
    (dolist (name rule-names)
      (serialize-rule (rule-by-name name) ruleset))
    (cl-yaml:emit-to-string ruleset)))

(defun save-to-file (path &rest rule-names)
  (when (listp (first rule-names))
    (setf rule-names (first rule-names)))
  (let ((str-ruleset (save-to-str rule-names)))
    (with-open-file (stream path :direction :output)
      (format stream str-ruleset))))
