
(in-package :cl-user)
(defpackage cl-rules.serialization
  (:use :cl :cl-yaml)
  (:import-from :cl-rules.core
                :action-args
                :action-name
                :action-reg-p
                :cond-args
                :cond-name
                :cond-reg-p
                :make-action
                :make-cond
                :make-rule
                :param-reg-p
                :register-rule
                :rule-by-name
                :rule-actions
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
  "Does the string contain a parameter?
  Example: '{{param-name}}'"

  (and (stringp item)
       (> (length item) 4)
       (equal (subseq item 0 2) "{{")
       (equal (subseq (reverse item) 0 2) "}}")))

(defun unserialize-param (raw-param)
  (flet ((param-name (str)
           (string-right-trim
            "}}"
            (string-left-trim "{{" str))))
    (let ((name (param-name raw-param)))
      (unless (param-reg-p name)
        (error (format nil "Parameter with name '~a' not registered!" name)))
      (intern (string-upcase name)))))

(defun unserialize-cond (raw-condition)
  (let ((cond-name (first raw-condition)))
    (unless (cond-reg-p cond-name)
      (error (format nil "Condition with name '~a' not registered!" cond-name)))
    (make-cond
     cond-name
     (map 'list
          (lambda (x)
            (if (is-param-p x)
                (unserialize-param x)
                x))
          (rest raw-condition)))))

(defun unserialize-action (raw-action)
  (let ((action-name (first raw-action)))
    (unless (action-reg-p action-name)
      (error (format nil "Action with name '~a' not registered!" action-name)))
    (make-action
     action-name
     (map 'list
          (lambda (x)
            (if (is-param-p x)
                (unserialize-param x)
                x))
          (rest raw-action)))))

(defun unserialize-rule (name rules)
  (when (rule-reg-p name)
    (error (format nil "Rule with name '~a' already registered!" name)))
  (let ((raw-rule (gethash name rules)))
    (if (listp raw-rule)
        (make-rule
         name
         (map 'list #'unserialize-cond raw-rule))
        (make-rule
         name
         (map 'list
              #'unserialize-cond
              (gethash "conditions" raw-rule))
         (map 'list
              #'unserialize-action
              (gethash "actions" raw-rule))))))

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

(defun serialize-action (action)
  (cons
   (string-downcase (action-name action))
   (map 'list
        (lambda (x)
          (if (and (symbolp x) (param-reg-p x))
              (serialize-param x)
              (eval x)))
        (action-args action))))

(defun serialize-rule (rule ruleset)
  (let ((actions (map 'list #'serialize-action (rule-actions rule)))
        (conditions (map 'list #'serialize-cond (rule-conditions rule)))
        (name (string-downcase (rule-name rule))))
    (if (null actions)
        (setf (gethash name (gethash +root-key+ ruleset))
              conditions)
        (progn
          (setf (gethash name (gethash +root-key+ ruleset))
                (make-hash-table :test 'equalp))
          (setf (gethash "conditions" (gethash name (gethash +root-key+ ruleset)))
                conditions)
          (setf (gethash "actions" (gethash name (gethash +root-key+ ruleset)))
                actions)))
    ruleset))

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
