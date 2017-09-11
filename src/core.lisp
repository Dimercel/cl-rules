(in-package :cl-user)
(defpackage cl-rules.core
  (:use :cl)
  (:import-from :alexandria
                :hash-table-keys)
  (:export :action-reg-p
           :action-name
           :action-args
           :cond-args
           :cond-name
           :cond-reg-p
           :defaction
           :defcond
           :defparam
           :defrule
           :eval-rule
           :fire-rule
           :make-action
           :make-cond
           :make-rule
           :param-reg-p
           :param-val
           :register-rule
           :rule-by-name
           :rule-conditions
           :rule-name
           :rule-reg-p
           :setparam
           :unregister-rule
           :with-rules))
(in-package :cl-rules.core)


(defvar *parameters* (make-hash-table :test 'equal))
(defvar *actions*    (make-hash-table :test 'equal))
(defvar *conditions* (make-hash-table :test 'equal))
(defvar *rules*      (make-hash-table :test 'equal))


;;; Description of parameters


(defun param-val (name &optional (bad-val nil))
  "Will return value of parameter, if he is exists.
  If parameter with NAME not exists, then return
  BAD-VAL. NAME is symbol or string"
  (if (symbolp name)
      (gethash (symbol-name name) *parameters* bad-val)
      (gethash (string-upcase name) *parameters* bad-val)))

(defun param-reg-p (name)
  "Parameter with NAME is registered?"
  (let ((uniq (gensym)))
    (not (equal (param-val name uniq) uniq))))

(defmacro setparam (name form)
  "Set a new value for parameter"
  `(when ,(param-reg-p name)
     (setf (gethash ,(symbol-name name) *parameters*)
           ,form)))

(defmacro defparam (name form)
  "Defines a new parameter"
  (if (param-reg-p name)
      (error (format nil "Parameter with name '~a' already exists!" name))
      `(setf (gethash ,(symbol-name name) *parameters*)
             ,form)))


;;; Description of actions


(defun make-action (name args)
  "Action consists of name and arguments for the function.
  ARGS - list of arguments, which will be link with function"
  (if (symbolp name)
      (list (symbol-name name) args)
      (list (string-upcase name) args)))

(defun action-name (action)
  (first action))

(defun action-args (action)
  (second action))

(defun action-by-name (name)
  "Return action by specified name.
   Name must be a symbol or string"
  (if (symbolp name)
      (gethash (symbol-name name) *actions*)
      (gethash (string-upcase name) *actions*)))

(defun action-reg-p (name)
  "Action with NAME is registered?"
  (not (null (action-by-name name))))

(defun eval-action (action)
  "Each action linked with function. Function
  called with arguments of action"
  (let ((name (action-name action)))
    (if (action-reg-p name)
        (apply (gethash name *actions*) (action-args action))
        (error (format nil "Action with name '~a' does not exists!" name)))))

(defmacro defaction (name args &body forms)
  (if (action-reg-p name)
      (error (format nil "Action with name '~a' already exists!" name))
      `(setf (gethash ,(symbol-name name) *actions*)
             (lambda ,args ,@forms))))


;;; Description of conditions


(defun make-cond (name args)
  "Create condition from itself name and list
  of arguments ARGS. NAME is symbol or string"
    (if (symbolp name)
        (list (symbol-name name) args)
        (list (string-upcase name) args)))

(defun cond-name (condition)
  "Will return name of condition as string"
  (first condition))

(defun cond-args (condition)
  "Return list contains condition arguments"
  (second condition))

(defun cond-eval-params (condition)
  "Evaluate values of all parameters, which
  specified in list of condition arguments"
  (make-cond
   (cond-name condition)
   (map 'list
        (lambda (x)
          (if (and (symbolp x) (param-reg-p x))
              (param-val x)
              x))
        (cond-args condition))))

(defun cond-by-name (name)
  "Will return condiiton by specified name.
  NAME is symbol or string"
  (if (symbolp name)
      (gethash (symbol-name name) *conditions*)
      (gethash (string-upcase name) *conditions*)))

(defun cond-reg-p (name)
  "Condition with NAME id registered?"
  (not (null (cond-by-name name))))

(defmacro defcond (name args &body forms)
  "Define new condition with NAME.
  Represents arbitrary predicate"
  (if (cond-reg-p name)
      (error (format nil "Condition with name '~a' already exists!" name))
      `(setf (gethash ,(symbol-name name) *conditions*)
             (lambda ,args ,@forms))))


;;; Description of rules


(defun make-rule (name conditions &optional actions)
  "Create new rule with NAME and list of conditions.
  Rule is true, only if all conditions is true"
  (if (symbolp name)
      (list (symbol-name name) conditions actions)
      (list (string-upcase name) conditions actions)))

(defun rule-name (rule)
  (first rule))

(defun rule-conditions (rule)
  "Condition stored with static values of arguments"
  (second rule))

(defun rule-actions (rule)
  "List of actions, linked with rule. If value of
  rule is true, then actions evaluates"
  (third rule))


(defun rule-by-name (name)
  "NAME is symbol or string"
  (if (symbolp name)
      (gethash (symbol-name name) *rules*)
      (gethash (string-upcase name) *rules*)))

(defun rule-reg-p (name)
  "Rule with NAME is registered?"
  (not (null (rule-by-name name))))

(defun register-rule (rule)
  "Registers new rule. Names of rules must be unique"
  (setf (gethash (rule-name rule) *rules*) rule))

(defun unregister-rule (name-of-rule)
  (let ((rule (rule-by-name name-of-rule)))
    (when rule
      (remhash (rule-name rule) *rules*))))


(defun actions-specified-p (rule-args)
  (when (> (length rule-args) 2)
    (eq (first rule-args) :actions)))

(defmacro defrule (name &body forms)
  (let ((actions (when (actions-specified-p forms)
                   (let ((act-list (second forms)))
                     (setf forms (cddr forms))
                     (map 'list
                          (lambda (act)
                            (make-action (first act) (rest act)))
                          act-list))))
        (conditions (map 'list
                         (lambda (x)
                           (make-cond (first x) (rest x)))
                         forms)))
    (if (rule-reg-p name)
        (error (format nil "Rule with name '~a' already exists!" name))
        `(setf (gethash ,(symbol-name name) *rules*)
               ',(make-rule name conditions actions)))))

(defmacro with-rules (sym &body forms)
  "Allow traversing by all registered rules.
  In SYM stored name of rule"
  `(dolist (,sym (hash-table-keys *rules*))
     ,@forms))

(defun fire-condition (condition)
  "Evaluate predicate. Will return true or false"
  (let ((predicate (cond-by-name (cond-name condition))))
    (apply predicate (cond-args (cond-eval-params condition)))))

(defun rule-value (name)
  "Rule is true?"
  (let ((rule (rule-by-name name)))
    (if rule
        (every #'fire-condition (rule-conditions rule))
        (error (format nil "Rule with name '~a' does not exists!" name)))))

(defun fire-rule (&rest rule-names)
  "Will return true if all conditions is true.
  Linked actions not execute."
  (when (listp (first rule-names))
    (setf rule-names (first rule-names)))
  (every #'rule-value rule-names))

(defun eval-rule (&rest rule-names)
  "Is similar fire-rule, but evaluate actions. Actions will
  be evaluate only if the value of rule is true"
  (when (listp (first rule-names))
    (setf rule-names (first rule-names)))
  (let ((result t))
    (dolist (name rule-names)
      (if (rule-value name)
          (map 'list
               #'eval-action
               (rule-actions (rule-by-name name)))
          (setf result nil)))
    result))
