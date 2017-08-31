(in-package :cl-user)
(defpackage cl-rules.core
  (:use :cl)
  (:import-from :alexandria
                :hash-table-keys)
  (:export :bind-commands
           :cond-args
           :cond-name
           :cond-reg-p
           :defcond
           :defparam
           :defrule
           :eval-rule
           :fire-rule
           :make-cond
           :make-rule
           :param-reg-p
           :param-val
           :register-rule
           :rule-by-name
           :rule-commands
           :rule-conditions
           :rule-name
           :rule-reg-p
           :setparam
           :with-rules))
(in-package :cl-rules.core)


(defvar *parameters* (make-hash-table :test 'equal))
(defvar *commands*   (make-hash-table :test 'equal))
(defvar *conditions* (make-hash-table :test 'equal))
(defvar *rules*      (make-hash-table :test 'equal))


;;; Описание параметров


(defun param-val (name &optional (bad-val nil))
  "Возвращает значение параметра, если таковой существует.
  Если параметр с именем NAME отсутсвует, то возвращается
  BAD-VAL. NAME может быть символом или строкой"
  (if (symbolp name)
      (gethash (symbol-name name) *parameters* bad-val)
      (gethash (string-upcase name) *parameters* bad-val)))

(defun param-reg-p (name)
  "Параметр с именем NAME зарегистрирован?"
  (let ((uniq (gensym)))
    (not (equal (param-val name uniq) uniq))))

(defmacro setparam (name form)
  "Устанавливает новое значение параметра"
  `(when ,(param-reg-p name)
     (setf (gethash ,(symbol-name name) *parameters*)
           ,form)))

(defmacro defparam (name form)
  "Собственно объявление нового параметра."
  (if (param-reg-p name)
      (error (format nil "Parameter with name '~a' already exists!" name))
      `(setf (gethash ,(symbol-name name) *parameters*)
             ,form)))


;;; Описание команд


(defun command-by-name (name)
  "Return command by specified name.
   Name must be a symbol or string"
  (if (symbolp name)
      (gethash (symbol-name name) *commands*)
      (gethash (string-upcase name) *commands*)))

(defun command-reg-p (name)
  "Command with NAME is registered?"
  (not (null (command-by-name name))))

(defun eval-command (&rest command-names)
  (when (listp (first command-names))
    (setf command-names (first command-names)))
  (dolist (name command-names)
    (let ((command (command-by-name name)))
      (if (functionp command)
          (funcall command)
          (error (format nil "Command with name '~a' does not exists!" name))))))

(defmacro defcommand (name &body forms)
  (if (command-reg-p name)
      (error (format nil "Command with name '~a' already exists!" name))
      `(setf (gethash ,(symbol-name name) *commands*)
             (lambda () ,@forms))))


;;; Описание условий


(defun make-cond (name args)
  "Создает условие из его имени и списка аргументов ARGS.
  NAME может быть символом или строкой"
    (if (symbolp name)
        (list (symbol-name name) args)
        (list (string-upcase name) args)))

(defun cond-name (condition)
  "Вернет имя условия в виде строки"
  (first condition))

(defun cond-args (condition)
  "Вернет список из аргументов условия"
  (second condition))

(defun cond-eval-params (condition)
  "Вычисляет значения всех параметров,
  указанных в списке аргументов условия"
  (make-cond
   (cond-name condition)
   (map 'list
        (lambda (x)
          (if (and (symbolp x) (param-reg-p x))
              (param-val x)
              x))
        (cond-args condition))))

(defun cond-by-name (name)
  "Возвращает условие по переданному имени.
  NAME может быть символом или строкой"
  (if (symbolp name)
      (gethash (symbol-name name) *conditions*)
      (gethash (string-upcase name) *conditions*)))

(defun cond-reg-p (name)
  "Условие с именем NAME зарегистрировано?"
  (not (null (cond-by-name name))))

(defmacro defcond (name args &body forms)
  "Объявляет новое условие с именем NAME.
  Представляет собой произвольного вида предикат"
  (if (cond-reg-p name)
      (error (format nil "Condition with name '~a' already exists!" name))
      `(setf (gethash ,(symbol-name name) *conditions*)
             (lambda ,args ,@forms))))


;;; Описание правил


(defun make-rule (name conditions &optional commands)
  "Создает новое правило с именем NAME и
  списком условий CONDITIONS"
  (if (symbolp name)
      (list (symbol-name name) conditions (when commands (symbol-name commands)))
      (list (string-upcase name) conditions (when commands (symbol-name commands)))))

(defun rule-name (rule)
  "Вернет имя правила в виде строки"
  (first rule))

(defun rule-conditions (rule)
  "Список из условий, входящих в правило. Условия
  хранятся вместе с зафиксированными аргументами"
  (second rule))

(defun rule-commands (rule)
  "List of command names. If rule is true,
   then commands evaluates"
  (third rule))


(defun rule-by-name (name)
  "Вернет правило по имени NAME.
  NAME может быть символом или строкой."
  (if (symbolp name)
      (gethash (symbol-name name) *rules*)
      (gethash (string-upcase name) *rules*)))

(defun rule-reg-p (name)
  "Правило с именем NAME зарегистрировано?"
  (not (null (rule-by-name name))))

(defun register-rule (rule)
  "Регистрирует новое правило"
  (setf (gethash (rule-name rule) *rules*) rule))

(defmacro defrule (name &body forms)
  "Объявляет новое правило с именем NAME"
  (let ((commands (when (eq (first forms) :commands)
                    (progn
                      (setf forms (cddr forms))
                      (second forms))))
        (conditions (map 'list
                         (lambda (x)
                           (make-cond (first x) (rest x)))
                         forms)))
    (if (rule-reg-p name)
        (error (format nil "Rule with name '~a' already exists!" name))
        `(setf (gethash ,(symbol-name name) *rules*)
               ',(make-rule name conditions commands)))))

(defmacro with-rules (sym &body forms)
  "Позволяет пройти по именам всех
  зарегистрированных правил"
  `(dolist (,sym (hash-table-keys *rules*))
     ,@forms))

(defun bind-commands (&rest commands)
  "Verify command names and puts them in list"
  (if (every #'command-reg-p commands)
      commands
      (error (format nil "Command with speccified name does not exists!"))))

(defun fire-condition (condition)
  "Выполняет предикат-условие. Вернет истину или ложь."
  (let ((predicate (cond-by-name (cond-name condition))))
    (apply predicate (cond-args (cond-eval-params condition)))))

(defun rule-value (name)
  "Rule is true?"
  (let ((rule (rule-by-name name)))
    (if rule
        (every #'fire-condition (rule-conditions rule))
        (error (format nil "Rule with name '~a' does not exists!" name)))))

(defun fire-rule (&rest rule-names)
  "Выполняет правила. Вернет истину, если все правила истинны.
  Правило истинно, если все его условия истинны. Связанные с
  правилом команды не выполняются"
  (when (listp (first rule-names))
    (setf rule-names (first rule-names)))
  (every #'rule-value rule-names))

(defun eval-rule (&rest rule-names)
  "Is similar fire-rule, but evaluate commands"
  (when (listp (first rule-names))
    (setf rule-names (first rule-names)))
  (every
   (lambda (x)
     (when (rule-value x)
       (eval-command (rule-commands (rule-by-name x)))
       t))
   rule-names))
