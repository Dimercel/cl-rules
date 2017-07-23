(in-package :cl-user)
(defpackage cl-rules
  (:use :cl)
  (:export :param-reg-p
           :rule-by-name
           :make-cond
           :make-rule
           :cond-name
           :cond-args
           :rule-conditions
           :rule-name
           :setparam
           :defparam
           :defcond
           :defrule
           :fire-rule))
(in-package :cl-rules)


(defvar *parameters* (make-hash-table :test 'equal))
(defvar *conditions* (make-hash-table :test 'equal))
(defvar *rules* (make-hash-table :test 'equal))


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
  `(setf (gethash ,(symbol-name name) *parameters*)
         ,form))


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
  `(setf (gethash ,(symbol-name name) *conditions*)
         (lambda ,args ,@forms)))


;;; Описание правил


(defun make-rule (name conditions)
  "Создает новое правило с именем NAME и
  списком условий CONDITIONS"
  (if (symbolp name)
      (list (symbol-name name) conditions)
      (list (string-upcase name) conditions)))

(defun rule-name (rule)
  "Вернет имя правила в виде строки"
  (first rule))

(defun rule-conditions (rule)
  "Список из условий, входящих в правило. Условия
  хранятся вместе с зафиксированными аргументами"
  (second rule))

(defun rule-valid-p (rule)
  "Проверяет корректность построенного правила."
  (every
   ;; Все условия, составляющие правило должны
   ;; быть зарегистрированными
   (lambda (x) (cond-reg-p (first x)))
   (rule-conditions rule)))

(defun rule-by-name (name)
  "Вернет правило по имени NAME.
  NAME может быть символом или строкой."
  (if (symbolp name)
      (gethash (symbol-name name) *rules*)
      (gethash (string-upcase name) *rules*)))

(defun rule-reg-p (name)
  "Правило с именем NAME зарегистрировано?"
  (not (null (rule-by-name name))))

(defun rule-register (rule)
  "Регистрирует новое правило"
  (gethash (rule-name rule) rule))

(defmacro defrule (name &body forms)
  "Объявляет новое правило с именем NAME"
  (let ((conditions (map 'list
                         (lambda (x)
                           (make-cond (first x)
                                      (rest x)))
                         forms)))
    `(setf (gethash ,(symbol-name name) *rules*)
           ',(make-rule name conditions))))

(defun fire-condition (condition)
  "Выполняет предикат-условие. Вернет истину или ложь."
  (let ((predicate (cond-by-name (cond-name condition))))
    (apply predicate (cond-args (cond-eval-params condition)))))

(defun fire-rule (&rest rule-names)
  "Выполняет правила. Вернет истину, если все
  правила истинны. Правило истинно, если все
  его условия истинны."
  (when (listp (first rule-names))
    (setf rule-names (first rule-names)))
  (every
   (lambda (x)
     (let ((rule (if (symbolp x)
                     (gethash (symbol-name x) *rules*)
                     (gethash (string-upcase x) *rules*))))
       (every
        #'fire-condition
        (rule-conditions rule))))
   rule-names))
