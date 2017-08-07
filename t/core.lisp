(in-package :cl-user)
(defpackage cl-rules-test.core
  (:use :cl :prove)
  (:import-from :cl-rules.core
                :cond-args
                :cond-name
                :cond-reg-p
                :defcond
                :defparam
                :defrule
                :fire-rule
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
   ))
(in-package :cl-rules-test.core)

(plan 24)


(defcond test-cond ()
  t)

;; Not possible to redefine condition
(is-error (defcond test-cond () t) 'error)


;; cond-name always return string in upper case
(is "TEST-COND" (cond-name (make-cond 'test-cond  '(1 2 3))))
(is "TEST-COND" (cond-name (make-cond "test-cond" '(1 2 3))))
(is "TEST-COND" (cond-name (make-cond "tEst-cOnd" '(1 2 3))))
(is "TEST-COND" (cond-name (make-cond "TEST-COND" '(1 2 3))))

(ok (equal (cond-name (make-cond 'test-cond '(1 2 3)))
           (cond-name (make-cond "test-cond" '(1 2 3)))))

(ok (equal (cond-name (make-cond "test-cond" '(1 2 3)))
           (cond-name (make-cond "TEST-COND" '(1 2 3)))))

;; empty arguments list is correct
(is 0 (length (cond-args (make-cond 'test-cond '()))))

;; equivalent names
(ok (cond-reg-p 'test-cond))
(ok (cond-reg-p "test-cond"))
(ok (cond-reg-p "tEst-cOnd"))
(ok (cond-reg-p "TEST-COND"))


(defparam mister-x t)

(defcond always-true ()
  t)

(defcond always-false ()
  nil)

(defcond uncertain (value)
  value)

(defrule rule1
  (always-true)
  (uncertain mister-x))

(defrule rule2
  (always-false)
  (uncertain mister-x))


(ok (fire-rule '()))
(ok (fire-rule 'rule1))
(ok (fire-rule 'rule1 "rule1" "RULE1" "RuLe1"))

;; error if rule not exists
(is-error (fire-rule 'unknown) 'error)

;; rule-name always return string in upper case
(is "TEST-RULE" (rule-name (make-rule 'test-rule  '())))
(is "TEST-RULE" (rule-name (make-rule "test-rule" '())))
(is "TEST-RULE" (rule-name (make-rule "tEst-rule" '())))
(is "TEST-RULE" (rule-name (make-rule "TEST-rule" '())))

;; empty conditions list is correct
(is 0 (length (rule-conditions (make-rule 'test-rule '()))))

(ok (param-reg-p 'mister-x))
(ok (not (param-reg-p 'james-bond)))

(setparam mister-x nil)

(is nil (param-val 'mister-x))


(finalize)
