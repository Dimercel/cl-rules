(in-package :cl-user)
(defpackage cl-rules-test.core
  (:use :cl :prove)
  (:import-from :cl-rules.core
                :action-reg-p
                :condn-args
                :condn-name
                :condn-reg-p
                :condn-val
                :defaction
                :defcondn
                :defparam
                :defrule
                :fire-rule
                :make-condn
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
   ))
(in-package :cl-rules-test.core)

(plan 33)


(defcondn test-cond ()
  t)

;; Not possible to redefine condition
(is-error (defcondn test-cond () t) 'error)


;; condn-name always return string in upper case
(is "TEST-COND" (condn-name (make-condn 'test-cond  '(1 2 3))))
(is "TEST-COND" (condn-name (make-condn "test-cond" '(1 2 3))))
(is "TEST-COND" (condn-name (make-condn "tEst-cOnd" '(1 2 3))))
(is "TEST-COND" (condn-name (make-condn "TEST-COND" '(1 2 3))))

(ok (equal (condn-name (make-condn 'test-cond '(1 2 3)))
           (condn-name (make-condn "test-cond" '(1 2 3)))))

(ok (equal (condn-name (make-condn "test-cond" '(1 2 3)))
           (condn-name (make-condn "TEST-COND" '(1 2 3)))))

;; empty arguments list is correct
(is 0 (length (condn-args (make-condn 'test-cond '()))))

;; equivalent names
(ok (condn-reg-p 'test-cond))
(ok (condn-reg-p "test-cond"))
(ok (condn-reg-p "tEst-cOnd"))
(ok (condn-reg-p "TEST-COND"))


(defaction test-action ()
  t)

(ok (action-reg-p 'test-action))
(ok (action-reg-p "test-action"))
(ok (action-reg-p "tEst-ActIOn"))
(ok (action-reg-p "TEST-ACTION"))


(defparam mister-x t)

(defcondn always-true ()
  t)

(defcondn always-false ()
  nil)

(defcondn uncertain (value)
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

;; impossible to redefine rule
(is-error (defrule rule1 (always-true)) 'error)

;; error if rule not exists
(is-error (fire-rule 'unknown) 'error)

;; rule-name always return string in upper case
(is "TEST-RULE" (rule-name (make-rule 'test-rule  '())))
(is "TEST-RULE" (rule-name (make-rule "test-rule" '())))
(is "TEST-RULE" (rule-name (make-rule "tEst-rule" '())))
(is "TEST-RULE" (rule-name (make-rule "TEST-RULE" '())))

;; empty conditions list is correct
(is 0 (length (rule-conditions (make-rule 'test-rule '()))))

(ok (param-reg-p 'mister-x))
(ok (not (param-reg-p 'james-bond)))

(setparam 'mister-x nil)

(is nil (param-val 'mister-x))

(defcondn less-seven (val)
  (< val 7))

(is t (condn-val 'less-seven '(1)))
(is nil (condn-val 'less-seven '(10)))
(is 7 (condn-val 'not-valid-name '() 7))


(unregister-rule 'rule1)
(ok (not (rule-reg-p 'rule1)))

(finalize)
