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
                :register-rule
                :rule-by-name
                :rule-conditions
                :rule-name
                :rule-reg-p
                :setparam
   ))
(in-package :cl-rules-test.core)

(plan 12)

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

(finalize)
