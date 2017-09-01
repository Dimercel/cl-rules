(in-package :cl-user)
(defpackage cl-rules-test.serialization
  (:use :cl :prove)
  (:import-from :cl-rules
                :defcommand
                :defparam
                :defcond
                :fire-rule
                :eval-rule
                :setparam)
  (:import-from :cl-rules.serialization
                :loads
                :save-to-str
                :save-to-file))
(in-package :cl-rules-test.serialization)

(plan 10)


(defparam ram 1024)
(defparam cpu 1)
(defparam disc 30)
(defparam traffic 2)

(defvar *balance* 1000)


(defcond ram (param index)
  (= param index))

(defcond cpu (param index)
  (= param index))

(defcond disc (param index)
  (= param index))

(defcond traffic (param index)
  (= param index))

(defcommand pay
  (when (fire-rule 'mini-tariff) (decf *balance* 100))
  (when (fire-rule 'base-tariff) (decf *balance* 200)))


(loads (merge-pathnames "data/tariffs.yml" *load-truename*))

;; Impossible second loads
(is-error (loads (merge-pathnames "data/tariffs.yml" *load-truename*)) 'error)

(ok (fire-rule 'base-tariff))
(ok (not (fire-rule 'mini-tariff)))

(ok (not (eval-rule 'mini-tariff 'base-tariff)))
(is *balance* 800)


(setparam ram 2048)
(setparam cpu 2)
(setparam disc 40)
(setparam traffic 3)

;; Correct loading from string
(loads
"rules:
   extra-tariff:
   - [ram, \"{{ram}}\", 2048]
   - [cpu, \"{{cpu}}\", 2]
   - [disc, \"{{disc}}\", 40]
   - [traffic, \"{{traffic}}\", 3]")

(ok (fire-rule 'extra-tariff))
(ok (not (fire-rule 'base-tariff)))
(ok (not (fire-rule 'mini-tariff)))


;; Not correct loading. Not registered condition
(is-error
 (loads
  "rules:
   mega-tariff:
   - [ram, \"{{ram}}\", 2048]
   - [cpu, \"{{cpu}}\", 2]
   - [disc-space, \"{{disc}}\", 40]
   - [traffic, \"{{traffic}}\", 3]")
 'error)

;; Not correct loading. Not registered parameter
(is-error
 (loads
  "rules:
   mega-tariff:
   - [ram, \"{{ram}}\", 2048]
   - [cpu, \"{{core-count}}\", 2]
   - [disc, \"{{disc}}\", 40]
   - [traffic, \"{{traffic}}\", 3]")
 'error)


(finalize)
