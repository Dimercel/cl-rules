#|
  This file is a part of cl-rules project.
  Copyright (c) 2017 Ito Dimercel (xolcman@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-rules-test-asd
  (:use :cl :asdf))
(in-package :cl-rules-test-asd)

(defsystem cl-rules-test
  :author "Ito Dimercel"
  :license "GPL-3.0"
  :depends-on (:cl-rules
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-rules")
                 (:test-file "core"))))
  :description "Test system for cl-rules"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
