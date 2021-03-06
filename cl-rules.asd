#|
  This file is a part of cl-rules project.
  Copyright (c) 2019 Ito Dimercel (xolcman@gmail.com)
|#

#|
  Author: Ito Dimercel (xolcman@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-rules-asd
  (:use :cl :asdf))
(in-package :cl-rules-asd)

(defsystem cl-rules
  :version "0.1"
  :author "Ito Dimercel"
  :license "GPL-3.0"
  :depends-on (:cl-yaml :alexandria)
  :components ((:module "src"
                :components
                ((:file "core")
                 (:file "serialization" :depends-on ("core"))
                 (:file "cl-rules" :depends-on ("core" "serialization")))))
  :description "Simple DSL for rules that can be configured without code"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-rules-test))))
