#|
  This file is a part of cl-fast-cont project.
  Copyright (c) 2016 κeen
|#

(in-package :cl-user)
(defpackage cl-fast-cont-test-asd
  (:use :cl :asdf))
(in-package :cl-fast-cont-test-asd)

(defsystem cl-fast-cont-test
  :author "κeen"
  :license "BSD"
  :depends-on (:cl-fast-cont
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-fast-cont"))))
  :description "Test system for cl-fast-cont"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
