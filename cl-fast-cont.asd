#|
  This file is a part of cl-fast-cont project.
  Copyright (c) 2016 κeen
|#

#|
  Author: κeen
|#

(in-package :cl-user)
(defpackage cl-fast-cont-asd
  (:use :cl :asdf))
(in-package :cl-fast-cont-asd)

(defsystem cl-fast-cont
  :version "0.1"
  :author "κeen"
  :license "BSD"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "cl-fast-cont"))))
  :description ""
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
  :in-order-to ((test-op (test-op cl-fast-cont-test))))
