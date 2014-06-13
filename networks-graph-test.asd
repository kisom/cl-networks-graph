#|
  This file is a part of networks-graph project.
|#

(in-package :cl-user)
(defpackage networks-graph-test-asd
  (:use :cl :asdf))
(in-package :networks-graph-test-asd)

(defsystem networks-graph-test
  :author ""
  :license ""
  :depends-on (:networks-graph)
  :components ((:module "t"
                :components
                ((:file "networks-graph"))))

  ;; :perform
  ;; (test-op :after (op c)
  ;;          (funcall (intern #. (string :run-test-system) :cl-test-more)
  ;;                   c)
  ;;          (asdf:clear-system c)))
  )
