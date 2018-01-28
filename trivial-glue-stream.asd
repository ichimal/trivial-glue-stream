(in-package :cl-user)

(defpackage :trivial-glue-stream-asd
  (:use :cl :asdf) )

(in-package :trivial-glue-stream-asd)

(defsystem trivial-glue-stream
  :name "trivial-glue-stream"
  :version "0.1"
  :maintainer "SUZUKI Shingo"
  :author "SUZUKI Shingo"
  :licence "MIT"
  :description
    "glue-stream, a gray-stream specific two-way-stream implementation"
  :serial nil
  :depends-on (:trivial-gray-streams)
  :components
    ((:file #:glue-stream)) )

(defmethod perform ((op test-op)
                    (component (eql (find-system :trivial-glue-stream))) )
  (declare (ignore op component))
  (operate 'load-op :trivial-glue-stream-test)
  (operate 'test-op :trivial-glue-stream-test :force t) )

