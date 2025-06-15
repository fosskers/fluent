(defsystem "fluent"
  :version "0.0.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage "https://github.com/fosskers/fluent"
  :depends-on (:parcom :plurals)
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "types")
                             (:file "resolution")
                             (:file "parser"))))
  :description "Software localisation via Mozilla's Project Fluent."
  :in-order-to ((test-op (test-op :fluent/tests))))

(defsystem "fluent/tests"
  :depends-on (:fluent :parcom :parachute)
  :components ((:module "tests" :components ((:file "tests"))))
  :perform (test-op (op c) (symbol-call :parachute :test :fluent/tests)))
