(defsystem "fluent"
  :version "0.0.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage ""
  :depends-on (:parcom)
  :serial t
  :components ((:module "src" :components ((:file "package"))))
  :description "Software localisation via Mozilla's Project Fluent.")
