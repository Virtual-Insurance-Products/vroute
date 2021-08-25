(in-package :cl-user)

(asdf:defsystem #:vroute
  :description "VIP HTTP routing system. Initially built to implement some JSON API"
  :author "VIP"
  :license "vip"
  :depends-on ("cl-ppcre" "anaphora"
                          "closer-mop"
                          ;; "net.aserve" ; not a dependency, but can be used - I'd like to make this portable across HTTP servers
                          )
  
  :serial t
  :components ((:file "package")
               (:file "endpoint")
               (:file "request-response")
               (:file "aserve") ; aserve integration
               ))


