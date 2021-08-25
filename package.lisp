
(in-package :cl-user)

(defpackage :vroute
  (:use :cl :anaphora :cl-ppcre)
  (:export :role :endpoint :agent-for-endpoint :respond :authorized-p :header
           ;; useful error responses
           :forbidden
           ))

