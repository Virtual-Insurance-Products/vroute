
(in-package :cl-user)

(defpackage :vroute
  (:use :cl :anaphora :cl-ppcre)
  (:export :role :endpoint :agent-for-endpoint :respond :authorized-p :header
           :body
           :prefix
           ;; non error responses
           :created
           ;; useful error responses
           :forbidden
           ))

