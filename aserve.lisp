
(in-package :vroute)

#+aserve
(defmethod find-endpoint ((request net.aserve:http-request) prefix &key endpoint-superclass)
  (find-endpoint-for-uri (format nil "~A" (net.aserve:request-raw-uri request))
                         prefix
                         :endpoint-superclass endpoint-superclass))

#+aserve
(defmethod initialize-for-request ((endpoint standard-class) (request net.aserve:http-request)
                                   &key extra prefix)
  (make-instance endpoint
                 :request request
                 :extra extra
                 :prefix prefix
                 :uri (subseq (format nil "~A" (net.aserve:request-raw-uri request))
                              (length prefix))
                 :method (net.aserve:request-method request)
                 :param-function (lambda (name)
                                   (net.aserve:request-query-value name request))
                 :header-function (lambda (name)
                                    (net.aserve:header-slot-value request name))
                 :body-function (lambda ()
                                  (net.aserve:get-request-body request))))

#+aserve
(defun publish-dispatching-prefix (prefix &key (endpoint-superclass (find-class 'endpoint)))
  (net.aserve:publish-prefix
   :prefix prefix
   :function (lambda (req ent)
               (handler-case
                   (let ((endpoint (find-endpoint req prefix :endpoint-superclass endpoint-superclass)))
                     (if endpoint
                         (awhen (initialize-for-request endpoint req :extra ent :prefix prefix)
                           (handle-request it))
                         (send-response-for-request (make-instance 'endpoint
                                                                   :uri (concatenate 'string prefix "???")
                                                                   :request req
                                                                   :extra ent)
                                                    req
                                                    (make-instance 'not-found-response))))
                 (error (c)
                   (break "~A" c))))))


;; (publish-dispatching-prefix "/api/")

#+aserve
(defmethod send-response-for-request ((e endpoint) (req net.aserve:http-request) (response response))
  (let ((ent (endpoint-extra e)))
    (net.aserve:with-http-response (req ent
                                        :content-type (content-type response)
                                        :response (or (find (code response)
                                                            net.aserve::*responses*
                                                            :key #'net.aserve::response-number)
                                                      (net.aserve::make-resp (code response)
                                                                             (regex-replace-all "-"
                                                                                                (symbol-name
                                                                                                 (class-name
                                                                                                  (class-of response)))
                                                                                                " "))))
                                   
      ;; !!! TODO - handle response headers
      (net.aserve:with-http-body (req ent)
        (write-response-body-to-stream response
                                       (net.aserve:request-reply-stream req))))))


