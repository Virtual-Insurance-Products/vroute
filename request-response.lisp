
(in-package :vroute)

(defclass response ()
  ((code :initarg :code :reader code :initform 200)
   (body :initarg :body :reader body)
   (content-type :initarg :content-type :initform "text/html" :reader content-type
                 :type string)
   ;; headers...
   ))

(defmethod additional-headers ((r response)) nil)

(defmethod write-response-body-to-stream ((r response) (s stream))
  (when (body r)
    (cond ((equal (content-type r) "application/json")
           (write-sequence (json:encode-json-to-string (body r)) s))
          (t (write-sequence (body r) s)))))


(defclass error-response (response)
  ((code :initform 400)))

(defclass forbidden (error-response)
  ((code :initform 403)
   (body :initform "Forbidden")))

(defclass not-found-response (error-response)
  ((code :initform 404)
   (body :initform "Not found")))

(defclass method-not-supported (error-response)
  ((code :initform 405)
   (body :initform "Method not supported")))


;; The assumption is that the endpoint hasn't implemented that method (ie there is no defmethod for it)
(defmethod respond ((e endpoint) method agent)
  (declare (ignore agent))
  (make-instance 'method-not-supported :code 405
                 ;; !!! We are required to specify what methods /are/ supported
                 ;; this will require some MOP-fu
                 :body (format nil "Method ~A not supported" method)))

;; default deny so I don't accidentally allow someone to do something
(defmethod authorized-p ((e endpoint) (method t) (agent t))
  nil)

;; then handle authorisation
(defmethod respond :around ((e endpoint) method agent)
  (let ((auth (authorized-p e method agent)))
    (cond ((typep auth 'error-response)
           auth)
          ((not auth)
           (make-instance 'error-response :code 401 :body "Unauthorized"))
          ;; otherwise authorized returns a true value
          (t (call-next-method)))))


;; This is just a simple case to make the 
(defmethod send-response-for-request ((e endpoint) req (response string))
  (send-response-for-request e req (make-instance 'response :body response)))

(defmethod handle-request ((e endpoint))
  (let ((response (handler-case
                      (respond e (endpoint-method e) (agent-for-endpoint e))
                    (error (c)
                      ;; (break)
                      ;; !!! It might be better /not/ to send the error message
                      (make-instance 'error-response
                                     :body "Unknown error occured"
                                     ;; (format nil "~A" c)
                                     )))))
    (send-response-for-request e (endpoint-request e) response)))


