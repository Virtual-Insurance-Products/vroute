
(in-package :vroute)

;; When a suitable endpoint class is found it is instantiated with the request details ready to handle the request
;; maybe it would make more sense to have the instantiated class be kept around and pass the request bits as
;; parameters to the handle function
;; Problem then is: all the methods which define actions to take will need lots of parameters
;; with the current scheme they only need 3: endpoint, method, agent
(defclass endpoint ()
  ((request :reader endpoint-request :initarg :request)
   (extra :reader endpoint-extra :initarg :extra)
   (uri :reader endpoint-uri :initarg :uri)
   (method :reader endpoint-method :initarg :method)
   ;; (uri-pattern)
   (param-function :initarg :param-function :reader param-function)
   (header-function :initarg :header-function :reader header-function)
   (body-function :initarg :body-function :reader body-function)))

(defun uri-components (uri)
  (let ((x (split "/" uri)))
    (if (scan "/$" uri)
        (append x (list "/"))
        x)))

;; (uri-components "")
;; (uri-components "one")
;; (uri-components "one/two")
;; (uri-components "one/two/")

(defun uri-pattern-components (uri-pattern)
  (mapcar (lambda (x)
            (if (scan "^\\$" x)
                (intern (string-upcase (subseq x 1)) :keyword)
                x))
          (uri-components uri-pattern)))

;; (uri-pattern-components "one/two/$foo/")

(defun endpoint-uri-pattern (e)
  (uri-pattern-components (string-downcase (symbol-name (class-name (class-of e))))))

(defun will-handle-uri (class uri)
  (let ((pattern (uri-pattern-components (string-downcase (class-name class))))
        (test (uri-components uri)))
    (when (= (length pattern)
             (length test))
      (loop for p in pattern
         for a in test
         unless (or (and (keywordp p)
                         (not (equal a "/")))
                    (equal p a))
         do (return-from will-handle-uri nil))
      t)))

;; this grabs the query parameters
(defmethod initialize-instance :after ((e endpoint) &rest initargs)
  (declare (ignore initargs))
  (loop for x in (endpoint-uri-pattern e)
     for piece in (uri-components (endpoint-uri e))
     when (keywordp x)
       ;; !!! I could do something to interpret the value as typed using accept-from-string or something
     do (let ((slot (find x
                          (c2mop:class-slots (class-of e))
                          :test (lambda (name slot)
                                  (member name (c2mop:slot-definition-initargs slot))))))
          (setf (slot-value e (c2mop:slot-definition-name slot))
                piece))))


(defun find-endpoint-for-uri (uri prefix &key (endpoint-superclass (find-class 'endpoint)))
  (when (and (> (length uri) (length prefix))
             (equal (subseq uri 0 (length prefix))
                    prefix))
    (let ((uri (subseq uri (length prefix))))
      (find-if (lambda (subclass)
                 (will-handle-uri subclass uri))
               (c2mop:class-direct-subclasses endpoint-superclass)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Access the bits of the request
(defmethod parameter ((e endpoint) name)
  (funcall (param-function e) name))

(defmethod header ((e endpoint) name)
  (funcall (header-function e) name))

(defmethod body ((e endpoint))
  (funcall (body-function e)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; override this method to figure out the agent making a request to an endpoint
(defmethod agent-for-endpoint ((e endpoint)) nil)

