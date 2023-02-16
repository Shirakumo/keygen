(in-package #:org.shirakumo.keygen.client)

(defclass client (north:client)
  ((api-base :reader api-base))
  (:default-initargs
   :api-base NIL
   :request-token-uri NIL
   :authorize-uri NIL
   :access-token-uri NIL))

(defmethod initialize-instance :after ((client client) &key api-base)
  (setf api-base (string-right-trim "/" api-base))
  (setf (slot-value client 'api-base) api-base)
  (unless (north:request-token-uri client)
    (setf (north:request-token-uri client) (format NIL "~a/oauth/request-token" api-base)))
  (unless (north:authorize-uri client)
    (setf (north:authorize-uri client) (format NIL "~a/oauth/authorize" api-base)))
  (unless (north:access-token-uri client)
    (setf (north:access-token-uri client) (format NIL "~a/oauth/access-token" api-base)))
  (unless (north:verify-uri client)
    (setf (north:verify-uri client) (format NIL "~a/oauth/verify" api-base))))

(defun decode-radiance-payload (data)
  (let ((json (com.inuoe.jzon:parse
               (etypecase data
                 (string data)
                 (vector (babel:octets-to-string data))
                 (stream data)))))
    (when (/= 200 (gethash "status" json))
      (error "Request failed: ~s" (gethash "message" json)))
    (gethash "data" json)))

(defmethod post ((client client) endpoint parameters)
  (decode-radiance-payload
   (north:make-signed-request client (format NIL "~a/~a" (api-base client) endpoint)
                              :post :params parameters)))

(defmethod post-file ((client client) endpoint data parameters)
  (decode-radiance-payload
   (north:make-signed-data-request client (format NIL "~a/~a" (api-base client) endpoint)
                                   data :params parameters)))

(defun login (key secret &key (api-base "https://keygen.tymoon.eu/api/"))
  (let* ((client (make-instance 'client :api-base api-base :key key :secret secret))
         (url (north:initiate-authentication client)))
    (format *query-io* "~&> ~a~%Token: " url)
    (finish-output *query-io*)
    (north:complete-authentication client (read-line *query-io*))
    (values client (north:token client) (north:token-secret client))))

(defun upload-file (file payload &key client key secret token token-secret parameters (api-base "https://keygen.tymoon.eu/api/"))
  (let ((parameters (append `(("file" . ,file))
                            parameters))
        (client (or client (make-instance 'client :key key :secret secret :token token :token-secret token-secret :api-base api-base))))
    (post-file client "keygen/file/upload" `(("payload" . ,payload)) parameters)))
