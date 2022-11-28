#|
 This file is a part of Keygen
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:keygen)

(defun name (&rest parts)
  (intern (format NIL "~{~a~^-~}" parts)))

(defun name/ (&rest parts)
  (intern (format NIL "~{~a~^/~}" parts)))

(defun enlist (o &rest v)
  (if (listp o) o (list* o v)))

(defun converter (type)
  (case type
    ((NIL T) 'identity)
    (user 'user:id)
    (integer '(lambda (x) (when x (parse-integer x))))
    (time '(lambda (x) (when x (parse-integer x))))
    (T `(lambda (x) (dm:id (,(name 'ensure type) x))))))

(defun format-last-modified (timestamp)
  (local-time:format-timestring
   NIL (etypecase timestamp
         (local-time:timestamp)
         (integer (local-time:universal-to-timestamp timestamp))
         ((eql T) (local-time:now)))
   :format '(:short-weekday ", " (:day 2) #\space :short-month #\space (:year 4) #\space
             (:hour 2) #\: (:min 2) #\: (:sec 2) #\space "GMT")
   :timezone local-time:+gmt-zone+))

(defun generate-code ()
  (let ((max-code #.(parse-integer (make-string 32 :initial-element #\Z) :radix 36)))
    (write-to-string (logxor (random max-code) (parse-integer (or (config :salt) "0") :radix 36)) :base 36)))

(defun output (object message url-format &rest args)
  (let ((target (uri-to-url (apply #'format NIL url-format args)
                            :representation :external
                            :query `(("message" . ,message)))))
    (if (string= "true" (post/get "browser"))
        (redirect target)
        (api-output object :message message :target target))))

(defun extract-kargs (keys)
  (let* ((request *request*)
         (post (post-data request))
         (get (get-data request)))
    (loop for key in keys
          for val = (or (gethash (string key) post)
                        (gethash (string key) get))
          when val collect key
          when val collect val)))

(defun email-auth-code (email)
  (cryptos:hmac email (config :private-key) :digest :sha256 :to :hex))
