(in-package #:modularize-user)
(define-module #:keygen
  (:use #:cl #:radiance)
  (:shadow #:make-package #:delete-package #:edit-package #:ensure-package #:delete-file #:file)
  (:export)
  (:local-nicknames))
(in-package #:keygen)

(define-trigger startup ()
  (defaulted-config "Keygen" :title)
  (defaulted-config "Shirakumo" :copyright)
  (defaulted-config (make-random-string 32) :private-key)
  (defaulted-config (make-random-string 16 "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789") :salt))
