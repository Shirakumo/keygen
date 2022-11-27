#|
 This file is a part of Keygen
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:keygen
  (:use #:cl #:radiance)
  (:shadow #:make-package #:delete-package #:edit-package #:ensure-package #:delete-file)
  (:export)
  (:local-nicknames))
(in-package #:keygen)

(define-trigger startup ()
  (defaulted-config "Keygen" :title)
  (defaulted-config "Shirakumo" :copyright)
  (defaulted-config (make-random-string 32) :private-key)
  (defaulted-config (make-random-string 32) :salt))
