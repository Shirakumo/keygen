#|
 This file is a part of Keygen
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:keygen)

(define-trigger db:connected ()
  ;; A project, like: Kandria
  (db:create 'project
             '((author :id)
               (title (:varchar 64))
               (description :text))
             :indices '(title))

  ;; Access levels for other users
  (db:create 'access
             '((project (:id project))
               (user :id)
               (level (:integer 1)))
             :indices '(project user))

  ;; A download package, like: full, demo, special, OST
  (db:create 'package
             '((project (:id project))
               (title (:varchar 64))
               (description :text))
             :indices '(project))

  ;; A file within a package, like: windows, linux, handbook
  (db:create 'file
             '((package (:id package))
               (filename (:varchar 128))
               (types (:varchar 16))
               (download-count (:integer 4)))
             :indices '(package))

  ;; A key to access the contents of a package. Can be claimed.
  (db:create 'key
             '((package (:id package))
               (code (:varchar 64))
               (owner-email (:varchar 128))
               (expires (:integer 5))
               (first-access-time (:integer 5))
               (last-access-time (:integer 5))
               (access-count (:integer 4)))
             :indices '(code package)))

(defun name (&rest parts)
  (intern (format NIL "~{~a~^-~}" parts)))

(defun enlist (o &rest v)
  (if (listp o) o (list* o v)))

(defun converter (type)
  (case type
    ((NIL T) 'identity)
    (user 'user:id)
    (time '(lambda (x) (or x (get-universal-time))))
    (T `(lambda (x) (dm:id (,(name 'ensure type) x))))))

(defun generate-code ()
  ;; FIXME: needs to be unique!
  )

(defmacro define-object (table keys &optional subobjects)
  (let ((keys (mapcar #'enlist keys)))
    `(progn
       (defun ,(name 'make table) (,@(loop for key in keys when (< (length key) 3) collect (first key))
                                   &key ,@(loop for key in keys when (= (length key) 3) collect (list (first key) (third key))))
         (let ((object (dm:hull ',table)))
           ,@(loop for (key type) in keys
                   collect `(setf (dm:field object ,(string-downcase key)) (,(converter type) ,key)))
           (dm:insert object)))

       (defun ,(name 'ensure table) (object)
         (etypecase object
           (dm:data-model
            (ecase (dm:collection object)
              (,table object)
              ,@(loop for subobject in subobjects
                      collect `(,subobject (,(name 'ensure table) (dm:field object ,(string table)))))))
           (db:id
            (dm:get-one ',table (db:query (:= '_id object))))
           (T
            (,(name 'ensure table) (db:ensure-id object)))))

       (defun ,(name 'edit table) (object &key ,@(loop for (key) in keys
                                                       collect `(,key NIL ,(name key 'p))))
         (db:with-transaction ()
           (let ((object (,(name 'ensure table) object)))
             ,@(loop for (key type) in keys
                     collect `(when ,(name key 'p)
                                (setf (dm:field object ,(string-downcase key)) (,(converter type) ,key))))
             (dm:save object))))

       (defun ,(name 'delete table) (object)
         (db:with-transaction ()
           (let ((object (,(name 'ensure table) object)))
             ,@(loop for subobject in subobjects
                     collect `(db:remove ',subobject (db:query (:= ',table (dm:id object)))))
             (dm:delete object)))))))

(define-object project ((author user (auth:current)) title (description T "")) (access package))
(define-object access ((project project) user level))
(define-object package ((project project) title (description T "")) (file key))
(define-object file ((package package) filename types (download-count T 0)))
(define-object key ((package package) (code NIL (generate-code)) (owner-email T NIL) (expires T NIL) (first-access-time T NIL) (last-access-time T NIL) (access-count T 0)))
