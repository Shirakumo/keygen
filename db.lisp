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

  ;; A file within a project, like: windows, linux, handbook
  (db:create 'file
             '((project (:id project))
               (filename (:varchar 128))
               (types (:varchar 16))
               (last-modified (:integer 5))
               (download-count (:integer 4)))
             :indices '(project))

  ;; Which files a package gives access to
  (db:create 'package-files
             '((package (:id package))
               (file (:id file)))
             :indices '(package))

  ;; A key to access the contents of a package. Can be claimed.
  (db:create 'key
             '((package (:id package))
               (code (:varchar 32))
               (owner-email (:varchar 128))
               (time (:integer 5))
               (expires (:integer 5))
               (first-access (:integer 5))
               (last-access (:integer 5))
               (access-count (:integer 4)))
             :indices '(code package)))

(defmacro define-object (table keys &body body)
  (form-fiddle:with-body-options (body other-options subobjects url) body
    (declare (ignore other-options))
    (let* ((keys (loop for key in keys collect (if (listp key) key (list key T))))
           (req (loop for key in keys when (< (length key) 3) collect (first key)))
           (opt (loop for key in keys when (<= 3 (length key)) collect (list (first key) (third key))))
           (urlargs (loop for field in (rest url)
                          collect `(dm:field object ,field)))
           (url (first url)))
      `(progn
         (defun ,(name- 'make table) (,@req &key ,@opt)
           (let ((object (dm:hull ',table)))
             ,@(loop for (key type) in keys
                     when type
                     collect `(setf (dm:field object ,(string-downcase key)) (,(converter type) ,key)))
             (dm:insert object)
             ,(when (assoc :make body)
                `((lambda ,@(rest (assoc :make body))) object))
             object))

         (defun ,(name- 'ensure table) (object)
           (etypecase object
             (dm:data-model
              (ecase (dm:collection object)
                (,table object)
                ,@(loop for subobject in subobjects
                        collect `(,subobject (,(name- 'ensure table) (dm:field object ,(string-downcase table)))))))
             (db:id
              (or (dm:get-one ',table (db:query (:= '_id object)))
                  (error 'radiance:request-not-found)))
             (T
              (,(name- 'ensure table) (db:ensure-id object)))))

         (defun ,(name- 'edit table) (object &key ,@(loop for (key) in keys
                                                         collect `(,key NIL ,(name- key 'p))))
           (db:with-transaction ()
             (let ((object (,(name- 'ensure table) object)))
               ,@(loop for (key type) in keys
                       when type
                       collect `(when ,(name- key 'p)
                                  (setf (dm:field object ,(string-downcase key)) (,(converter type) ,key))))
               (dm:save object)
               ,(when (assoc :edit body)
                  `((lambda ,@(rest (assoc :edit body))) object))
               object)))

         (defun ,(name- 'delete table) (object)
           (db:with-transaction ()
             (let ((object (,(name- 'ensure table) object)))
               ,@(loop for subobject in subobjects
                       collect `(db:remove ',subobject (db:query (:= ',table (dm:id object)))))
               (dm:delete object)
               ,(when (assoc :delete body)
                  `((lambda ,@(rest (assoc :delete body))) object))
               object)))


         (define-api ,(name/ 'keygen table) (,table) (:access (perm keygen))
           (api-output (,(name- 'ensure table) ,table)))

         (define-api ,(name/ 'keygen table 'new) ,req (:access (perm keygen))
           (let* ((kargs (extract-kargs ',(loop for (key) in opt collect (intern (string key) "KEYWORD"))))
                  (object (apply #',(name- 'make table) ,@req kargs)))
             (output object
                     ,(format NIL "~@(~a~) created" table)
                     ,url ,@urlargs)))

         (define-api ,(name/ 'keygen table 'edit) (,table) (:access (perm keygen))
           (let* ((kargs (extract-kargs ',(loop for (key) in keys collect (intern (string key) "KEYWORD"))))
                  (object (apply #',(name- 'edit table) ,table kargs)))
             (output object
                     ,(format NIL "~@(~a~) edited" table)
                     ,url ,@urlargs)))

         (define-api ,(name/ 'keygen table 'delete) (,table) (:access (perm keygen))
           (let ((object (,(name- 'delete table) ,table)))
             (output object
                     ,(format NIL "~@(~a~) deleted" table)
                     ,url ,@urlargs)))))))

(define-object project
    ((author user (auth:current)) title (description T "") (cover NIL NIL))
  :subobjects (access package file)
  :url ("keygen/project/~a" "_id")
  (:make (project)
         (ensure-directories-exist (project-pathname project))
         (when cover
           (if (string= "image/png" (third cover))
               (uiop:copy-file (first cover) (project-cover project))
               (error "Bad image format. Requires png."))))
  (:edit (project)
         (when cover
           (if (string= "image/png" (third cover))
               (uiop:copy-file (first cover) (project-cover project))
               (error "Bad image format. Requires png."))))
  (:delete (project)
           (uiop:delete-directory-tree (project-pathname project) :validate (constantly T) :if-does-not-exist :ignore)))

(defun find-project (name)
  (or (dm:get-one 'project (db:query (:= 'title name)))
      (ignore-errors (dm:get-one 'project (db:query (:= '_id (db:ensure-id name)))))
      (error 'radiance:request-not-found)))

(defun project-pathname (project)
  (let* ((project (ensure-project project))
         (path (make-pathname :directory (list :relative (princ-to-string (dm:id project))))))
    (environment-module-pathname #.*package* :data path)))

(defun project-cover (project)
  (make-pathname :name "cover" :type "png" :defaults (project-pathname project)))

(defun list-projects ()
  (dm:get 'project (db:query :all)
          :sort '(("title" :ASC))))

(define-object access
    ((project project) (user user) (level integer 0))
  :url ("keygen/project/~a" "project"))

(define-object package
    ((project project) title (description T "") (file[] NIL NIL))
  :subobjects (package-files key)
  :url ("keygen/project/~a" "project")
  (:make (package)
         (dolist (file file[])
           (db:insert 'package-files `(("package" . ,(dm:id package))
                                       ("file" . ,(db:ensure-id file))))))
  (:edit (package)
         (when file[]-p
           (db:remove 'package-files (db:query (:= 'package (dm:id package))))
           (dolist (file file[])
             (db:insert 'package-files `(("package" . ,(dm:id package))
                                         ("file" . ,(db:ensure-id file))))))))

(defun list-packages (project)
  (let ((project (ensure-project project)))
    (dm:get 'package (db:query (:= 'project (dm:id project)))
            :sort '(("title" :asc)))))

(define-object file
    ((project project) filename (types types) (download-count integer 0) (last-modified time (get-universal-time)) (payload NIL NIL))
  :subobjects (package-files)
  :url ("keygen/project/~a" "project")
  (:make (file)
         (when payload
           (uiop:copy-file (first payload) (file-pathname file))))
  (:edit (file)
         (when payload
           (uiop:copy-file (first payload) (file-pathname file))))
  (:delete (file)
           (uiop:delete-file-if-exists (file-pathname file))))

(defun file-pathname (file)
  (let* ((file (ensure-file file))
         (path (make-pathname :name (princ-to-string (dm:id file))
                              :type "dat"
                              :directory (list :relative (princ-to-string (dm:field file "project"))))))
    (environment-module-pathname #.*package* :data path)))

(defun list-files (thing)
  (typecase thing
    (dm:data-model
     (ecase (dm:collection thing)
       (project (dm:get 'file (db:query (:= 'project (dm:id thing)))
                        :sort '(("filename" :asc))))
       (package (dm:get (rdb:join (file _id) (package-files file)) (db:query (:= 'package (dm:id thing)))
                        :sort '(("filename" :asc))))))
    (T (list-files (ensure-project thing)))))

(define-object key
    ((package package) (code T (generate-code)) (owner-email T "") (time time (get-universal-time)) (expires time NIL) (first-access time NIL) (last-access time NIL) (access-count integer 0))
  :url ("keygen/project/~a" "project"))

(defun key-valid-p (key &optional authcode)
  (let* ((key (ensure-key key))
         (owner (dm:field key "owner-email"))
         (expiry (dm:field key "expires")))
    (cond ((or (null owner) (string= "" owner))
           (or (null expiry)
               (= 0 expiry)
               (< (get-universal-time) expiry)))
          (authcode
           (string= authcode (email-auth-code owner))))))

(defun find-key (code)
  (or (dm:get-one 'key (db:query (:= 'code code)))
      (error 'radiance:request-not-found)))

(defun generate-keys (package count &key expires)
  (let ((package (ensure-package package)))
    (db:with-transaction ()
      (loop repeat count
            for key = (make-key package :expires expires)
            collect (dm:field key "code")))))

(defun list-keys (thing)
  (typecase thing
    (dm:data-model
     (ecase (dm:collection thing)
       (project (dm:get (rdb:join (key package) (package _id)) (db:query (:= 'project (dm:id thing)))
                        :sort '(("time" :asc))))
       (package (dm:get 'key (db:query (:= 'package (dm:id thing)))
                        :sort '(("time" :asc))))))
    (T (list-keys (ensure-project thing)))))
