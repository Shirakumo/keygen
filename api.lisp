#|
 This file is a part of Keygen
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:keygen)

(define-api keygen/project/list () (:access (perm keygen))
  ;; FIXME: filter by access
  (api-output (list-projects)))

(define-api keygen/project/find (name) (:access (perm keygen))
  (api-output (find-package name)))

(define-api keygen/project/cover (project) ()
  (setf (header "Cache-Control") "public")
  (serve-file (project-cover project) "image/png"))

(define-api keygen/package/list (project) (:access (perm keygen))
  (let ((project (ensure-project (db:ensure-id project))))
    (api-output (list-packages project))))

(define-api keygen/package/files (package) (:access (perm keygen))
  (let ((package (ensure-package (db:ensure-id package))))
    (api-output (list-files package))))

(define-api keygen/package/keys (package) (:access (perm keygen))
  (let ((package (ensure-package (db:ensure-id package))))
    (api-output (list-keys package))))

(define-api keygen/file/list (project) (:access (perm keygen))
  (let ((project (ensure-project (db:ensure-id project))))
    (api-output (list-files project))))

(define-api keygen/file/upload (file payload) (:access (perm keygen))
  (let ((file (ensure-file (db:ensure-id file))))
    (uiop:copy-file (first payload) (file-pathname file))
    (edit-file file :last-modified (get-universal-time))
    (output file "File uploaded" "keygen/project/~a" (dm:field file "project"))))

(define-api keygen/file/download (file) (:access (perm keygen))
  (let ((file (ensure-file (db:ensure-id file))))
    (setf (header "Cache-Control") "private")
    (setf (header "Last-Modified") (format-last-modified (dm:field file "last-modified")))
    (setf (header "Content-Disposition") (format NIL "inline; filename=\"~a\"" (dm:field file "filename")))
    (serve-file (file-pathname file) "application/octet-stream")))

(define-api keygen/key/list (package) (:access (perm keygen))
  (let ((package (ensure-package (db:ensure-id package))))
    (api-output (list-keys package))))

(define-api keygen/key/generate (package count &optional expires) (:access (perm keygen))
  (let ((package (ensure-package (db:ensure-id package))))
    (api-output (generate-keys package (parse-integer count) :expires (when expires (parse-integer expires))))))

(define-api keygen/key/export (package) (:access (perm keygen))
  #++
  (let* ((package (ensure-package (db:ensure-id package)))
         (keys (list-keys package)))
    (setf (header "Content-Disposition") (format NIL "inline; filename=\"~a.csv\""))
    (setf (content-type *response*) "text/csv;encoding=utf-8")
    (setf (data response) (generate-csv keys))))

(define-api keygen/key/claim (code email) ()
  (db:with-transaction ()
    (let ((key (find-key code)))
      (unless (key-valid-p key)
        (error 'radiance:request-not-found))
      (setf (dm:field key "owner-email") email)
      (dm:save key)
      (redirect (uri-to-url "keygen/access"
                            :representation :external
                            :query `(("message" . ,(format NIL "Code registered to ~a. Please check your email!" email))
                                     ("code" . ,code)
                                     ("authcode" . ,(email-auth-code email))))))))

(define-api keygen/key/resolve (code file &optional authcode) ()
  (db:with-transaction ()
    (let ((key (find-key code)))
      (unless (key-valid-p key authcode)
        (error 'radiance:request-not-found))
      (let ((file (ensure-file (db:ensure-id file))))
        (setf (header "Cache-Control") "private")
        (setf (header "Last-Modified") (format-last-modified (dm:field file "last-modified")))
        (setf (header "Content-Disposition") (format NIL "inline; filename=\"~a\"" (dm:field file "filename")))
        (serve-file (file-pathname file) "application/octet-stream")))))
