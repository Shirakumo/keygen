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

(define-api keygen/file/upload (file payload &optional chunk) (:access (perm keygen))
  (let* ((file (ensure-file (db:ensure-id file)))
         (tmp (make-pathname :type "tmp" :defaults (file-pathname file)))
         (chunk (or* chunk)))
    (cond ((null chunk)
           (uiop:copy-file (first payload) (file-pathname file))
           (edit-file file :last-modified (get-universal-time)))
          ((string= "0" chunk)
           (uiop:copy-file (first payload) tmp))
          ((string= "end" chunk)
           (uiop:rename-file-overwriting-target tmp (file-pathname file))
           (edit-file file :last-modified (get-universal-time)))
          (T
           (with-open-file (in (first payload) :direction :input :element-type '(unsigned-byte 8))
             (with-open-file (out tmp :direction :output :element-type '(unsigned-byte 8) :if-exists :append)
               (uiop:copy-stream-to-stream in out :element-type '(unsigned-byte 8))))))
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

(define-api keygen/key/generate (package count &optional segment expires) (:access (perm keygen))
  (let ((package (ensure-package (db:ensure-id package)))
        (codes (generate-keys package (parse-integer count) :segment segment :expires (when (or* expires) (parse-integer expires)))))
    (setf (header "Cache-Control") "no-store")
    (setf (header "Content-Disposition") (format NIL "inline; filename=\"~a-~a.csv\""
                                                 (dm:field package "title")
                                                 (format-filesystem-date (local-time:now))))
    (setf (content-type *response*) "text/csv;encoding=utf-8")
    (format NIL "~{~a~^~%~}" codes)))

(define-api keygen/key/export (package &optional segment format) (:access (perm keygen))
  (let* ((package (ensure-package (db:ensure-id package)))
         (keys (list-keys package :segment (or* segment))))
    (setf (header "Cache-Control") "no-store")
    (setf (header "Content-Disposition") (format NIL "inline; filename=\"~a-~a.~a\""
                                                 (dm:field package "title")
                                                 (format-filesystem-date (local-time:now))
                                                 format))
    (cond ((string-equal "txt" format)
           (setf (content-type *response*) "text/plain;encoding=utf-8")
           (with-output-to-string (out)
             (dolist (key keys)
               (format out "~a~%" (dm:field key "code")))))
          ((string-equal "csv" format)
           (setf (content-type *response*) "text/csv;encoding=utf-8")
           (with-output-to-string (out)
             (format out "Code, Package, Segment, Owner, Date Created, Date Expires, Date First Access, Date Last Access, Access Count~%")
             (flet ((date (date)
                      (when (and date (< 0 date))
                        (format-machine-date date out))
                      (format out ", ")))
               (dolist (key keys)
                 (format out "~a, ~a, ~@[~a~], ~@[~a~], " (dm:field key "code") (dm:field package "title") (dm:field key "segment") (dm:field key "owner-email"))
                 (date (dm:field key "expires"))
                 (date (dm:field key "first-access"))
                 (date (dm:field key "last-access"))
                 (format out "~d~%" (dm:field key "access-count"))))))
          ((string-equal "json" format)
           (setf (content-type *response*) "application/json;encoding=utf-8")
           (funcall (api-format "json") keys)))))

(defun claim-email (key)
  (let* ((url (key-url key))
         (package (ensure-package key))
         (project (ensure-project package))
         (subject (format NIL "Your code for ~a ~a" (dm:field project "title") (dm:field package "title")))
         (content (r-clip:process (@template "email.ctml")
                                  :subject subject
                                  :package package
                                  :project project
                                  :key key
                                  :url url))
         (text (with-output-to-string (out)
                 (lquery:$ content "body" (text) (each (lambda (text) (write-line text out))))))
         (plump:*tag-dispatchers* plump:*html-tags*))
    (values subject text (plump:serialize content NIL))))

(define-api keygen/key/claim (code email) ()
  (db:with-transaction ()
    (let ((key (find-key code))
          (authcode (email-auth-code email)))
      (unless (key-valid-p key)
        (error 'radiance:request-not-found))
      (setf (dm:field key "owner-email") email)
      (multiple-value-bind (subject text html) (claim-email key)
        (mail:send email subject text :html html))
      (dm:save key)
      (let* ((message (format NIL "Code registered to ~a. Please check your email!" email))
             (target (uri-to-url "keygen/access"
                                 :representation :external
                                 :query `(("code" . ,code)
                                          ("authcode" . ,authcode)
                                          ("message" . ,message)))))
        (if (string= "true" (post/get "browser"))
            (redirect target)
            (api-output key :message message :target target))))))

(define-api keygen/key/resolve (code file &optional authcode) ()
  (let ((key (find-key code)))
    (unless (key-valid-p key authcode)
      (error 'radiance:request-not-found))
    (let ((file (ensure-file (db:ensure-id file))))
      (incf (dm:field file "download-count"))
      (dm:save file)
      (setf (header "Cache-Control") "no-cache, no-transform")
      (setf (header "Last-Modified") (format-last-modified (dm:field file "last-modified")))
      (setf (header "Content-Disposition") (format NIL "inline; filename=\"~a\"" (dm:field file "filename")))
      (serve-file (file-pathname file) "application/octet-stream"))))
