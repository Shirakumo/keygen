#|
 This file is a part of Keygen
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:keygen)

(defun render-page (page content &rest args)
  (r-clip:with-clip-processing ("template.ctml")
    (apply #'r-clip:process T
           :title (config :title)
           :page page
           :content (plump:parse content)
           :copyright (config :copyright)
           :version (asdf:component-version (asdf:find-system :keygen))
           args)))

(define-page landing "keygen/^$" (:access (perm keygen))
  (if (user:check (auth:current "anonymous") (perm keygen))
      (render-page "Dashboard" (@template "dashboard.ctml")
                   :projects (list-projects))
      (render-page (config :title) (@template "frontpage.ctml"))))

(define-page project "keygen/project/([^/]+)" (:uri-groups (project) :access (perm keygen))
  (let ((project (find-project project)))
    (render-page (dm:field project "title") (@template "project.ctml")
                 :project project
                 :packages (list-packages project)
                 :files (list-files project)
                 :cover (project-cover project)
                 :up (uri-to-url "keygen/" :representation :external))))

(define-page keys "keygen/project/([^/]+)/keys" (:uri-groups (project) :access (perm keygen))
  (let ((project (find-project project)))
    (render-page (dm:field project "title") (@template "keys.ctml")
                 :project project
                 :packages (list-packages project)
                 :keys (list-keys project)
                 :cover (project-cover project)
                 :up (uri-to-url (format NIL "keygen/project/~a" (dm:field project "title")) :representation :external))))

(define-page public "keygen/access(?:/(.*))?" (:uri-groups (code))
  (let ((code (or code (post/get "code"))))
    (cond (code
           (let ((key (find-key code))
                 (authcode (get-var "authcode")))
             (unless (key-valid-p key authcode)
               (error 'radiance:request-not-found))
             (when (or (null (dm:field key "first-access")) (= 0 (dm:field key "first-access")))
               (setf (dm:field key "first-access") (get-universal-time)))
             (setf (dm:field key "last-access") (get-universal-time))
             (incf (dm:field key "access-count"))
             (dm:save key)
             (let* ((package (ensure-package key))
                    (project (ensure-project package)))
               (render-page (format NIL "~a ~a" (dm:field project "title") (dm:field package "title"))
                            (@template "access.ctml")
                            :project project
                            :package package
                            :files (list-files package)
                            :cover (project-cover project)
                            :code code
                            :authcode authcode
                            :key key))))
          (T
           (render-page (config :title) (@template "frontpage.ctml"))))))
