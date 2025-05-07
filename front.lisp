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
                   :cover NIL
                   :projects (list-projects)
                   :description "Keygen project dashboard")
      (render-page (config :title) (@template "frontpage.ctml")
                   :cover NIL
                   :description "Access a software package via your personal download key")))

(define-page project "keygen/project/([^/]+)" (:uri-groups (project) :access (perm keygen))
  (let ((project (find-project project)))
    (render-page (dm:field project "title") (@template "project.ctml")
                 :project project
                 :packages (list-packages project)
                 :files (list-files project)
                 :cover (project-cover project)
                 :up (uri-to-url "keygen/" :representation :external)
                 :description (dm:field project "description"))))

(define-page keys "keygen/project/([^/]+)/keys" (:uri-groups (project) :access (perm keygen))
  (let ((project (find-project project)))
    (render-page (dm:field project "title") (@template "keys.ctml")
                 :project project
                 :packages (list-packages project)
                 :keys (list-keys project)
                 :cover (project-cover project)
                 :up (uri-to-url (format NIL "keygen/project/~a" (dm:field project "title")) :representation :external)
                 :description (format NIL "Keys for the ~a project" (dm:field project "title")))))

(define-page public "keygen/access(?:/(.*))?" (:uri-groups (code))
  (let ((code (or code (post/get "code"))))
    (cond (code
           (let* ((key (access-key code (get-var "authcode")))
                  (package (ensure-package key))
                  (project (ensure-project package)))
             (render-page (format NIL "~a ~a" (dm:field project "title") (dm:field package "title"))
                          (@template "access.ctml")
                          :project project
                          :package package
                          :files (list-files package)
                          :cover (project-cover project)
                          :code code
                          :authcode (key-authcode key)
                          :key key
                          :description (format NIL "Access your downloads for ~a ~a" (dm:field project "title") (dm:field package "title")))))
          (T
           (render-page (config :title) (@template "frontpage.ctml")
                        :cover NIL
                        :description "Access a software package via your personal download key")))))

(define-page feed "keygen/feed(?:/(.*))?" (:uri-groups (code))
  (let ((feed (make-feed (access-key (or code (post/get "code") "") (get-var "authcode")))))
    (setf (content-type *response*) "application/atom+xml; charset=utf-8")
    (plump:serialize feed NIL)))
