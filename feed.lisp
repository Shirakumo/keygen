(in-package #:keygen)

(defun make-feed (key)
  (let* ((package (ensure-package key))
         (project (ensure-project package))
         (feed (make-instance 'feeder:feed
                              :id (dm:id project)
                              :link (make-instance 'feeder:link :url (key-url key))
                              :logo (cover-url project)
                              :title (format NIL "~a ~a"
                                             (dm:field project "title")
                                             (dm:field package "title"))
                              :published-on (local-time:universal-to-timestamp (dm:field key "time"))
                              :summary (format-text (dm:field project "description")
                                                    (dm:field package "description"))
                              :generator (make-instance 'feeder:generator :name "Keygen"))))
    (dolist (file (list-files package) (feeder:serialize-feed feed 'feeder:atom))
      (push
       (make-instance 'feeder:entry
                      :id (dm:id file)
                      :link (make-instance 'feeder:link :url (file-url file key)
                                                        :title "download")
                      :title (dm:field file "filename")
                      :updated-on (local-time:universal-to-timestamp (dm:field file "last-modified"))
                      :categories (map 'list #'type-name (dm:field file "types"))
                      :summary (format NIL "~a ~@[v~a ~]last changed on ~a"
                                       (dm:field file "filename")
                                       (or* (dm:field file "version"))
                                       (format-fancy-date (dm:field file "last-modified"))))
       (feeder:content feed)))))
