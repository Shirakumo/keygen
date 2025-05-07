(in-package #:keygen)

(defun name- (&rest parts)
  (intern (format NIL "~{~a~^-~}" parts)))

(defun name/ (&rest parts)
  (intern (format NIL "~{~a~^/~}" parts)))

(defun enlist (o &rest v)
  (if (listp o) o (list* o v)))

(defun converter (type)
  (case type
    ((NIL T) 'identity)
    (user 'user:id)
    (integer '(lambda (x) (etypecase x (string (parse-integer x)) (integer x) (null))))
    (time '(lambda (x) (etypecase x (string (parse-integer x)) (integer x) (null))))
    (types '(lambda (x) (parse-types x)))
    (T `(lambda (x) (dm:id (,(name- 'ensure type) x))))))

(defun format-last-modified (timestamp)
  (local-time:format-timestring
   NIL (etypecase timestamp
         (local-time:timestamp)
         (integer (local-time:universal-to-timestamp timestamp))
         ((eql T) (local-time:now)))
   :format '(:short-weekday ", " (:day 2) #\space :short-month #\space (:year 4) #\space
             (:hour 2) #\: (:min 2) #\: (:sec 2) #\space "GMT")
   :timezone local-time:+gmt-zone+))

(defun format-filesystem-date (stamp &optional stream)
  (when (integerp stamp) (setf stamp (local-time:universal-to-timestamp stamp)))
  (local-time:format-timestring
   stream stamp :format '((:year 4) "." (:month 2) "." (:day 2) " " (:hour 2) "-" (:min 2) "-" (:sec 2))
   :timezone local-time:+utc-zone+))

(defun generate-code ()
  (let* ((max-code #.(parse-integer (make-string 16 :initial-element #\Z) :radix 36))
         (code (logxor (random max-code) (parse-integer (or (config :salt) "0") :radix 36)))
         (string (format NIL "~36,16,'0r" code)))
    (if (< 16 (length string))
        (subseq string 0 16)
        string)))

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


(defparameter *file-types*
  '((#\a "fab fa-android" "android")
    (#\c "fas fa-file-code" "source-code" "source" "code")
    (#\i "fas fa-file-image" "image" "picture" "drawing")
    (#\l "fab fa-linux" "linux" "unix" "ubuntu")
    (#\m "fab fa-apple" "apple" "macos" "mac os" "osx" "os x")
    (#\o "fas fa-file-audio" "soundtrack" "audio" "music" "ost")
    (#\p "fas fa-file-pdf" "pdf" "document")
    (#\t "fas fa-file-lines" "text" "text file")
    (#\v "fas fa-file-video" "video" "movie" "making-of")
    (#\w "fab fa-windows" "windows" "microsoft windows" "ms windows" "ms-dos")
    (#\z "fas fa-file-zipper" "zip" "archive" "zip file")))

(defun type-icon (type)
  (or (second (assoc type *file-types*))
      "fa-circle-question"))

(defun type-name (type)
  (or (third (assoc type *file-types*))
      "???"))

(defun type-char (name)
  (flet ((match (a b)
           (and (= (length a) (length b))
                (loop for ac across a
                      for bc across b
                      always (or (char= ac bc)
                                 (and (find ac "- _")
                                      (find bc "- _")))))))
    (setf name (string-trim " " name))
    (loop for (char icon . names) in *file-types*
          do (when (find name names :test #'match)
               (return char))
          finally (error "Unknown type ~s" name))))

(defun parse-types (types)
  (map 'string #'type-char (cl-ppcre:split "[ ,]+" types)))

(defun unparse-types (types)
  (format NIL "~{~a~^, ~}" (map 'list #'type-name types)))

(defun format-text (&rest text)
  (let ((target (plump:make-root)))
    (dolist (text text target)
      (org.shirakumo.markless:output
       text
       :target target
       :format 'org.shirakumo.markless.plump:plump))))
