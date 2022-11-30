(asdf:defsystem #:keygen-client
  :serial T
  :components ((:file "package")
               (:file "client"))
  :depends-on (:dexador
               :yason
               :north-dexador))
