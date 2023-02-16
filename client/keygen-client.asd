(asdf:defsystem #:keygen-client
  :serial T
  :components ((:file "package")
               (:file "client"))
  :depends-on (:dexador
               :com.inuoe.jzon
               :north-dexador))
