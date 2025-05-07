(asdf:defsystem #:keygen
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "0.0.0"
  :license "zlib"
  :description "A software distribution service"
  :homepage "https://shirakumo.github.io/keygen/"
  :bug-tracker "https://github.com/shirakumo/keygen/issues"
  :source-control (:git "https://github.com/shirakumo/keygen.git")
  :serial T
  :components ((:file "module")
               (:file "toolkit")
               (:file "db")
               (:file "feed")
               (:file "front")
               (:file "api"))
  :depends-on ((:interface :relational-database)
               (:interface :auth)
               (:interface :mail)
               :feeder
               :form-fiddle
               :i-json
               :r-data-model
               :r-clip
               :ratify
               :alexandria
               :crypto-shortcuts
               :local-time
               :cl-ppcre
               :cl-markless-plump))
