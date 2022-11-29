#|
 This file is a part of Keygen
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem #:keygen
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
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
               (:file "front")
               (:file "api"))
  :depends-on ((:interface :relational-database)
               (:interface :auth)
               (:interface :mail)
               :form-fiddle
               :i-json
               :r-data-model
               :r-clip
               :ratify
               :alexandria
               :crypto-shortcuts
               :local-time
               :cl-ppcre))
