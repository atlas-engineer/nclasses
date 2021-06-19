;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.defclass-star+hu.dwim.def
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :version (:read-file-form "version.sexp")
  :depends-on (:hu.dwim.def
               :hu.dwim.defclass-star)
  :components ((:module "integration"
                :components ((:file "def")))))
