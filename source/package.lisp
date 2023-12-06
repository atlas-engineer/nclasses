;;;; SPDX-FileCopyrightText: hu.dwim & Atlas Engineer LLC
;;;; SPDX-License-Identifier: Public Domain

(in-package :common-lisp-user)

(defpackage :nclasses
  (:use :common-lisp)
  (:export #:define-class
           #:defclass* ; alias
           #:define-condition*
           #:defcondition* ; alias
           #:define-generic
           #:define-generic* ; alias
           #:defgeneric* ; alias
           #:make-instance*
           #:make* ; alias
           #:make ; alias
           ;; transformers
           #:default-accessor-name-transformer
           #:dwim-accessor-name-transformer
           #:question-mark-accessor-name-transformer
           #:default-initarg-name-transformer
           #:default-slot-definition-transformer
           #:default-predicate-name-transformer
           #:always-dashed-predicate-name-transformer
           #:question-mark-predicate-name-transformer
           #:default-type-inference
           #:make-name-transformer
           #:*allowed-slot-definition-properties*)
  (:documentation "This library offers four helper macros:
- `nclasses:define-class' (aliases `nclasses:define-class*' and `nclasses:defclass*')
- `nclasses:define-condition*' (alias `nclasses:defcondition*').
- `nclasses:define-generic' (aliases `nclasses:define-generic*' and `nclasses:defgeneric*').
- `nclasses:make-instance*' (alias `nclasses:make*' and `nclasses:make').

Compared to the standard macros, they accept extra options and slot definition
is smarter.

Example of `nclasses:define-class':

(define-class foo ()
  ((slot1 :initarg nil)
   (slot2 \"hello!\")
   (unexported-slot :export nil))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer #'nclasses:default-accessor-name-transformer))

In the above, all slot accessors are automatically defined using
`nclasses:default-accessor-name-transformer'.  They are also exported together with the
class name.
The initarg default to the keyword version of the slot symbol, unless it's
explicitly set to NIL.

Notice that the second value of the slot definition, if not an option, is then
the initform.

See `nclasses:define-class' and other macros' documentation for more details."))
