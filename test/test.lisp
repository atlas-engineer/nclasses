;;;; SPDX-FileCopyrightText: hu.dwim & Atlas Engineer LLC
;;;; SPDX-License-Identifier: Public Domain

(in-package :nclasses/test)

(defun with-test-class-options (thunk)
  "A context sets the class options to specific defaults."
  (let ((*package* #.*package*)
        (nclasses::*automatic-accessors-p* t)
        (nclasses::*accessor-name-transformer* 'dwim-accessor-name-transformer)
        (nclasses::*automatic-initargs-p* t)
        (nclasses::*initarg-name-transformer* 'default-initarg-name-transformer)
        (nclasses::*export-class-name-p* nil)
        (nclasses::*export-accessor-names-p* nil)
        (nclasses::*export-slot-names-p* nil)
        (nclasses::*automatic-predicates-p* nil)
        (nclasses::*automatic-types-p* nil))
    (funcall thunk)))

(defun with-type-inheritence-class-options (thunk)
  "A context sets the class options to specific defaults."
  (let ((*package* #.*package*)
        (nclasses::*automatic-accessors-p* t)
        (nclasses::*accessor-name-transformer* 'dwim-accessor-name-transformer)
        (nclasses::*automatic-initargs-p* t)
        (nclasses::*initarg-name-transformer* 'default-initarg-name-transformer)
        (nclasses::*export-class-name-p* nil)
        (nclasses::*export-accessor-names-p* nil)
        (nclasses::*export-slot-names-p* nil)
        (nclasses::*automatic-predicates-p* nil)
        (nclasses::*automatic-types-p* t))
    (funcall thunk)))

(defmacro assert-slot= (expansion form &rest extras)
  `(let ((nclasses::*accessor-names* nil)
         (nclasses::*slot-names* nil))
     (assert-equal
      ',expansion
      (nclasses::process-slot-definition ',form)
      ,@extras)))

(defmacro assert-slot-warns (form &rest extras)
  `(let ((nclasses::*accessor-names* nil)
         (nclasses::*slot-names* nil))
     (assert-warning 'warning
                     (nclasses::process-slot-definition ',form) ,@extras)))

(defmacro assert-slot-errors (form &rest extras)
  `(let ((nclasses::*accessor-names* nil)
         (nclasses::*slot-names* nil))
     (assert-error 'error
                   (nclasses::process-slot-definition ',form) ,@extras)))

(define-class foo ()
  ((name "fooname")))

(define-test simple-class ()
  (assert-string= "fooname"
                  (let ((foo (make-instance 'foo)))
                    (name foo))))

(define-class bar ()
  ((name "fooname")
   (age :accessor this-age)
   (address :accessor nil))
  (:accessor-name-transformer (lambda (name def) (declare (ignore def)) name)))

(define-test simple-class-with-custom-accessors ()
  (make-instance 'bar)
  (assert-true (fboundp 'name))
  (assert-true (fboundp 'this-age))
  (assert-false (fboundp 'address)))

(define-class foo-no-accessors ()
  ((name-no-acc :unbound :type string))
  (:automatic-accessors-p nil))

(define-test no-accessor ()
  (assert-false (progn
                  (make-instance 'foo-no-accessors)
                  (fboundp 'name-no-acc))))

(define-test nop (:contexts '(with-test-class-options))
  (assert-expands
   (defclass some-class (some super classes)
     ()
     (1 2)
     (3 4))
   (define-class some-class (some super classes)
     ()
     (1 2)
     (3 4))))

(define-test accessors (:contexts '(with-test-class-options))
  (assert-slot= (slot1 :accessor slot1-custom :initarg slot1-custom)
                (slot1 :unbound :accessor slot1-custom :initarg slot1-custom))
  (assert-slot= (slot1 :initform nil :accessor slot1-of :initarg :slot1)
                (slot1))
  (assert-slot= (slot1 :accessor slot1-of :initarg :slot1)
                (slot1 :unbound))
  (assert-slot= (slot1 :accessor slot1-p :initarg :slot1 :type boolean)
                (slot1 :unbound :type boolean))
  (assert-slot= (slotp :accessor slotp :initarg :slotp :type boolean)
                (slotp :unbound :type boolean))
  (assert-slot= (slot-name :accessor slot-name-p :initarg :slot-name :type boolean)
                (slot-name :unbound :type boolean))
  (let ((nclasses::*automatic-accessors-p* nil)
        (nclasses::*automatic-initargs-p* nil))
    (assert-slot= (slot1 :initform nil)
                  (slot1))
    (assert-slot= (slot1)
                  (slot1 :unbound)))
  (let ((nclasses::*accessor-name-transformer* (make-name-transformer "FOO-" name "-BAR"))
        (nclasses::*initarg-name-transformer* (make-name-transformer "BAZ-" name)))
    (assert-slot= (slot1 :initform nil :accessor foo-slot1-bar :initarg baz-slot1)
                  (slot1))
    (assert-slot= (slot1 :accessor foo-slot1-bar :initarg baz-slot1)
                  (slot1 :unbound))))

(define-test reconfiguration (:contexts '(with-test-class-options))
  (assert-expands (defclass some-class (some super classes)
                    ((slot1 :initform nil :accessor slot1-zork :initarg :slot1))
                    (1 2)
                    (3 4))
                  (define-class some-class (some super classes)
                      ((slot1))
                      (1 2)
                      (:accessor-name-transformer (make-name-transformer name "-ZORK"))
                      (3 4)))
  (assert-expands (defclass some-class (some super classes)
                    ((slot1 :accessor slot1-zork :initarg :slot1))
                    (1 2)
                    (3 4))
                  (define-class some-class (some super classes)
                      ((slot1 :unbound))
                      (1 2)
                      (:accessor-name-transformer (make-name-transformer name "-ZORK"))
                      (3 4)))
  (assert-expands (defclass some-class (some super classes)
                    ((slot1 :initform nil :accessor nclasses/test.dummy::slot1-of :initarg :slot1)))
                  (define-class some-class (some super classes)
                      ((slot1))
                      (:accessor-name-package (find-package :nclasses/test.dummy))))
  (assert-expands (defclass some-class (some super classes)
                    ((slot1 :accessor nclasses/test.dummy::slot1-of :initarg :slot1)))
                  (define-class some-class (some super classes)
                      ((slot1 :unbound))
                      (:accessor-name-package (find-package :nclasses/test.dummy)))))

(define-test full (:contexts '(with-test-class-options))
  (assert-expands (defclass some-class (some super classes)
                    ((slot1 :accessor slot1-of :initarg :slot1 :documentation "zork"))
                    (1 2)
                    (3 4))
                  (define-class some-class (some super classes)
                      ((slot1 :unbound :documentation "zork"))
                      (1 2)
                      (3 4)))
  (assert-expands (defclass some-class (some super classes)
                    ((slot1 :initform 42 :documentation "zork"))
                    (1 2)
                    (3 4))
                  (define-class some-class (some super classes)
                    ((slot1 42 :documentation "zork"))
                    (1 2)
                    (:automatic-accessors-p nil)
                    (:automatic-initargs-p nil)
                    (3 4)))
  (assert-equal `(progn
                   (defclass some-class (some super classes)
                     ((slot1 :initform 42 :accessor slot1-of :initarg :slot1
                             :documentation "zork")
                      (slot2 :accessor slot2-custom :initarg :slot2)
                      (slot3 :initarg :slot3)))
                   (eval-when (:compile-toplevel :load-toplevel :execute)
                     (export '(some-class slot2 slot2-custom slot1 slot1-of) ,(package-name *package*)))
                   (setf (documentation 'slot2-custom 'function)
                         "Auto-generated accessor function for slot SLOT2.")
                   (ignore-errors
                    (setf (documentation (fdefinition 'slot2-custom) 'function)
                          "Auto-generated accessor function for slot SLOT2."))
                   (ignore-errors
                    (setf (documentation (fdefinition '(setf slot2-custom)) 'function)
                          "Auto-generated accessor function for slot SLOT2."))
                   (setf (documentation 'slot1-of 'function)
                         "Auto-generated accessor function for slot SLOT1.
zork")
                   (ignore-errors
                    (setf (documentation (fdefinition 'slot1-of) 'function)
                          "Auto-generated accessor function for slot SLOT1.
zork"))
                   (ignore-errors
                    (setf (documentation (fdefinition '(setf slot1-of)) 'function)
                          "Auto-generated accessor function for slot SLOT1.
zork"))
                   (find-class 'some-class nil))
                (macroexpand-1 '(define-class some-class (some super classes)
                                    ((slot1 42 :documentation "zork")
                                     (slot2 :unbound :accessor slot2-custom)
                                     (slot3 :unbound :accessor nil :export :accessor))
                                    (:export-accessor-names-p t)
                                    (:export-class-name-p t)
                                    (:export-slot-names-p t))))
  (assert-equal `(progn
                   (defclass some-class (some super classes)
                     ((slot1 :initform 42 :accessor slot1-of :initarg :slot1
                             :documentation "zork")
                      (slot2 :accessor slot2-custom :initarg :slot2)
                      (slot3 :initform 42 :accessor slot3-of :initarg :slot3)
                      (slot4 :initform 42 :accessor slot4-of :initarg :slot4)))
                   (eval-when (:compile-toplevel :load-toplevel :execute)
                     (export '(some-class slot3-of slot2 slot2-custom slot1) ,(package-name *package*)))
                   (setf (documentation 'slot4-of 'function)
                         "Auto-generated accessor function for slot SLOT4.")
                   (ignore-errors
                    (setf (documentation (fdefinition 'slot4-of) 'function)
                          "Auto-generated accessor function for slot SLOT4."))
                   (ignore-errors
                    (setf (documentation (fdefinition '(setf slot4-of)) 'function)
                          "Auto-generated accessor function for slot SLOT4."))
                   (setf (documentation 'slot3-of 'function)
                         "Auto-generated accessor function for slot SLOT3.")
                   (ignore-errors
                    (setf (documentation (fdefinition 'slot3-of) 'function)
                          "Auto-generated accessor function for slot SLOT3."))
                   (ignore-errors
                    (setf (documentation (fdefinition '(setf slot3-of)) 'function)
                          "Auto-generated accessor function for slot SLOT3."))
                   (setf (documentation 'slot2-custom 'function)
                         "Auto-generated accessor function for slot SLOT2.")
                   (ignore-errors
                    (setf (documentation (fdefinition 'slot2-custom) 'function)
                          "Auto-generated accessor function for slot SLOT2."))
                   (ignore-errors
                    (setf (documentation (fdefinition '(setf slot2-custom)) 'function)
                          "Auto-generated accessor function for slot SLOT2."))
                   (setf (documentation 'slot1-of 'function)
                         "Auto-generated accessor function for slot SLOT1.
zork")
                   (ignore-errors
                    (setf (documentation (fdefinition 'slot1-of) 'function)
                          "Auto-generated accessor function for slot SLOT1.
zork"))
                   (ignore-errors
                    (setf (documentation (fdefinition '(setf slot1-of)) 'function)
                          "Auto-generated accessor function for slot SLOT1.
zork"))
                   (find-class 'some-class nil))
                (macroexpand-1 '(define-class some-class (some super classes)
                                 ((slot1 42 :documentation "zork" :export :slot)
                                  (slot2 :unbound :accessor slot2-custom)
                                  (slot3 42 :export :accessor)
                                  (slot4 42 :export nil))
                                 (:export-accessor-names-p t)
                                 (:export-class-name-p t)
                                 (:export-slot-names-p t)))))

(with-test-class-options
  (lambda ()
    (macroexpand-1 '(define-class some-class (some super classes)
                     ((slot1 42 :documentation "zork")
                      (slot2 :unbound :accessor slot2-custom)
                      (slot3 :unbound :accessor nil :export :accessor))
                     (:export-accessor-names-p t)
                     (:export-class-name-p t)
                     (:export-slot-names-p t)))))

(define-test warnings-and-errors (:contexts '(with-test-class-options))
  (let ((*allowed-slot-definition-properties* *allowed-slot-definition-properties*))
    (push :asdf *allowed-slot-definition-properties*)
    (push :unspecified *allowed-slot-definition-properties*)
    (assert-slot= (slot1 :accessor slot1-of :initarg :slot1 :asdf 42 :unspecified 43)
                  (slot1 :unbound :asdf 42 :unspecified 43))
    (assert-slot= (slot1 :initform nil :accessor slot1-of :initarg :slot1 :asdf 42 :unspecified 43)
                  (slot1 :asdf 42 :unspecified 43)))
  (assert-slot-warns (slot1 :unbound :asdf slot1-custom))
  (assert-slot-warns (slot1 :asdf slot1-custom))
  (assert-slot-errors (slot1 :unbound foo bar))
  (assert-slot-errors (slot1 foo bar))

  (assert-error 'error
                (macroexpand-1 '(define-class some-class ()
                                 ((slot1))
                                 (:accessor-name-transformer 'default-accessor-name-transformer many)))))

;;; `fmakunbound' seems to be broken on ECL 21.2.1, since it results in the symbol not being rebindable.
#-ecl
(define-test skip-export-slot-name-from-foreign-package (:contexts '(with-test-class-options))
  (let* ((pkg1 (find-package :nclasses/test.pkg1))
         (pkg2 (find-package :nclasses/test.pkg2))
         (exp1 (let ((*package* pkg1))
                 (macroexpand-1
                  '(define-class nclasses/test.pkg1::foo ()
                    ((nclasses/test.pkg1::foo-name))
                    (:export-slot-names-p nil)
                    (:accessor-name-transformer 'dwim-accessor-name-transformer)))))
         (exp2 (let ((*package* pkg2))
                 (macroexpand-1
                  '(define-class nclasses/test.pkg2::bar
                    (nclasses/test.pkg1::foo)
                    ;; override the slot in the superclass
                    ((nclasses/test.pkg1::foo-name :initform ""))
                    (:accessor-name-transformer 'dwim-accessor-name-transformer)
                    (:export-slot-names-p t))))))
    (let ((*package* pkg1))
      (eval exp1))
    (let ((*package* pkg2))
      (eval exp2))
    (assert-eq :internal
               (nth-value 1 (find-symbol "FOO-NAME" :nclasses/test.pkg1)))
    (fmakunbound 'nclasses/test.pkg1::foo-name-of)
    (fmakunbound 'nclasses/test.pkg2::foo-name-of)))

#-ecl
(define-test accessor-generation-from-foreign-package ()
  (let* ((pkg1 (find-package :nclasses/test.pkg1))
         (pkg2 (find-package :nclasses/test.pkg2)))
    (let ((*package* pkg1))
      (eval
       '(define-class nclasses/test.pkg1::foo ()
         ((nclasses/test.pkg1::foo-desc :accessor nil)))))
    (let ((*package* pkg2))
      (eval
       '(define-class nclasses/test.pkg2::bar (nclasses/test.pkg1::foo)
         ;; override the slot in the superclass
         ((nclasses/test.pkg1::foo-desc :initform "")))))
    (assert-false (fboundp 'nclasses/test.pkg1::foo-desc))
    (let ((*package* pkg2))
      (eval '(define-class nclasses/test.pkg2::baz (nclasses/test.pkg1::foo)
              ;; override the slot in the superclass
              ((nclasses/test.pkg1::foo-desc :initform ""
                                             :accessor t)))))
    (assert-false (fboundp 'nclasses/test.pkg1::foo-desc))
    (let ((*package* pkg2))
      (eval '(define-class nclasses/test.pkg2::qux (nclasses/test.pkg1::foo)
              ;; override the slot in the superclass
              ((nclasses/test.pkg1::foo-desc :initform ""
                                             :accessor t))
              (:accessor-name-package :slot-name))))
    (assert-true (fboundp 'nclasses/test.pkg1::foo-desc))
    (fmakunbound 'nclasses/test.pkg1::foo-desc)))

#-ecl
(define-test accessor-generation-from-foreign-package-dwim (:contexts '(with-test-class-options))
  (let* ((pkg1 (find-package :nclasses/test.pkg1))
         (pkg2 (find-package :nclasses/test.pkg2)))
    (let ((*package* pkg1))
      (eval
       '(define-class nclasses/test.pkg1::foo ()
         ((nclasses/test.pkg1::foo-bio :accessor nil))
         (:accessor-name-transformer 'dwim-accessor-name-transformer))))
    (let ((*package* pkg2))
      (eval
       '(define-class nclasses/test.pkg2::bar (nclasses/test.pkg1::foo)
         ;; override the slot in the superclass
         ((nclasses/test.pkg1::foo-bio :initform ""))
         (:accessor-name-transformer 'dwim-accessor-name-transformer))))

    (assert-true (fboundp 'nclasses/test.pkg2::foo-bio-of))
    (assert-false (fboundp 'nclasses/test.pkg1::foo-bio-of))
    (let ((*package* pkg2))
      (eval '(define-class nclasses/test.pkg2::baz (nclasses/test.pkg1::foo)
              ;; override the slot in the superclass
              ((nclasses/test.pkg1::foo-bio :initform ""
                                            :accessor t)))))
    (assert-false (fboundp 'nclasses/test.pkg1::foo-bio-of))
    (let ((*package* pkg2))
      (eval '(define-class nclasses/test.pkg2::qux (nclasses/test.pkg1::foo)
              ;; override the slot in the superclass
              ((nclasses/test.pkg1::foo-bio :initform ""))
              (:accessor-name-package :slot-name))))
    (assert-false (fboundp 'nclasses/test.pkg1::foo-bio-of))
    (let ((*package* pkg2))
      (eval '(define-class nclasses/test.pkg2::qux (nclasses/test.pkg1::foo)
              ;; override the slot in the superclass
              ((nclasses/test.pkg1::foo-bio :initform ""
                                            :accessor t))
              (:accessor-name-package :slot-name))))
    (assert-true (fboundp 'nclasses/test.pkg1::foo-bio-of))
    (fmakunbound 'nclasses/test.pkg1::foo-bio-of)
    (fmakunbound 'nclasses/test.pkg2::foo-bio-of)))

(defvar street-name "bar")
(define-test type-inference ()
  (define-class foo-type-infer ()
    ((name "foo")
     (nickname street-name)
     (age 1)
     (height 2.0)
     (width 2 :type number)
     (lisper t)
     (nil-is-not-bool nil)
     (empty-list '())
     (nonempty-list '(1 2 3))
     (mark :foo)
     (binding :unbound)
     (sym 'sims)
     (fun #'list)
     (composite (error "Should not eval, type should not be inferred")))
    (:automatic-types-p t))
  (assert-eq 'string
             (getf (mopu:slot-properties 'foo-type-infer 'name) :type))
  (assert-eq 'string
             (getf (mopu:slot-properties 'foo-type-infer 'nickname) :type))
  (assert-eq 'integer
             (getf (mopu:slot-properties 'foo-type-infer 'age) :type))
  (assert-eq 'number
             (getf (mopu:slot-properties 'foo-type-infer 'height) :type))
  (assert-eq 'number
             (getf (mopu:slot-properties 'foo-type-infer 'width) :type))
  (assert-eq 'boolean
             (getf (mopu:slot-properties 'foo-type-infer 'lisper) :type))
  (assert (not (eq 'boolean
                   (getf (mopu:slot-properties 'foo-type-infer 'nil-is-not-bool) :type))))
  (assert-eq 'list
             (getf (mopu:slot-properties 'foo-type-infer 'empty-list) :type))
  (assert-eq 'list
             (getf (mopu:slot-properties 'foo-type-infer 'nonempty-list) :type))
  (assert-eq 'keyword
             (getf (mopu:slot-properties 'foo-type-infer 'mark) :type))
  (assert-false
   (getf (mopu:slot-properties 'foo-type-infer 'binding) :type))
  (assert-eq 'symbol
             (getf (mopu:slot-properties 'foo-type-infer 'sym) :type))
  (assert-eq 'function
             (getf (mopu:slot-properties 'foo-type-infer 'fun) :type))
  (assert-eq nil
             (getf (mopu:slot-properties 'foo-type-infer 'composite) :type)))


(define-class parent ()
  ((name nil
         :type (or null string))
   (age nil
        :type (or null number))))

(define-test inherited-type-inference ()
  (define-class child (parent)
    ((name "foo" :type string)          ; inherited.
     (age 17)                           ; inherited.
     (bio "Dummy biography")))
  (let ((c (make-instance 'child)))
    ;; Perform some assignments: CCL can catch type errors here.
    (setf (slot-value c 'age) 15)
    #+ccl
    (assert-error 'error
                  (setf (slot-value c 'name) nil))
    (assert-equal 'string
                  (getf (mopu:slot-properties 'child 'name) :type))
    ;; Given that the parent may or may not be finalized by then, we have to way
    ;; to test it but checking it falls into a reliable set of options (NIL =
    ;; parent not finalized, type = parent finalized).
    (assert-true (member (getf (mopu:slot-properties 'child 'age) :type)
                         (list nil '(or null number))
                         :test #'equal))
    (assert-true (member (getf (mopu:slot-properties 'child 'bio) :type)
                         (list nil 'string)
                         :test #'equal))))

(define-class parent2 ()
  ((name nil
         :type (or null string))
   (age nil
        :type (or null number))))
(closer-mop:ensure-finalized (find-class 'parent2))

(define-test inherited-type-inference-after-finalized ()
  (define-class child2 (parent2)
    ((name "foo" :type string)
     (age 17)
     (bio "Dummy biography"))
    (:automatic-types-p t))
  (closer-mop:ensure-finalized (find-class 'child2))
  (assert-equal '(or null number)
                (getf (mopu:slot-properties (find-class 'child2) 'age) :type))
  (assert-equal 'string
                (getf (mopu:slot-properties (find-class 'child2) 'name) :type))
  (assert-equal 'string
                (getf (mopu:slot-properties (find-class 'child2) 'bio) :type)))

(define-test accessor-and-class-documentation ()
  (define-class documented-class ()
    ((documented-slot
      nil
      :documentation "A slot with explicit documentation.")
     ;; Documentation autogenerated.
     (undocumented-slot nil)
     (no-writer-slot
      nil
      :writer nil
      :reader t)
     (no-reader-slot
      nil
      :writer t
      :reader nil))
    (:documentation "Documented class."))
  (assert-string= "Documented class."
                  (documentation 'documented-class 'type))
  (assert-string= "Auto-generated accessor function for slot DOCUMENTED-SLOT.
A slot with explicit documentation."
                  (documentation 'documented-slot 'function))
  (assert-string= "Auto-generated accessor function for slot UNDOCUMENTED-SLOT."
                  (documentation 'undocumented-slot 'function))
  (assert-string= "Auto-generated accessor function for slot NO-WRITER-SLOT."
                  (documentation 'no-writer-slot 'function))
  (assert-string= "Auto-generated accessor function for slot NO-READER-SLOT."
                  (documentation (fdefinition '(setf no-reader-slot)) 'function)))

(define-test define-generic-arguments ()
  ;; Simple specialized arglist.
  (assert-expands
   (prog1
       (defgeneric generic (a b)
         (:method ((a a) (b b))
           (+ a b))))
   (define-generic generic ((a a) (b b))
     (+ a b)))
  ;; Keyword argument preservation.
  (assert-expands
   (prog1
       (defgeneric generic (a b &key c)
         (:method ((a a) (b b) &key (c nil))
           (+ a b))))
   (define-generic generic ((a a) (b b) &key (c nil))
     (+ a b)))
  ;; Keyword arguments removal with &allow-other-keys.
  (assert-expands
   (prog1
       (defgeneric generic (a b &key &allow-other-keys)
         (:method ((a a) (b b) &key (c nil) &allow-other-keys)
           (+ a b))))
   (define-generic generic ((a a) (b b) &key (c nil) &allow-other-keys)
     (+ a b)))
  ;; Unspecialized arguments.
  (assert-expands
   (prog1
       (defgeneric generic (a b &key &allow-other-keys)
         ;; This is safe, as methods can have unspecialized arguments.
         (:method (a b &key c &allow-other-keys)
           (+ a b))))
   (define-generic generic (a b &key c &allow-other-keys)
     (+ a b)))
  ;; Warns on specialized args without body.
  (assert-warning 'nclasses::hu.dwim.defclass-star-style-warning
                  (macroexpand-1 '(define-generic generic ((a a) (b b) &key (c nil))
                                   "Documentation"))))

(define-test define-generic-body ()
  ;; Two methods---one implicit, one explicit.
  (assert-expands
   (prog1
       (defgeneric generic (a b &key c)
         (:method ((a a) (b b) &key (c nil))
           (+ a b))
         (:method ((a x) (b z) &key c)
           c)))
   (define-generic generic ((a a) (b b) &key (c nil))
     (+ a b)
     (:method ((a x) (b z) &key c)
       c)))
  ;; Single documentation body.
  (assert-expands
   (prog1
       (defgeneric generic (a b &key c)
         (:documentation "Body consisting of documentation."))
     (setf (documentation 'generic 'function) "Body consisting of documentation.")
     (setf (documentation (fdefinition 'generic) 'function) "Body consisting of documentation."))
   (define-generic generic (a b &key c)
     "Body consisting of documentation."))
  ;; Documentation and implicit method.
  (assert-expands
   (prog1
       (defgeneric generic (a b &key c)
         (:method ((a a) (b b) &key (c nil))
           (+ a b))
         (:documentation "This expands to documentation."))
     (setf (documentation 'generic 'function) "This expands to documentation.")
     (setf (documentation (fdefinition 'generic) 'function) "This expands to documentation."))
   (define-generic generic ((a a) (b b) &key (c nil))
     "This expands to documentation."
     (+ a b)))
  ;; Messed up documentation as method body.
  (assert-expands
   (prog1
       (defgeneric generic (a b &key c)
         (:method ((a a) (b b) &key (c nil))
           (+ a b)
           "This is not recognized as documentation.")))
   (define-generic generic ((a a) (b b) &key (c nil))
     (+ a b)
     "This is not recognized as documentation.")))

(define-test define-generic-regular-options ()
  ;; Combination.
  (assert-expands
   (prog1
       (defgeneric generic (a b &key c)
         (:method-combination progn)))
   (define-generic generic (a b &key c)
     (:method-combination progn)))
  ;; Declaration.
  (assert-expands
   (prog1
       (defgeneric generic (a b &key c)
         (declare (optimize (speed 3)))))
   (define-generic generic (a b &key c)
     (declare (optimize (speed 3)))))
  ;; Explicit documentation.
  (assert-expands
   (prog1
       (defgeneric generic (a b &key c)
         (:documentation "Documentation is set both as :documentation option and `documentation'."))
     (setf (documentation 'generic 'function)
           "Documentation is set both as :documentation option and `documentation'.")
     (setf (documentation (fdefinition 'generic) 'function)
           "Documentation is set both as :documentation option and `documentation'."))
   (define-generic generic (a b &key c)
     (:documentation "Documentation is set both as :documentation option and `documentation'.")))
  ;; Explicit methods.
  (assert-expands
   (prog1
       (defgeneric generic (a b &key c)
         (:method ((a a) (b b) &key (c nil))
           (+ a b))
         (:method ((a x) (b z) &key c)
           c)))
   (define-generic generic (a b &key c)
     (:method ((a a) (b b) &key (c nil))
       (+ a b))
     (:method ((a x) (b z) &key c)
       c))))

(define-test define-generic-special-options ()
  (assert-equal
   `(prog1
        (defgeneric generic
            (a b &key c)
          (:documentation "If :export-generic-name-p is true, export the name."))
      (eval-when (:compile-toplevel :load-toplevel :execute)
        (export 'generic ,(package-name *package*)))
      (setf (documentation 'generic 'function)
            "If :export-generic-name-p is true, export the name.")
      (setf (documentation (fdefinition 'generic) 'function)
            "If :export-generic-name-p is true, export the name."))
   (macroexpand-1 `(define-generic generic (a b &key c)
                     "If :export-generic-name-p is true, export the name."
                     (:export-generic-name-p t))))
  (assert-equal
   `(prog1
        (defgeneric generic
            (a b &key c)
          (:documentation ":setf-method generating (setf GENERIC) declaration."))
      (defgeneric (setf generic)
          (value a b c)
        (:method (value a b c) (declare (ignorable value a b c)) nil))
      (setf (documentation (fdefinition '(setf generic)) 'function)
            ":setf-method generating (setf GENERIC) declaration.")
      (setf (documentation 'generic 'function)
            ":setf-method generating (setf GENERIC) declaration.")
      (setf (documentation (fdefinition 'generic) 'function)
            ":setf-method generating (setf GENERIC) declaration."))
   (macroexpand-1
    '(define-generic generic (a b &key c)
      ":setf-method generating (setf GENERIC) declaration."
      (:setf-method (value a b c)
       (declare (ignorable value a b c))
       nil))))
  (assert-error
   'nclasses::hu.dwim.defclass-star-style-warning
   (macroexpand-1
    '(define-generic (setf generic) (value a b &key c)
      ":setf-method redundant and `style-warn'ed in (setf GENERIC)."
      (:setf-method (value a b c)
       (declare (ignorable value a b c))
       nil)))))


(define-test make-instance-star-backward-compatible ()
  (assert-expands
   (make-instance 'class)
   (make-instance* 'class))
  (assert-expands
   (make-instance 'class :foo 4 :bar 5)
   (make-instance* 'class :foo 4 :bar 5)))

(define-test make-instance-star-shortcuts ()
  (assert-expands
   (apply #'make-instance 'class :a a :b b :c c nil)
   ;; NIL required to disambiguate shortcuts from apply args.
   (make-instance* 'class (a b c) nil))
  (assert-expands
   (make-instance 'class :a a :b b :c c :x x)
   (make-instance* 'class (a b c) :x x))
  (assert-expands
   (apply #'make-instance 'class :a a :b b :c c (when t (list :x x)))
   (make-instance* 'class (a b c) (when t (list :x x))))
  (assert-expands
   (make-instance 'class)
   (make-instance* 'class)))

(define-test make-instance-star-rest ()
  (assert-expands
   (apply #'make-instance 'class :c c :b b (when something (list :a a)))
   (make-instance* 'class :c c :b b (when something (list :a a))))
  (assert-expands
   (apply #'make-instance 'class :b b :c c :x x (when something (list :a a)))
   (make-instance* 'class (b c) :x x (when something (list :a a))))
  (assert-expands
   (apply #'make-instance 'class (when something (list :a a)))
   (make-instance* 'class () (when something (list :a a)))))

(define-test make-instance-ambiguous ()
  (assert-error
   'nclasses::hu.dwim.defclass-star-style-warning
   (macroexpand-1
    '(make-instance* 'class ())))
  (assert-error
   'nclasses::hu.dwim.defclass-star-style-warning
   (macroexpand-1
    '(make-instance* 'class (when something (list :a a)))))
  (handler-bind ((warning #'muffle-warning))
    (assert-expands
     (apply #'make-instance 'class (when something (list :a a)))
     (make-instance* 'class (when something (list :a a))))))
