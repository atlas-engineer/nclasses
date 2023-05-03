;;;; SPDX-FileCopyrightText: dwim.hu & Atlas Engineer LLC
;;;; SPDX-License-Identifier: Public Domain

(in-package :nclasses)

(defmacro make-name-transformer (&rest elements)
  "Return an accessor name transformer.
The unquoted `name' symbol argument is substituted for the slot name.
Class option examples:

  :accessor-name-transformer (make-name-transformer \"FOO-\" name \"-BAR\")

Use the slot name directly:

  :accessor-name-transformer (make-name-transformer name)"
  `(lambda (name definition)
    (declare (ignorable definition))
    (concatenate-symbol ,@(mapcar (lambda (el)
                                    (if (and (symbolp el)
                                             (string= (symbol-name el) "NAME"))
                                        'name
                                        el))
                                  elements))))

;; setting these globally is a very bad idea, because it affects every
;; package that uses defclass-star, and what's even worse, in a
;; load-order dependent way. a recipe for bugs that are hard to debug.
(defvar *accessor-name-package* nil
  "A package, or :slot-name means the home-package of the slot-name symbol and nil means *package*")
(defvar *accessor-name-transformer* 'default-accessor-name-transformer)
(defvar *automatic-accessors-p* t)

(defvar *automatic-predicates-p* t)
(defvar *predicate-name-transformer* 'default-predicate-name-transformer
  "A function that takes the class name and its definition as argument.
Return the name of the predicate function.
The predicate function returns true when the argument is a type of the `name' class.")

(defun default-predicate-name-transformer (name &rest args)
  "Return predicate name that's suffixed with '-p' if NAME already contains a '-',
or just 'p' otherwise.."
  (declare (ignore args))
  (intern (concatenate 'string
                       (symbol-name name)
                       (string (if (position #\- (symbol-name name)) '#:-p '#:p))) ; compatibility with mutable syntax table case
          (symbol-package name)))

(defun always-dashed-predicate-name-transformer (name &rest args)
  "Return predicate name that's always suffixed with '-p'."
  (declare (ignore args))
  (intern (format nil "~a-P" name) (symbol-package name)))

(defun question-mark-predicate-name-transformer (name &rest args)
  "Return predicate name that's always suffixed with '?'."
  (declare (ignore args))
  (intern (format nil "~a?" name) (symbol-package name)))

(defun default-type-inference (initform name definition)
  "Return type of INITFORM.
This is like `type-of' but returns less specialized types for some common
subtypes, e.g.  for \"\" return 'string instead of `(SIMPLE-ARRAY CHARACTER
\(0))'.

Note that in a slot definition, '() is inferred to be a list while NIL is
inferred to be a boolean.

If the slot is found in a finalized superclass, the inferred type is then that
of the superclass.  If some superclass is not finalized, no type inference is
performed.

Non-basic form types are not inferred (returns nil).
Non-basic scalar types are derived to their own type (with `type-of')."
  (declare (ignore name definition))
  (cond
    ((and (consp initform)
          (eq (first initform) 'quote)
          (symbolp (second initform)))
     (if (eq (type-of (second initform)) 'null)
         'list                          ; The empty list.
         'symbol))
    ((and (consp initform)
          (eq (first initform) 'function))
     'function)
    ((and (consp initform)
          (not (eq (first initform) 'quote)))
     ;; Non-basic form.
     nil)
    ;; This is necessary to prevent nil-initform slots to be recognized as
    ;; boolean-typed ones. Nil does not guarantee any type.
    ((null initform)
     nil)
    (t (let* ((type (if (symbolp initform)
                        (handler-case
                            ;; We can get type of externally defined symbol.
                            (type-of (eval initform))
                          (error ()
                            ;; Don't infer type if symbol is not yet defined.
                            nil))
                        (type-of initform))))
         (if type
             (flet ((derive-type (general-type)
                      (when (subtypep type general-type)
                        general-type)))
               (or (some #'derive-type '(string boolean list array hash-table integer
                                         complex number))
                   ;; Only allow objects of the same type by default.
                   ;; We could have returned nil to inhibit the generation of a
                   ;; :type property.
                   type))
             nil)))))

;; these control whether the respective names should be exported from *package* (which is sampled at macroexpand time)
(defvar *export-class-name-p* nil)
(defvar *export-accessor-names-p* nil)
(defvar *export-slot-names-p* nil)
(defvar *export-predicate-name-p* nil)
(defvar *export-generic-name-p* nil)

(defvar *initarg-name-transformer* 'default-initarg-name-transformer)
(defvar *automatic-initargs-p* t)

(defvar *type-inference* 'default-type-inference
  "Function that returns the type of a slot definition (with an initform).
It takes 3 arguments:
- The initform.
- The slot name.
- The rest of the slot definition.")
(defvar *automatic-types-p* nil)

(defvar *slot-definition-transformer* 'default-slot-definition-transformer)

(defun default-slot-definition-transformer (slot-def)
  "Converts illegal (list foo) :type declarations into simple list declarations."
  (let ((name (pop slot-def))
        (type (getf slot-def :type)))
    (when (and type (listp type) (eq (first type) 'list))
      (setf (getf slot-def :type) 'list))
    (push name slot-def)
    slot-def))

(defvar *allowed-slot-definition-properties* '(:documentation :type :reader :writer :allocation :export)
  "Holds a list of keywords that are allowed in slot definitions (:accessor and :initarg are implicitly included).")

;; expand-time temporary dynamic vars
(defvar *accessor-names*)
(defvar *slot-names*)
(defvar *symbols-to-export*)

(define-condition hu.dwim.defclass-star-style-warning (simple-condition style-warning)
  ())

(defun style-warn (datum &rest args)
  (warn 'hu.dwim.defclass-star-style-warning :format-control datum :format-arguments args))

(defun slot-name-package (name)
  (if (packagep *accessor-name-package*)
      *accessor-name-package*
      (case *accessor-name-package*
        (:slot-name (symbol-package name))
        (:default *package*)
        (t *package*))))

(defun default-accessor-name-transformer (name definition)
  "Return an accessor name following the slot name NAME."
  (declare (ignore definition))
  (intern (symbol-name name) (slot-name-package name)))

(defun dwim-accessor-name-transformer (name definition)
  "Return an accessor name using the slot name NAME suffixed with \"-OF\".
If the slot is a boolean not ending with a question mark, it is suffixed with \"-P\" instead.
If the slot ends with a question mark, the name is taken as is."
  ;; This is `hu.dwim.defclass-star:default-accessor-name-transformer'.
  (let* ((type (getf definition :type))
         (package (slot-name-package name))
         (name-string (string name))
         (last-char (elt name-string (1- (length name-string))))
         (ends-with-question-mark? (char= last-char #\?)))
    (cond
      ((and (eq type 'boolean)
            (not ends-with-question-mark?))
       (cond ((char-equal last-char #\p)
              name)
             ;; i like unconditional '-p' postfix. ymmv.
             #+nil((not (find #\- name-string))
                   (concatenate-symbol name '#:p package))
             (t (concatenate-symbol name '#:-p package))))
      (ends-with-question-mark?
       name)
      (t (concatenate-symbol name '#:-of package)))))

(defun question-mark-accessor-name-transformer (name definition)
  "Return an accessor name using the slot name NAME suffixed with \"-OF\".
If the slot is a boolean, it ensures the name is suffixed with \"?\"."
  ;; This is `hu.dwim.defclass-star:dwim-accessor-name-transformer'.
  (let* ((type (getf definition :type))
         (package (slot-name-package name))
         (name-string (string name))
         (last-char (elt name-string (1- (length name-string)))))
    (if (eq type 'boolean)
        (if (char= last-char #\?)
            name
            (concatenate-symbol name "?" package))
        (concatenate-symbol name '#:-of package))))

(defun default-initarg-name-transformer (name definition)
  "Converts the symbol to a keyword with the same name."
  (declare (ignorable definition))
  (concatenate-symbol name #.(symbol-package :asdf)))

(defun slot-type-maybe-inherited (initform slot-name definition superclasses)
  (let* ((superclasses (mapcar (lambda (s) (find-class s nil)) superclasses))
         (all-parents-finalized-p (and superclasses
                                       (every #'identity superclasses)
                                       (every #'closer-mop:class-finalized-p superclasses))))
    (cond
      ((null superclasses)
       (funcall *type-inference* initform slot-name definition))
      ((and all-parents-finalized-p
            (find slot-name (apply #'append (mapcar #'mopu:slot-names superclasses))))
       (let ((parent (find (list slot-name) superclasses
                           :key #'mopu:slot-names :test #'intersection)))
         (when parent
           (getf (mopu:slot-properties parent slot-name) :type))))
      (all-parents-finalized-p
       (funcall *type-inference* initform slot-name definition))
      (t nil))))

(defun process-slot-definition (definition &key superclasses)
  (unless (consp definition)
    (setf definition (list definition)))
  (let ((name (pop definition))
        (initform 'missing)
        (entire-definition definition))
    (assert name)
    (push name *slot-names*)
    (if (oddp (length definition))
        (progn
          (setf initform (pop definition))
          (setf entire-definition definition)
          (when (eq initform :unbound)
            (setf initform 'missing)))
        (setf initform (getf definition :initform nil)))
    (assert (every #'keywordp (loop for el :in definition :by #'cddr
                                    collect el))
            () "Found non-keywords in ~S" definition)
    (destructuring-bind (&key (accessor 'missing) (initarg 'missing)
                              (reader 'missing) (writer 'missing)
                              (export 'missing) (type 'missing)
                              &allow-other-keys)
        definition
      (remf-keywords definition :accessor :reader :writer :initform :initarg :export :type)
      (let ((unknown-keywords (loop for el :in definition :by #'cddr
                                    unless (or (member t *allowed-slot-definition-properties*)
                                               (member el *allowed-slot-definition-properties*))
                                    collect el))
            (slot-name-warning-triggered? nil))
        (when unknown-keywords
          (style-warn "Unexpected properties in slot definition ~S.~%~
                       The unexpected properties are ~S.~%~
                       To avoid this warning (pushnew (or T :your-custom-keyword) hu.dwim.defclass-star:*allowed-slot-definition-properties*)"
                      entire-definition unknown-keywords))
        (flet ((provided-p (value)
                 (and value
                      (not (eq value 'missing))))
               (transform-accessor ()
                 (funcall *accessor-name-transformer* name entire-definition))
               (maybe-warn-for-slot-name ()
                 (unless (or slot-name-warning-triggered?
                             (eq (symbol-package name) *package*))
                   (setf slot-name-warning-triggered? t)
                   #+nil ;; this generates too many warnings which makes it kinda pointless
                   (style-warn "define-class for a slot name ~A while its home package is not *package* (~A). Default generated names will be interned into *package*!"
                               (fully-qualified-symbol-name name) *package*))))
          (prog1
              (funcall *slot-definition-transformer*
                       (append (list name)
                               (unless (eq initform 'missing)
                                 (list :initform initform))
                               (if (and (eq accessor 'missing)
                                        (eq reader 'missing)
                                        (eq writer 'missing))
                                   (when (and *automatic-accessors-p*
                                              (or (eq (symbol-package name) *package*)
                                                  (not (eq *accessor-name-package* :slot-name))
                                                  (and (fboundp name)
                                                       (typep (fboundp name) 'generic-function))))
                                     (maybe-warn-for-slot-name)
                                     (setf accessor (transform-accessor))
                                     (list :accessor accessor))
                                   (let ((transformed-accessor (transform-accessor)))
                                     (append (progn
                                               (when (eq accessor t)
                                                 (setf accessor transformed-accessor))
                                               (when (provided-p accessor)
                                                 (list :accessor accessor)))
                                             (progn
                                               (when (eq reader t)
                                                 (setf reader transformed-accessor))
                                               (when (provided-p reader)
                                                 (list :reader reader)))
                                             (progn
                                               (when (eq writer t)
                                                 (setf writer `(setf ,transformed-accessor)))
                                               (when (provided-p writer)
                                                 (list :writer writer))))))
                               (if (eq initarg 'missing)
                                   (when *automatic-initargs-p*
                                     (list :initarg (funcall *initarg-name-transformer* name entire-definition)))
                                   (when initarg
                                     (list :initarg initarg)))
                               (if (eq type 'missing)
                                   (when *automatic-types-p*
                                     (let ((type (slot-type-maybe-inherited
                                                  initform name entire-definition superclasses)))
                                       (when type
                                         (list :type type))))
                                   (when type
                                     (list :type type)))

                               definition))
            (when (provided-p accessor)
              (pushnew accessor *accessor-names*))
            (when (provided-p reader)
              (pushnew reader *accessor-names*))
            (when (provided-p writer)
              (pushnew (second writer) *accessor-names*))
            (if (not (eq export 'missing))
                (ecase export
                  (:accessor
                   (when accessor
                     (push accessor *symbols-to-export*)))
                  ((:slot :name :slot-name)
                   (push name *symbols-to-export*))
                  ((t)
                   (when (and accessor (not (eq accessor 'missing)))
                     (push accessor *symbols-to-export*))
                   (push name *symbols-to-export*))
                  ((nil)))
                (progn
                  (when *export-accessor-names-p*
                    (when (and accessor (not (eq accessor 'missing)))
                      (push accessor *symbols-to-export*)))
                  (when *export-slot-names-p*
                    (push name *symbols-to-export*))))))))))

(defun extract-options-into-bindings (options)
  (let ((binding-names)
        (binding-values)
        (clean-options))
    (macrolet ((rebinding-table (&rest args)
                 `(case (car option)
                   ,@(loop for (arg-name var-name) :on args :by #'cddr
                           collect `(,arg-name
                                     (assert (= (length option) 2))
                                     (push ',var-name binding-names)
                                     (push (second option) binding-values)))
                   (t (push option clean-options)))))
      (dolist (option options)
        (rebinding-table
         :accessor-name-package *accessor-name-package*
         :accessor-name-transformer *accessor-name-transformer*
         :automatic-accessors-p *automatic-accessors-p*
         :initarg-name-transformer *initarg-name-transformer*
         :automatic-initargs-p *automatic-initargs-p*
         :export-class-name-p *export-class-name-p*
         :export-accessor-names-p *export-accessor-names-p*
         :export-slot-names-p *export-slot-names-p*
         :automatic-predicates-p *automatic-predicates-p*
         :predicate-name-transformer *predicate-name-transformer*
         :export-predicate-name-p *export-predicate-name-p*
         :automatic-types-p *automatic-types-p*
         :type-inference *type-inference*
         :slot-definition-transformer *slot-definition-transformer*)))
    (values binding-names binding-values (nreverse clean-options))))

(defun build-defclass-like-expansion (name supers slots options expansion-builder)
  #+nil ;; this generates warnings where defclass would not, delme eventually?
  (unless (eq (symbol-package name) *package*)
    (style-warn "define-class for ~A while its home package is not *package* (~A)"
                (fully-qualified-symbol-name name) *package*))
  (let ((*accessor-names* nil)
        (*slot-names* nil)
        (*symbols-to-export* nil))
    (multiple-value-bind (binding-names binding-values clean-options)
        (extract-options-into-bindings options)
      (progv binding-names (mapcar #'eval binding-values)
        (let* ((processed-slots (mapcar (lambda (definition)
                                          (process-slot-definition
                                           definition
                                           :superclasses supers))
                                        slots))
               (result (funcall expansion-builder processed-slots clean-options)))
          (if (or *symbols-to-export*
                  *export-class-name-p*
                  (and *automatic-predicates-p*
                       *predicate-name-transformer*))
              `(progn
                 ,result
                 ,@(when (and *automatic-predicates-p*
                              *predicate-name-transformer*)
                     (let ((pred-name (funcall *predicate-name-transformer* name)))
                       `((unless (fboundp ',pred-name)
                           (defun ,pred-name (object)
                             ,(format nil "Auto-generated predicate for whether OBJECT is an instance of ~a."
                                      name)
                             #+sbcl
                             (declare (sb-ext:muffle-conditions style-warning))
                             (typep object ',name)))
                         ;; TODO shouldn't *export-predicate-name-p*
                         ;; get its default from the "e" flag? (def
                         ;; (class e) foo-bar ...) doesn't export
                         ;; FOO-BAR-P
                         ,@(when *export-predicate-name-p*
                             `((eval-when (:compile-toplevel :load-toplevel :execute)
                                 (export ',pred-name
                                         ,(package-name *package*))))))))
                 ,@(when (or *symbols-to-export*
                             *export-class-name-p*)
                     ;; Don't try to export symbols that don't belong to *package*.
                     ;; This can happen when inheriting from a class and
                     ;; overriding some slot.
                     (let ((syms (remove-if (lambda (sym)
                                              (not (eq (symbol-package sym) *package*)))
                                            (append (when *export-class-name-p*
                                                      (list name))
                                                    *symbols-to-export*))))
                       (when syms
                         `((eval-when (:compile-toplevel :load-toplevel :execute)
                             (export ',syms
                                     ,(package-name *package*)))))))
                 ;; Ensure documentation for accessor/reader/writer symbols.
                 ,@(loop for accessor in *accessor-names*
                         for (slot-name . slot-props)
                           = (find-if #'(lambda (s)
                                          (member accessor (list (getf s :reader)
                                                                 (let ((writer (getf s :writer)))
                                                                   (typecase writer
                                                                     ((cons symbol t)
                                                                      (second writer))
                                                                     (symbol writer)))
                                                                 (getf s :accessor))))
                                      processed-slots
                                      :key #'rest)
                         for documentation = (format nil
                                                     "Auto-generated accessor function for slot ~a.~@[
~a~]"
                                                     slot-name (getf slot-props :documentation))
                         when slot-name
                           collect `(setf (documentation (quote ,accessor) 'function) ,documentation)
                           and collect `(ignore-errors
                                         (setf (documentation (fdefinition (quote ,accessor)) 'function)
                                               ,documentation))
                           and collect `(ignore-errors
                                         (setf (documentation (fdefinition '(setf ,accessor)) 'function)
                                               ,documentation)))
                 ,@(let ((documentation (second (first (member :documentation clean-options :key #'first)))))
                     (when documentation
                       `((setf (documentation ',name 'type) ,documentation)
                         ;; Conditions might be non-class types.
                         (ignore-errors
                          (setf (documentation (find-class ',name) 'type) ,documentation)))))
                 (find-class ',name nil))
              result))))))

(defmacro define-class (name supers slots &rest options)
  "Convenience wrapper of `defclass'.

It automates a lot of boilerplate (like exporting all the accessors of a class)
plus adds some new features.



Slot options:

- The initform can be specified as the second value.

- `:accessor', `:reader' and `:writer' can be set to either a name, NIL or T.
  If NIL, the method is not generated.
  If T, the method is generated using the `:accessor-name-transformer' class option value.

- The `:export' option allows to control the slot-name export on a per-slot basis.



New class options (defaults are NIL unless specified):

- `:slot-definition-transformer' (default: `default-slot-definition-transformer').
  A function that takes a SLOT-DEFINITION and returns a new one.

- `:accessor-name-package': The package of the generated accessors.  See
  `:accessor-name-transformer'.  Some special values are accepted:
  - `:slot-name': The package of the slot-name.
  - `:default': The current package.

- `:accessor-name-transformer' (default `default-accessor-name-transformer').

  A function of (SLOT-NAME SLOT-DEFINITION) arguments that returns the accessor
  name (a symbol).  Other transformer: `dwim-accessor-name-transformer'.

- `:automatic-accessors-p': Whether to generate accessor names automatically for
  all slots.

  As a special case, the accessor is not generated if all following conditions are met:
  - `:accessor-name-package' is `:slot-name',
  - slot name package is different from current package,
  - if the `:accessor' slot option is not provided,


- `:initarg-name-transformer' (default: `default-initarg-name-transformer'.)

  A function of (SLOT-NAME SLOT-DEFINITION) arguments that returns the accessor
  name (a symbol.

- `:automatic-initargs-p' (default: T): Whether to generate accessor names
  automatically for all slots.

- `:predicate-name-transformer' (default: `default-predicate-name-transformer').

  A function of (SLOT-NAME SLOT-DEFINITION) arguments that returns the predicate
  name (a symbol).

- `:automatic-predicates-p' (default: T): Whether to generate a predicate
  for the class.

- `:type-inference' (default: `default-type-inference').

  Function that returns the type of a slot definition (with an initform).  It
  takes 3 arguments:

  - The initform.
  - The slot name.
  - The rest of the slot definition.

- `:automatic-types-p': Whether to generate the type automatically for all
  slots.  The type can still be specified manually with the `:type' slot option.

- Self-explanatory export options (all NIL by default):
  - `:export-class-name-p'
  - `:export-accessor-names-p'
  - `:export-slot-names-p'
  - `:export-predicate-name'"
  (build-defclass-like-expansion
   name supers slots options
   (lambda (processed-slots clean-options)
     `(defclass ,name ,supers
        ,processed-slots
        ,@clean-options))))

(setf (macro-function 'defclass*) (macro-function 'define-class)
      (documentation 'defclass* 'function) (documentation 'define-class 'function)
      (macro-function 'define-class*) (macro-function 'define-class)
      (documentation 'define-class* 'function) (documentation 'define-class 'function))

(defmacro define-condition* (name supers slots &rest options)
  "To `define-condition' what `define-class' is to `defclass'.
See `define-class' for more details."
  (build-defclass-like-expansion
   name supers slots options
   (lambda (processed-slots clean-options)
     `(define-condition ,name ,supers
        ,processed-slots
        ,@clean-options))))

(setf (macro-function 'defcondition*) (macro-function 'define-condition*)
      (documentation 'defcondition* 'function) (documentation 'define-condition* 'function))

(defun generalize-arglist (arglist)
  "Generalizes ARGLIST to be accepted by `defgeneric' (3.4.2 of CLCS/CLHS).
Removes all the default forms or specializers and removes all keyword
arguments if &allow-other-keys is present."
  ;; FIXME: Is removing all the list-like args safe as generic
  ;; function arglist?
  (let ((arglist (mapcar (lambda (a)
                           (if (listp a)
                               (first a)
                               a))
                         arglist))
        ;; FIXME: Is it actually necessary to remove keyword args? May
        ;; be useful for debugging/eldoc... Eldoc/SWANK seems to
        ;; collect keys across methods anyway.
        (allow-other-keys (member '&allow-other-keys arglist)))
    (if allow-other-keys
        (let ((key-position (position '&key arglist)))
          (setf (cdr (nthcdr key-position arglist))
                allow-other-keys)
          arglist)
        arglist)))

(defmacro define-generic (name (&rest arglist) &body body)
  "Convenience macro to define generics as if they were methods.
ARGLIST can be a method arglist, in which case it's parsed into
generic function arglist by removing method-specific args.

BODY can be a list of `defgeneric' options (processed as-is, even
:method ones) intermixed with other Lisp forms:
- If there's a docstring as the first form, it's processed into
  :documentation option of `defgeneric'.
- If there are non-option forms, they are put together into a separate
  :method option under ARGLIST.
- If there's a `declare' form, it's put as a `declare' option of
  `defgeneric'. This may seem irregular if the method body also needs
  declarations, but in such a case one's better off with a :method
  option or `defgeneric' for the method with `declare'.

Additional, `define-generic'-specific options:
- `:export-generic-name-p' boolean option --- export (T) or don't
  export (NIL) the generic symbol.
- `:setf-method' --- same as `:method', but put into a (setf NAME)
  defgeneric (generated when necessary). Same as :method when already
  in (setf NAME), although meaningless and `style-warn'ed.

Example:

\(define-generic add ((a integer) (b integer) &key coerce-to-fixnum &allow-other-keys)
  \"Adds A and B, coercing them to fixnum if the sum is too big.\"
  (if coerce-to-fixnum
      (coerce (+ a b) 'fixnum)
      (+ a b))
  (:method ((a string) (b integer))
   (error \"Cannot use `add' on strings!\")))
=>
\(defgeneric add (a b &key &allow-other-keys)
  (:method ((a integer) (b integer) &key coerce-to-fixnum &allow-other-keys)
   (if coerce-to-fixnum
      (coerce (+ a b) 'fixnum)
      (+ a b)))
  (:method ((a string) (b integer))
   (error \"Cannot use `add' on strings!\"))
  (:documentation \"Adds A and B, coercing them to fixnum if the sum is too big.\"))"
  (let* ((documentation (if (stringp (first body))
                            (first body)
                            nil))
         (declarations (remove-if-not (lambda (form)
                                        (and (listp form) (eq 'declare (first form))))
                                      body))
         (forms (if documentation
                    (rest body)
                    body))
         ;; NOTE: `set-difference' can shuffle the order of elements, thus `remove-if'.
         (forms (remove-if (lambda (f) (member f declarations :test #'equal)) forms))
         (options (remove-if-not (lambda (form)
                                   (keywordp (first (uiop:ensure-list form))))
                                 forms))
         ;; NOTE: `set-difference' can shuffle the order of elements, thus `remove-if'.
         (method-body (remove-if (lambda (f) (member f options :test #'equal)) forms))
         (export-generic-name-p (find :export-generic-name-p options :key #'first))
         (options (remove export-generic-name-p options :test #'equal))
         (setf-methods (remove-if (lambda (o) (not (eq :setf-method (first o)))) options))
         (options (loop for option in options
                        unless (member option setf-methods)
                          collect option))
         (setf-generic-p (and (listp name) (eq 'setf (first name))))
         (export-p (if export-generic-name-p
                       (second export-generic-name-p)
                       *export-generic-name-p*))
         (generalized-arglist (generalize-arglist arglist)))
    (when (and (not (equal generalized-arglist arglist))
               (every #'null (list method-body options)))
      (style-warn "Specialized arglist used without method body in define-generic: ~a" arglist))
    `(prog1
         (defgeneric ,name ,generalized-arglist
           ,@declarations
           ,@(when method-body
               `((:method ,arglist
                   ,@method-body)))
           ,@options
           ,@(when (and setf-methods setf-generic-p)
               (loop for setf-method in setf-methods
                     do (style-warn ":setf-method ~a is meaningless in setf generic, turning into :method"
                                    setf-method)
                     collect `(:method ,@(rest setf-method))))
           ,@(when documentation
               `((:documentation ,documentation))))
       ,@(when (and setf-methods (not setf-generic-p))
           `((defgeneric (setf ,name) ,(generalize-arglist (second (first setf-methods)))
               ,@(loop for setf-method in setf-methods
                       collect `(:method ,@(rest setf-method))))))
       ;; FIXME: Maybe only export if package of generic function name
       ;; matches *PACKAGE*?
       ,@(when export-p
           `((eval-when (:compile-toplevel :load-toplevel :execute)
               (export ',name ,(package-name *package*)))))
       ,@(alexandria:when-let ((documentation
                                (or documentation
                                    (second (first (member :documentation options :key #'first))))))
           (append
            (when (or setf-generic-p setf-methods)
              `((setf (documentation (fdefinition '(setf ,name)) 'function) ,documentation)))
            (unless setf-generic-p
              `((setf (documentation ',name 'function) ,documentation)
                (setf (documentation (fdefinition ',name) 'function) ,documentation))))))))

(setf (macro-function 'defgeneric*) (macro-function 'define-generic)
      (documentation 'defgeneric* 'function) (documentation 'define-generic 'function)
      (macro-function 'define-generic*) (macro-function 'define-generic)
      (documentation 'define-generic* 'function) (documentation 'define-generic 'function))

(defmacro make-instance* (class &rest arguments)
  "Convenience macro on top of `make-instance'.

Conveniences:

- First argument can be a (possibly empty) list of shortcut symbols,
  which is automatically converted into a KEYWORD+SYMBOL pairs where
  - KEYWORD is the same as shortcut name, but interned as keyword.
  - And SYMBOL is the same as shortcut.

- The last argument can be an apply argument---arbitrary
  list-producing form, which will be used as the last argument
  to (apply #'make-instance ...) idiom.

Note that using a singular shortcut list of apply argument is an
ambiguous case that will be interpreted in favor of apply argument. To
force either of interpretations, provide shortcuts and apply arguments
explicitly:
(make-instance* 'class (a b c)) ;; BAD
(make-instance* 'class (a b c) nil) ;; GOOD
(make-instance* 'class (when t nil)) ;; BAD
(make-instance* 'class () (when t nil)) ;; GOOD

Otherwise, the behavior is equivalent to `make-instance'.

Examples:

Shortcut args:
\(make-instance* 'class (a b c))
=>
\(make-instance 'class :a a :b b :c c)

Last form converted into `apply' argument:
\(make-instance* 'class :c c :b b (when something (list :a a)))
=>
\(apply #'make-instance 'class :c c :b b (when something (list :a a)))

Shortcut arguments, regular arguments, and form for `apply':
\(make-instance* 'class (b c) :x x (when something (list :a a)))
=>
\(apply #'make-instance 'class :b b :c c :x x (when something (list :a a)))

Form for `apply' (notice the empty shortcuts---these are required for
unambiguous parsing):
\(make-instance* 'class () (when something (list :a a)))
=>
\(apply #'make-instance 'class (when something (list :a a)))

Regular `make-instance'-like use:
\(make-instance* 'class :a a-value :b b-value)
=>
\(make-instance 'class :a a-value :b b-value)"
  (when (and (= 1 (length arguments))
             (listp (first arguments)))
    (style-warn "Body of MAKE-INSTANCE* contains a single list-like form ~a.

If you meant APPLY args, add empty shortcuts list:
(make-instance* 'class () (apply-form))

If you meant shortcut list, add empty APPLY args:
(make-instance* 'class (shortcuts) nil)

Interpreting as APPLY args for now, but this behavior may change in
the future." arguments))
  (let* ((shortcuts-p (and (listp (first arguments))
                           (> (length arguments) 1)))
         (shortcuts (when shortcuts-p
                      (first arguments)))
         (arguments (if shortcuts-p
                        (rest arguments)
                        arguments))
         (last-appendable-form-p (oddp (length arguments)))
         (last-appendable-form (when last-appendable-form-p
                                 (first (last arguments))))
         (arguments (if last-appendable-form-p
                        (butlast arguments)
                        arguments)))
    `(,@(if last-appendable-form-p
            `(apply #'make-instance)
            `(make-instance))
      ,class
      ,@(loop for shortcut in shortcuts
              append (list (intern (symbol-name shortcut) :keyword)
                           shortcut))
      ,@arguments
      ,@(when last-appendable-form-p
          (list last-appendable-form)))))

(setf (macro-function 'make*) (macro-function 'make-instance*)
      (documentation 'make* 'function) (documentation 'make-instance* 'function))
