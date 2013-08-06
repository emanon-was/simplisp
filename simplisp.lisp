(cl:defpackage #:simplisp
  (:nicknames #:sl)
  (:use #:cl))
(cl:in-package #:simplisp)

(export '*load-paths*)
(defparameter *load-paths* '("./" "../" "~/" "~/.lisp/"))

(defparameter *main-file* "__main__.lisp")
(defparameter *test-file* "__test__.lisp")
(defparameter *ignore-files* (list *main-file* *test-file*))

(export '*extension*)
(defparameter *extension* "lisp")

(export 'add-load-paths)
(defun add-load-paths (&rest load-paths)
  (nconc *load-paths* load-paths))



(cl:defpackage #:simplisp.test
  (:nicknames #:sl.test)
  (:use #:cl)
  (:export #:*do-tests-when-defined* #:*test* #:continue-testing
           #:deftest #:do-test #:do-tests #:get-test #:pending-tests
           #:rem-all-tests #:rem-test))
(cl:in-package #:simplisp.test)

;;
;; RT Start
;;

(declaim (ftype (function (t) t) get-entry expanded-eval do-entries))
(declaim (type list *entries*))
(declaim (ftype (function (t &rest t) t) report-error))
(declaim (ftype (function (t &optional t) t) do-entry))

(defvar *test* nil "Current test name")
(defvar *do-tests-when-defined* nil)
(defvar *entries* '(nil) "Test database.  Has a leading dummy cell that does not contain an entry.")
(defvar *entries-tail* *entries* "Tail of the *entries* list")
(defvar *entries-table* (make-hash-table :test #'equal)
    "Map the names of entries to the cons cell in *entries* that precedes the one whose car is the entry.")
(defvar *in-test* nil "Used by TEST")
(defvar *debug* nil "For debugging")
(defvar *catch-errors* t "When true, causes errors in a test to be caught.")
(defvar *print-circle-on-failure* nil
  "Failure reports are printed with *PRINT-CIRCLE* bound to this value.")

(defvar *compile-tests* nil "When true, compile the tests before running them.")
(defvar *expanded-eval* nil "When true, convert the tests into a form that is less likely to have compiler optimizations.")
(defvar *optimization-settings* '((safety 3)))

(defvar *expected-failures* nil
  "A list of test names that are expected to fail.")

(defvar *notes* (make-hash-table :test 'equal)
  "A mapping from names of notes to note objects.")

(defstruct (entry (:conc-name nil))
  pend name props form vals)

;;; Note objects are used to attach information to tests.
;;; A typical use is to mark tests that depend on a particular
;;; part of a set of requirements, or a particular interpretation
;;; of the requirements.

(defstruct note
  name
  contents
  disabled ;; When true, tests with this note are considered inactive
  )

;; (defmacro vals (entry) `(cdddr ,entry))

(defmacro defn (entry)
  (let ((var (gensym)))
    `(let ((,var ,entry))
       (list* (name ,var) (form ,var) (vals ,var)))))

(defun entry-notes (entry)
  (let* ((props (props entry))
         (notes (getf props :notes)))
    (if (listp notes)
        notes
      (list notes))))

(defun has-disabled-note (entry)
  (let ((notes (entry-notes entry)))
    (loop for n in notes
          for note = (if (note-p n) n
                       (gethash n *notes*))
          thereis (and note (note-disabled note)))))

(defun pending-tests ()
  (loop for entry in (cdr *entries*)
        when (and (pend entry) (not (has-disabled-note entry)))
        collect (name entry)))

(defun rem-all-tests ()
  (setq *entries* (list nil))
  (setq *entries-tail* *entries*)
  (clrhash *entries-table*)
  nil)

(defun rem-test (&optional (name *test*))
  (let ((pred (gethash name *entries-table*)))
    (when pred
      (if (null (cddr pred))
          (setq *entries-tail* pred)
        (setf (gethash (name (caddr pred)) *entries-table*) pred))
      (setf (cdr pred) (cddr pred))
      (remhash name *entries-table*)
      name)))

(defun get-test (&optional (name *test*))
  (defn (get-entry name)))

(defun get-entry (name)
  (let ((entry ;; (find name (the list (cdr *entries*))
               ;;     :key #'name :test #'equal)
         (cadr (gethash name *entries-table*))
         ))
    (when (null entry)
      (report-error t
        "~%No test with name ~:@(~S~)."
        name))
    entry))

(defmacro deftest (name &rest body)
  (let* ((p body)
         (properties
          (loop while (keywordp (first p))
                unless (cadr p)
                do (error "Poorly formed deftest: ~A~%"
                          (list* 'deftest name body))
                append (list (pop p) (pop p))))
         (form (pop p))
         (vals p))
    `(add-entry (make-entry :pend t
                            :name ',name
                            :props ',properties
                            :form ',form
                            :vals ',vals))))

(defun add-entry (entry)
  (setq entry (copy-entry entry))
  (let* ((pred (gethash (name entry) *entries-table*)))
    (cond
     (pred
      (setf (cadr pred) entry)
      (report-error nil
        "Redefining test ~:@(~S~)"
        (name entry)))
     (t
      (setf (gethash (name entry) *entries-table*) *entries-tail*)
      (setf (cdr *entries-tail*) (cons entry nil))
      (setf *entries-tail* (cdr *entries-tail*))
      )))
  (when *do-tests-when-defined*
    (do-entry entry))
  (setq *test* (name entry)))

(defun report-error (error? &rest args)
  (cond (*debug*
         (apply #'format t args)
         (if error? (throw '*debug* nil)))
        (error? (apply #'error args))
        (t (apply #'warn args)))
  nil)

(defun do-test (&optional (name *test*))
  #-sbcl (do-entry (get-entry name))
  #+sbcl (handler-bind ((sb-ext:code-deletion-note #'muffle-warning))
                       (do-entry (get-entry name))))

(defun my-aref (a &rest args)
  (apply #'aref a args))

(defun my-row-major-aref (a index)
  (row-major-aref a index))

(defun equalp-with-case (x y)
  "Like EQUALP, but doesn't do case conversion of characters.
   Currently doesn't work on arrays of dimension > 2."
  (cond
   ((eq x y) t)
   ((consp x)
    (and (consp y)
         (equalp-with-case (car x) (car y))
         (equalp-with-case (cdr x) (cdr y))))
   ((and (typep x 'array)
         (= (array-rank x) 0))
    (equalp-with-case (my-aref x) (my-aref y)))
   ((typep x 'vector)
    (and (typep y 'vector)
         (let ((x-len (length x))
               (y-len (length y)))
           (and (eql x-len y-len)
                (loop
                 for i from 0 below x-len
                 for e1 = (my-aref x i)
                 for e2 = (my-aref y i)
                 always (equalp-with-case e1 e2))))))
   ((and (typep x 'array)
         (typep y 'array)
         (not (equal (array-dimensions x)
                     (array-dimensions y))))
    nil)

   ((typep x 'array)
    (and (typep y 'array)
         (let ((size (array-total-size x)))
           (loop for i from 0 below size
                 always (equalp-with-case (my-row-major-aref x i)
                                          (my-row-major-aref y i))))))
   ((and (typep x 'pathname)
         (typep y 'pathname))
    (pathname-match-p x y))

   (t (eql x y))))

(defun do-entry (entry &optional
                       (s *standard-output*))
  (catch '*in-test*
    (setq *test* (name entry))
    (setf (pend entry) t)
    (let* ((*in-test* t)
           ;; (*break-on-warnings* t)
           (aborted nil)
           r)
      ;; (declare (special *break-on-warnings*))

      (block aborted
        (setf r
              (flet ((%do
                      ()
                      (cond
                       (*compile-tests*
                        (multiple-value-list
                         (funcall (compile
                                   nil
                                   `(lambda ()
                                      (declare
                                       (optimize ,@*optimization-settings*))
                                      ,(form entry))))))
                       (*expanded-eval*
                        (multiple-value-list
                         (expanded-eval (form entry))))
                       (t
                        (multiple-value-list
                         (eval (form entry)))))))
                (if *catch-errors*
                    (handler-bind
                     (#-ecl (style-warning #'muffle-warning)
                            (error #'(lambda (c)
                                       (setf aborted t)
                                       (setf r (list c))
                                       (return-from aborted nil))))
                     (%do))
                  (%do)))))

      (setf (pend entry)
            (or aborted
                (not (equalp-with-case r (vals entry)))))

      (when (pend entry)
        (let ((*print-circle* *print-circle-on-failure*))
          (format s "~&Test ~:@(~S~) failed~
                   ~%Form: ~S~
                   ~%Expected value~P: ~
                      ~{~S~^~%~17t~}~%"
                  *test* (form entry)
                  (length (vals entry))
                  (vals entry))
          (handler-case
           (let ((st (format nil "Actual value~P: ~
                      ~{~S~^~%~15t~}.~%"
                            (length r) r)))
             (format s "~A" st))
           (error () (format s "Actual value: #<error during printing>~%")
                  ))
          (finish-output s)
          ))))
  (when (not (pend entry)) *test*))

(defun expanded-eval (form)
  "Split off top level of a form and eval separately.  This reduces the chance that
   compiler optimizations will fold away runtime computation."
  (if (not (consp form))
      (eval form)
   (let ((op (car form)))
     (cond
      ((eq op 'let)
       (let* ((bindings (loop for b in (cadr form)
                              collect (if (consp b) b (list b nil))))
              (vars (mapcar #'car bindings))
              (binding-forms (mapcar #'cadr bindings)))
         (apply
          (the function
            (eval `(lambda ,vars ,@(cddr form))))
          (mapcar #'eval binding-forms))))
      ((and (eq op 'let*) (cadr form))
       (let* ((bindings (loop for b in (cadr form)
                              collect (if (consp b) b (list b nil))))
              (vars (mapcar #'car bindings))
              (binding-forms (mapcar #'cadr bindings)))
         (funcall
          (the function
            (eval `(lambda (,(car vars) &aux ,@(cdr bindings)) ,@(cddr form))))
          (eval (car binding-forms)))))
      ((eq op 'progn)
       (loop for e on (cdr form)
             do (if (null (cdr e)) (return (eval (car e)))
                  (eval (car e)))))
      ((and (symbolp op) (fboundp op)
            (not (macro-function op))
            (not (special-operator-p op)))
       (apply (symbol-function op)
              (mapcar #'eval (cdr form))))
      (t (eval form))))))

(defun continue-testing ()
  (if *in-test*
      (throw '*in-test* nil)
      (do-entries *standard-output*)))

(defun do-tests (&optional
                 (out *standard-output*))
  (dolist (entry (cdr *entries*))
    (setf (pend entry) t))
  (if (streamp out)
      (do-entries out)
      (with-open-file
          (stream out :direction :output)
        (do-entries stream))))

(defun do-entries* (s)
  (format s "~&Doing ~A pending test~:P ~
             of ~A tests total.~%"
          (count t (the list (cdr *entries*)) :key #'pend)
          (length (cdr *entries*)))
  (finish-output s)
  (dolist (entry (cdr *entries*))
    (when (and (pend entry)
               (not (has-disabled-note entry)))
      (format s "~@[~<~%~:; ~:@(~S~)~>~]"
              (do-entry entry s))
      (finish-output s)
      ))
  (let ((pending (pending-tests))
        (expected-table (make-hash-table :test #'equal)))
    (dolist (ex *expected-failures*)
      (setf (gethash ex expected-table) t))
    (let ((new-failures
           (loop for pend in pending
                 unless (gethash pend expected-table)
                 collect pend)))
      (if (null pending)
          (format s "~&No tests failed.")
        (progn
          (format s "~&~A out of ~A ~
                   total tests failed: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
                  (length pending)
                  (length (cdr *entries*))
                  pending)
          (if (null new-failures)
              (format s "~&No unexpected failures.")
            (when *expected-failures*
              (format s "~&~A unexpected failures: ~
                   ~:@(~{~<~%   ~1:;~S~>~
                         ~^, ~}~)."
                    (length new-failures)
                    new-failures)))
          ))
      (finish-output s)
      (null pending))))

(defun do-entries (s)
  #-sbcl (do-entries* s)
  #+sbcl (handler-bind ((sb-ext:code-deletion-note #'muffle-warning))
                       (do-entries* s)))

;;; Note handling functions and macros

(defmacro defnote (name contents &optional disabled)
  `(eval-when (:load-toplevel :execute)
     (let ((note (make-note :name ',name
                            :contents ',contents
                            :disabled ',disabled)))
       (setf (gethash (note-name note) *notes*) note)
       note)))

(defun disable-note (n)
  (let ((note (if (note-p n) n
                (setf n (gethash n *notes*)))))
    (unless note (error "~A is not a note or note name." n))
    (setf (note-disabled note) t)
    note))

(defun enable-note (n)
  (let ((note (if (note-p n) n
                (setf n (gethash n *notes*)))))
    (unless note (error "~A is not a note or note name." n))
    (setf (note-disabled note) nil)
    note))

;;
;; RT End
;;



(cl:defpackage #:simplisp.utils
  (:nicknames #:sl.utils)
  (:use #:cl))
(cl:in-package #:simplisp.utils)

;;===================
;; Macro
;;=================== 

;; CL-USER> (performance 1000 (+ 1 2 3))
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000119 seconds of total run time (0.000024 user, 0.000095 system)
;;   100.00% CPU
;;   2,232 processor cycles
;;   0 bytes consed
(export 'performance)
(defmacro performance (times &body body)
  `(time (dotimes (x ,times) ,@body)))

(export 'with-gensyms)
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

;;===================
;; List
;;=================== 

;; CL-USER> (last1 '(1 2 3 4 5))
;; 5
(export 'last1)
(defun last1 (lst)
  (car (last lst)))

;; CL-USER> (single '(1))
;; 1
(export 'single)
(defun single (lst)
  (and (consp lst) (null (cdr lst)) (car lst)))

;; CL-USER> (filter #'(lambda (x) (if (oddp x) x)) '(1 2 3 4 5))
;; (1 3 5)
(export 'filter)
(defun filter (func lst)
  (let ((acc nil))
    (dolist (tmp lst (nreverse acc))
      (let ((val (funcall func tmp)))
        (if val (push val acc))))))

;;===================
;; String
;;=================== 

;; CL-USER> (string-split "," "abc,def,ghi,jkl")
;; ("abc" "def" "ghi" "jkl")
;; CL-USER> (string-split #\, "abc,def,ghi,jkl")
;; ("abc" "def" "ghi" "jkl")
(export 'string-split)
(defun string-split (separator string &key (test #'char=))
  (let* ((sep (if (stringp separator) separator (princ-to-string separator)))
         (str (if (stringp string) string (princ-to-string string)))
         (sep-length (if (stringp separator) (length separator) 1)))
    (do* ((start 0 (+ end sep-length))
          (end (search sep str :start2 start :test test)
               (search sep str :start2 start :test test))
          (acc (cons (subseq str start end) nil)
               (cons (subseq str start end) acc)))
         ((null end) (nreverse acc)))))

;; CL-USER> (string-join "," '("abc" "def" "ghi" "jkl"))
;; "abc,def,ghi,jkl"
;; CL-USER> (string-join #\, '("abc" "def" "ghi" "jkl"))
;; "abc,def,ghi,jkl"
(export 'string-join)
(defun string-join (delimiter list)
  (if (listp list)
      (let ((del (if (stringp delimiter) delimiter (princ-to-string delimiter))))
        (format nil (concatenate 'string "~{~a~^" del "~}") list))))

;; CL-USER> (string-gsub "e" "0" "sleep")
;; "sl00p"
;; CL-USER> (string-gsub #\e #\0 "sleep")
;; "sl00p"
(export 'string-gsub)
(defun string-gsub (match replacement string &key (test #'char=))
  (string-join replacement (string-split match string :test test)))

;; CL-USER> (string+ "abc" "def" "efg")
;; "abcdefefg"
(export 'string+)
(defun string+ (&rest other-args)
  (string-join "" other-args))

;; CL-USER> (namestring+ #P"/home/user/" "test" "." "lisp")
;; "/home/user/test.lisp"
(export 'namestring+)
(defun namestring+ (&rest other-args)
  (string-join "" (mapcar #'(lambda (x) (cond ((null x) "")
                                              ((stringp x) x)
                                              (t (namestring x)))) other-args)))

;;===================
;; Path
;;=================== 

;;
;; CL-FAD Start
;;

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(export 'directory-pathname-p)
(defun directory-pathname-p (pathspec)
  (if pathspec
      (and
       (not (component-present-p (pathname-name pathspec)))
       (not (component-present-p (pathname-type pathspec)))
       pathspec)))

(export 'pathname-as-directory)
(defun pathname-as-directory (pathspec)
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((not (directory-pathname-p pathspec))
           (make-pathname :directory (append (or (pathname-directory pathname)
                                                 (list :relative))
                                             (list (file-namestring pathname)))
                          :name nil
                          :type nil
                          :defaults pathname))
          (t pathname))))

(defun directory-wildcard (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only make wildcard directories from non-wildcard directories."))
  (make-pathname :name #-:cormanlisp :wild #+:cormanlisp "*"
                 :type #-(or :clisp :cormanlisp) :wild
                       #+:clisp nil
                       #+:cormanlisp "*"
                 :defaults (pathname-as-directory dirname)))

#+:clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname :directory (append (pathname-directory wildcard)
                                    (list :wild))
                 :name nil
                 :type nil
                 :defaults wildcard))

(export 'list-directory)
(defun list-directory (dirname &key (follow-symlinks t))
  (declare (ignorable follow-symlinks))
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  #+:ecl
  (let ((dir (pathname-as-directory dirname)))
    (concatenate 'list
                 (directory (merge-pathnames (pathname "*/") dir))
                 (directory (merge-pathnames (pathname "*.*") dir))))
  #-:ecl
  (let ((wildcard (directory-wildcard dirname)))
    #+:abcl
    (system::list-directory dirname)
    #+:sbcl
    (directory wildcard :resolve-symlinks follow-symlinks)
    #+(or :cmu :scl :lispworks)
    (directory wildcard)
    #+(or :openmcl :digitool)
    (directory wildcard :directories t :follow-links follow-symlinks)
    #+:allegro
    (directory wildcard :directories-are-files nil)
    #+:clisp
    (nconc (directory wildcard :if-does-not-exist :keep)
           (directory (clisp-subdirectories-wildcard wildcard)))
    #+:cormanlisp
    (nconc (directory wildcard)
           (cl::directory-subdirs dirname)))
  #-(or :sbcl :cmu :scl :lispworks :openmcl :allegro :clisp :cormanlisp :ecl :abcl :digitool)
  (error "LIST-DIRECTORY not implemented"))

(defun pathname-as-file (pathspec)
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((directory-pathname-p pathspec)
           (let* ((directory (pathname-directory pathname))
                  (name-and-type (pathname (first (last directory)))))
             (make-pathname :directory (butlast directory)
                            :name (pathname-name name-and-type)
                            :type (pathname-type name-and-type)
                            :defaults pathname)))
          (t pathname))))

(defun file-exists-p (pathspec)
  #+(or :sbcl :lispworks :openmcl :ecl :digitool)
  (probe-file pathspec)
  #+:allegro
  (or (excl:probe-directory (pathname-as-directory pathspec))
      (probe-file pathspec))
  #+(or :cmu :scl :abcl)
  (or (probe-file (pathname-as-directory pathspec))
      (probe-file pathspec))
  #+:cormanlisp
  (or (and (ccl:directory-p pathspec)
           (pathname-as-directory pathspec))
      (probe-file pathspec))
  #+:clisp
  (or (ignore-errors
        (let ((directory-form (pathname-as-directory pathspec)))
          (when (ext:probe-directory directory-form)
            directory-form)))
      (ignore-errors
        (probe-file (pathname-as-file pathspec))))
  #-(or :sbcl :cmu :scl :lispworks :openmcl :allegro :clisp :cormanlisp :ecl :abcl :digitool)
  (error "FILE-EXISTS-P not implemented"))

;;
;; CL-FAD End
;;

;; CL-USER> (pathname-exist-p #P"~")
;; #P"/home/user/"
(export 'pathname-exist-p)
(defun pathname-exist-p (pathspec)
  (let ((pathname (file-exists-p pathspec)))
    (if pathname
        (car (directory pathname)))))

;; CL-USER> (directory-exist-p #P"~")
;; #P"/home/user/"
;; CL-USER> (directory-exist-p #P"~/test.lisp")
;; NIL
(export 'directory-exist-p)
(defun directory-exist-p (pathspec)
  (if pathspec
      (let ((pathname (pathname-exist-p pathspec)))
        (if (directory-pathname-p pathname)
            pathname))))

;; CL-USER> (file-exist-p #P"/home/user/")
;; NIL
;; CL-USER> (file-exist-p #P"/home/user/test.lisp")
;; #P"/home/test/test.lisp"
(export 'file-exist-p)
(defun file-exist-p (pathspec)
  (if pathspec
      (let ((pathname (pathname-exist-p pathspec)))
        (if (null (directory-pathname-p pathname))
            pathname))))

;; CL-USER> (dirname #P"/home/user/")
;; #P"/home/"
(export 'dirname)
(defun dirname (pathspec)
  (let ((pathname (pathname-exist-p pathspec)))
    (if pathname
        (if (and (directory-pathname-p pathname)
                 (null (equal (pathname-directory pathname)
                              '(:ABSOLUTE))))
            (pathname-exist-p (make-pathname :directory (append (pathname-directory pathname) '(:UP))))
            (pathname-exist-p (make-pathname :directory (pathname-directory pathname)))))))

;; CL-USER> (basename #P"/home/user/")
;; "user"
;; CL-USER> (basename #P"/home/user/test.lisp")
;; "test.lisp"
(export 'basename)
(defun basename (pathspec)
  (let ((pathname (pathname-exist-p pathspec)))
    (if pathname
        (if (directory-pathname-p pathname)
            (car (last (pathname-directory pathname)))
            (file-namestring pathname)))))

(cl:in-package #:simplisp)
(cl:use-package :simplisp.utils)
(cl:use-package :simplisp.test)

;;===================
;; SIMPLISP CLASS
;;=================== 

(defclass <simplisp-option> ()
  ((load-paths
    :accessor load-paths
    :initform *load-paths*)
   (main-file
    :accessor main-file
    :initform *main-file*)
   (extension
    :accessor extension
    :initform *extension*)
   (expand-load-paths
    :accessor expand-load-paths
    :initform (let ((acc nil))
                (dolist (tmp *load-paths* (nreverse acc))
                  (dolist (tmp (directory tmp) acc)
                    (let ((x (directory-exist-p tmp)))
                      (if x (push x acc)))))))))

(defclass <simplisp> (<simplisp-option>)
  ((keyword
    :accessor simplisp-keyword
    :initarg :keyword
    :initform (error "Must supply simplisp-keyword (:keyword)"))
   (symbol
    :accessor simplisp-symbol
    :initarg :symbol
    :initform nil)
   (where
    :accessor simplisp-where
    :initarg :where
    :initform nil)
   (prefix
    :accessor simplisp-prefix
    :initarg :prefix
    :initform nil)
   (type
    :accessor simplisp-type
    :initarg :type
    :initform nil)))

(defmethod simplisp-type-tree-p ((simplisp <simplisp-option>) pathspec)
  (with-accessors ((main-file main-file)) simplisp
    (let ((pathname (directory-exist-p pathspec)))
      (and pathname
           (file-exist-p (namestring+ pathname "/" main-file))
           pathname))))

(defmethod simplisp-type-module-p ((simplisp <simplisp-option>) pathspec)
  (with-accessors ((main-file main-file)
                   (extension extension)) simplisp
    (let ((pathname (file-exist-p pathspec)))
      (and pathname
           (equal (pathname-type pathname) extension)
           (null (member (basename pathname) *ignore-files* :test #'equal))
           (simplisp-type-tree-p simplisp (dirname pathname))
           pathname))))

(defmethod simplisp-search ((simplisp <simplisp>))
  (with-accessors ((main-file main-file)
                   (extension extension)
                   (expand-load-paths expand-load-paths)
                   (keyword simplisp-keyword)
                   (symbol simplisp-symbol)
                   (where simplisp-where)
                   (prefix simplisp-prefix)
                   (type simplisp-type)) simplisp
    (let ((key (string-gsub #\. #\/ (symbol-name keyword))))
      (do* ((load-paths expand-load-paths (cdr load-paths))
            (lp (car load-paths) (car load-paths))
            (path (namestring+ lp "/" key) (namestring+ lp "/" key))
            (acc (simplisp-type-tree-p simplisp path) (simplisp-type-tree-p simplisp path)))
           ((or where (null lp)) (if where simplisp nil))
        (cond (acc
               (setq prefix lp where acc type :tree))
              ((setq acc (simplisp-type-module-p simplisp (namestring+ path "." extension)))
               (setq prefix lp where acc type :module)))))))

(defmethod simplisp-root-path ((simplisp <simplisp-option>) pathspec)
  (let ((path (or (simplisp-type-tree-p simplisp pathspec)
                  (dirname (simplisp-type-module-p simplisp pathspec)))))
    (if path
        (labels ((rec (acc) (if (simplisp-type-tree-p simplisp acc)
                                (rec (dirname acc))
                                acc)))
          (rec path)))))

(defmethod simplisp-make-symbol ((simplisp <simplisp-option>) simplisp-root-dir simplisp-where)
  (intern (string-upcase
           (string-gsub
            (string+ "." (extension simplisp)) ""
            (string-gsub "/" "."
                         (string-trim "/" (string-gsub (namestring simplisp-root-dir) ""
                                                       (namestring simplisp-where))))))
          "KEYWORD"))

(defmethod simplisp-root ((simplisp <simplisp>))
  (with-accessors ((where simplisp-where)
                   (keyword simplisp-keyword)
                   (symbol simplisp-symbol)
                   (prefix simplisp-prefix)) simplisp
    (if where
        (let* ((path (simplisp-root-path simplisp where))
               (pkg (simplisp-make-symbol simplisp path (simplisp-where simplisp))))
          (setq keyword pkg)
          (setq symbol pkg)
          (setq prefix path)))))

(defmethod initialize-instance :after ((simplisp <simplisp>) &key)
  (with-accessors ((symbol simplisp-symbol)
                   (where simplisp-where)
                   (prefix simplisp-prefix)
                   (type simplisp-type)) simplisp
    (if (null (and symbol where prefix type))
        (progn
          (if (simplisp-search simplisp)
              (simplisp-root simplisp))))))

(defmethod child-tree ((simplisp <simplisp>))
  (if (eql :tree (simplisp-type simplisp))
      (let (acc)
        (dolist (path (list-directory (simplisp-where simplisp)) acc)
          (if (simplisp-type-tree-p simplisp path)
              (let ((sym (simplisp-make-symbol simplisp (simplisp-prefix simplisp) path)))
                (push
                 (make-instance '<simplisp>
                                :keyword   sym
                                :symbol    sym
                                :where     path
                                :prefix (simplisp-prefix simplisp)
                                :type      :tree)
                 acc)))))))

(defmethod child-module ((simplisp <simplisp>))
  (if (eql :tree (simplisp-type simplisp))
      (let (acc)
        (dolist (path (list-directory (simplisp-where simplisp)) acc)
          (if (simplisp-type-module-p simplisp path)
              (let ((sym (simplisp-make-symbol simplisp (simplisp-prefix simplisp) path)))
                (push
                 (make-instance '<simplisp>
                                :keyword   sym
                                :symbol    sym
                                :where     path
                                :prefix (simplisp-prefix simplisp)
                                :type      :module)
                 acc)))))))

(defmethod simplisp-load-object ((simplisp <simplisp>))
  (let (acc)
    (labels ((rec (pkg)
               (let ((tree (child-tree pkg))
                     (module (child-module pkg)))
                 (if tree (dolist (p tree) (rec p)))
                 (dolist (m module) (push m acc))
                 (push pkg acc))))
      (rec simplisp))))

(defmethod simplisp-require-form ((simplisp <simplisp>))
  (with-accessors ((main-file main-file)
                   (symbol simplisp-symbol)
                   (where simplisp-where)
                   (type simplisp-type)) simplisp
    (let ((current (package-name *package*))
          (path (cond ((eql :tree type)
                       (namestring+ where main-file))
                      ((eql :module type) where))))
      `(progn
         (defpackage ,symbol (:use #:cl))
         (in-package ,symbol)
         (format t "~S~%" *package*)
         (if ,(eql :tree type)
             (dolist (pkg ',(mapcar #'simplisp-symbol (nconc (child-tree simplisp) (child-module simplisp))))
                 (use-package pkg)))
         (cl:load ,path)
         (in-package ,current)))))

;;===================
;; REQUIRE
;; IMPORT
;; EXPORT
;;=================== 

(export '*require*)
(defparameter *require* (make-hash-table :test #'eql))

(shadow 'require)
(export 'require)
(defun require (module &key (force nil))
  (let ((obj (make-instance '<simplisp> :keyword module)))
    (if (simplisp-symbol obj)
        (let ((objs (nreverse (simplisp-load-object obj))))
          (if force
              (dolist (obj objs T)
                (setf (gethash (simplisp-symbol obj) *require*) nil)))
          (dolist (obj objs T)
            (if (null (gethash (simplisp-symbol obj) *require*))
                (progn
                  (eval (simplisp-require-form obj))
                  (setf (gethash (simplisp-symbol obj) *require*) obj)))))
        (error "Not Found Package."))))

(shadow 'import)
(export 'import)
(defun import (module &key (force nil))
  (progn
    (require module :force force)
    (use-package module)))

(export 'external-symbols)
(defun external-symbols (package)
  (let (symbols)
    (do-external-symbols (sym (find-package package))
      (push sym symbols))
    symbols))

(export 'external-symbols-export)
(defun external-symbols-export (&rest packages)
  (dolist (package packages t)
    (do-external-symbols (sym (find-package package))
      (export sym))))

(export 'inherit-export)
(defun inherit-export ()
  (let* ((pkg (make-instance '<simplisp> :keyword (intern (package-name *package*) "KEYWORD")))
         (lst (nconc (child-tree pkg) (child-module pkg))))
    (dolist (l lst t) (external-symbols-export (simplisp-symbol l)))))

(shadow 'search)
(export 'search)
(defun search (module)
  (let ((obj (make-instance '<simplisp> :keyword module)))
    (if (simplisp-symbol obj)
        (simplisp-where obj))))

(shadow 'test)
(export 'test)
(defmacro test (module)
  (let* ((obj (make-instance '<simplisp> :keyword module))
         (current (package-name *package*))
         (base-symbol (simplisp-symbol obj))
         (test-symbol (intern (string+ (symbol-name base-symbol) "-TEST") "KEYWORD"))
         (test-file (merge-pathnames (simplisp-where obj) *test-file*)))
    (if (and (eql :tree (simplisp-type obj))
             (file-exist-p test-file))
        `(progn
           (require ,base-symbol)
           
           (defpackage ,test-symbol
             (:use :cl ,base-symbol))
           (in-package ,test-symbol)
           (format t "~%---------Test Start---------~%")
           (format t "~S~%" *package*)
           (load ,test-file)
           (in-package ,current)
           (format t "~%----------Test End----------~%"))
        (error (format nil "Not Found ~a.~%" *test-file*)))))



