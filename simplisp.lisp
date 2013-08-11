(cl:defpackage #:simplisp
  (:nicknames #:simple)
  (:use #:cl)
  (:shadow #:search
           #:require
           #:import
           #:test))

(cl:in-package #:simplisp)

;;===================
;; Macro
;;=================== 

(defmacro performance (times &body body)
  `(time (dotimes (x ,times) ,@body)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

;;===================
;; Symbol
;;=================== 

(defun external-symbols (package)
  (let (symbols)
    (do-external-symbols (sym (find-package package))
      (push sym symbols))
    symbols))

(defun typespecp (sym)
  (if (or (multiple-value-bind (x y)
              (ignore-errors (subtypep 't sym))
            (and (null x) (eq 't y)))
          (eq 't (ignore-errors (typep nil sym))))
      (progn (print sym) t)))

(defun defined-symbols (symbol)
  (and
    (eq *package* (symbol-package symbol))
    (or (fboundp symbol)
        (boundp symbol)
        (find-class symbol nil)
        (typespecp symbol))))

;;===================
;; List
;;=================== 

;; CL-USER> (last1 '(1 2 3 4 5))
;; 5
(defun last1 (lst)
  (car (last lst)))

;; CL-USER> (single '(1))
;; 1
(defun single (lst)
  (and (consp lst) (null (cdr lst)) (car lst)))

;; CL-USER> (filter #'(lambda (x) (if (oddp x) x)) '(1 2 3 4 5))
;; (1 3 5)
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
(defun string-split (separator string &key (test #'char=))
  (let* ((sep (if (stringp separator) separator (princ-to-string separator)))
         (str (if (stringp string) string (princ-to-string string)))
         (sep-length (if (stringp separator) (length separator) 1)))
    (do* ((start 0 (+ end sep-length))
          (end (cl:search sep str :start2 start :test test)
               (cl:search sep str :start2 start :test test))
          (acc (cons (subseq str start end) nil)
               (cons (subseq str start end) acc)))
         ((null end) (nreverse acc)))))

;; CL-USER> (string-join "," '("abc" "def" "ghi" "jkl"))
;; "abc,def,ghi,jkl"
;; CL-USER> (string-join #\, '("abc" "def" "ghi" "jkl"))
;; "abc,def,ghi,jkl"
(defun string-join (delimiter list)
  (if (listp list)
      (let ((del (if (stringp delimiter) delimiter (princ-to-string delimiter))))
        (format nil (concatenate 'string "~{~a~^" del "~}") list))))

;; CL-USER> (string-gsub "e" "0" "sleep")
;; "sl00p"
;; CL-USER> (string-gsub #\e #\0 "sleep")
;; "sl00p"
(defun string-gsub (match replacement string &key (test #'char=))
  (string-join replacement (string-split match string :test test)))

;; CL-USER> (string+ "abc" "def" "efg")
;; "abcdefefg"
(defun string+ (&rest other-args)
  (string-join "" other-args))

;; CL-USER> (namestring+ #P"/home/user/" "test" "." "lisp")
;; "/home/user/test.lisp"
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

(defun directory-pathname-p (pathspec)
  (if pathspec
      (and
       (not (component-present-p (pathname-name pathspec)))
       (not (component-present-p (pathname-type pathspec)))
       pathspec)))

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
(defun pathname-exist-p (pathspec)
  (let ((pathname (file-exists-p pathspec)))
    (if pathname
        (car (directory pathname)))))

;; CL-USER> (directory-exist-p #P"~")
;; #P"/home/user/"
;; CL-USER> (directory-exist-p #P"~/test.lisp")
;; NIL
(defun directory-exist-p (pathspec)
  (if pathspec
      (let ((pathname (pathname-exist-p pathspec)))
        (if (directory-pathname-p pathname)
            pathname))))

;; CL-USER> (file-exist-p #P"/home/user/")
;; NIL
;; CL-USER> (file-exist-p #P"/home/user/test.lisp")
;; #P"/home/test/test.lisp"
(defun file-exist-p (pathspec)
  (if pathspec
      (let ((pathname (pathname-exist-p pathspec)))
        (if (null (directory-pathname-p pathname))
            pathname))))

;; CL-USER> (dirname #P"/home/user/")
;; #P"/home/"
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
(defun basename (pathspec)
  (let ((pathname (pathname-exist-p pathspec)))
    (if pathname
        (if (directory-pathname-p pathname)
            (car (last (pathname-directory pathname)))
            (file-namestring pathname)))))

;;===================
;; OPTIONS
;;=================== 

(export '*repository*)
(defparameter *repository* '("./" "../" "~/" "~/.lisp/"))
(push (dirname *load-pathname*) *repository*)
(defparameter *main-file* "__main__.lisp")
(defparameter *test-file* "__test__.lisp")
(defparameter *ignore-files* (list *main-file* *test-file*))
(defparameter *extension* "lisp")

;;===================
;; SIMPLISP CLASS
;;=================== 

(defclass <simplisp-option> ()
  ((repository
    :accessor repository
    :initform *repository*)
   (main-file
    :accessor main-file
    :initform *main-file*)
   (extension
    :accessor extension
    :initform *extension*)
   (expand-repository
    :accessor expand-repository
    :initform (let ((acc nil))
                (dolist (tmp *repository* (nreverse acc))
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

(defmethod simplisp-type-system-p ((simplisp <simplisp-option>) pathspec)
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
           (simplisp-type-system-p simplisp (dirname pathname))
           pathname))))

(defmethod simplisp-search ((simplisp <simplisp>))
  (with-accessors ((main-file main-file)
                   (extension extension)
                   (expand-repository expand-repository)
                   (keyword simplisp-keyword)
                   (symbol simplisp-symbol)
                   (where simplisp-where)
                   (prefix simplisp-prefix)
                   (type simplisp-type)) simplisp
    (let ((key (string-gsub #\. #\/ (symbol-name keyword))))
      (do* ((repository expand-repository (cdr repository))
            (lp (car repository) (car repository))
            (path (namestring+ lp "/" key) (namestring+ lp "/" key))
            (acc (simplisp-type-system-p simplisp path) (simplisp-type-system-p simplisp path)))
           ((or where (null lp)) (if where simplisp nil))
        (cond (acc
               (setq prefix lp where acc type :system))
              ((setq acc (simplisp-type-module-p simplisp (namestring+ path "." extension)))
               (setq prefix lp where acc type :module)))))))

(defmethod simplisp-root-path ((simplisp <simplisp-option>) pathspec)
  (let ((path (or (simplisp-type-system-p simplisp pathspec)
                  (dirname (simplisp-type-module-p simplisp pathspec)))))
    (if path
        (labels ((rec (acc) (if (simplisp-type-system-p simplisp acc)
                                (rec (dirname acc))
                                acc)))
          (rec path)))))

(defmethod simplisp-make-symbol ((simplisp <simplisp-option>) simplisp-root-path simplisp-where)
  (intern (string-upcase
           (string-gsub
            (string+ "." (extension simplisp)) ""
            (string-gsub "/" "."
                         (string-trim "/" (string-gsub (namestring simplisp-root-path) ""
                                                       (namestring simplisp-where))))))
          "KEYWORD"))

(defmethod simplisp-chroot ((simplisp <simplisp>))
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
              (simplisp-chroot simplisp))))))

(defmethod child-system ((simplisp <simplisp>))
  (if (eql :system (simplisp-type simplisp))
      (let (acc)
        (dolist (path (list-directory (simplisp-where simplisp)) acc)
          (if (simplisp-type-system-p simplisp path)
              (let ((sym (simplisp-make-symbol simplisp (simplisp-prefix simplisp) path)))
                (push
                 (make-instance '<simplisp>
                                :keyword   sym
                                :symbol    sym
                                :where     path
                                :prefix (simplisp-prefix simplisp)
                                :type      :system)
                 acc)))))))

(defmethod child-module ((simplisp <simplisp>))
  (if (eql :system (simplisp-type simplisp))
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

(defmethod simplisp-all ((simplisp <simplisp>))
  (let (acc)
    (labels ((rec (pkg)
               (let ((system (child-system pkg))
                     (module (child-module pkg)))
                 (if system (dolist (p system) (rec p)))
                 (dolist (m module) (push m acc))
                 (push pkg acc))))
      (rec simplisp))))

(defmethod simplisp-require-form ((simplisp <simplisp>))
  (with-accessors ((main-file main-file)
                   (symbol simplisp-symbol)
                   (where simplisp-where)
                   (type simplisp-type)) simplisp
    (let ((current (package-name *package*))
          (path (cond ((eql :system type)
                       (namestring+ where main-file))
                      ((eql :module type) where))))
      `(progn
         (defpackage ,symbol (:use #:cl))
         (in-package ,symbol)
         (format t "~S~%" *package*)
         (cl:load ,path)
         (if ,(eql :module type)
             (do-symbols (sym)
               (if (defined-symbols sym)
                   (export sym))))
         (in-package ,current)))))

;;===================
;; SEARCH
;; REQUIRE
;; IMPORT
;; TEST
;;=================== 

;;(shadow 'search)
(export 'search)
(defun search (system)
  (let ((obj (make-instance '<simplisp> :keyword system)))
    (if (simplisp-symbol obj)
        (simplisp-where obj))))

(export '*provide*)
(defparameter *provide* (make-hash-table :test #'eql))

(defun %require (system &key (force nil))
  (let ((obj (make-instance '<simplisp> :keyword system)))
    (if (simplisp-symbol obj)
        (progn
          (if force
              (dolist (obj (simplisp-all obj) t)
                (setf (gethash (simplisp-symbol obj) *provide*) nil)))
          (if (null (gethash (simplisp-symbol obj) *provide*))
            (progn
              (eval (simplisp-require-form obj))
              (setf (gethash (simplisp-symbol obj) *provide*) obj)
              t)))
        (error (format nil "Not found system(~A)." system)))))

(export 'require)
(defun require (system-or-systems &key (force nil))
  (if (listp system-or-systems)
      (dolist (sys system-or-systems t)
        (%require sys :force force))
      (%require system-or-systems :force force)))

(defun %import (system &key (force nil))
  (%require system :force force)
  (if (null (member (symbol-name system)
                    (mapcar #'package-name (package-use-list *package*))))
      (use-package system)))

(export 'import)
(defun import (system-or-systems &key (force nil))
  (if (listp system-or-systems)
      (dolist (sys system-or-systems t)
        (%import sys :force force))
      (%import system-or-systems :force force)))

(export 'attach)
(defun attach (system-or-systems)
  (if (listp system-or-systems)
      (dolist (system system-or-systems)
        (progn
          (import system)
          (export (external-symbols system))))
      (progn
        (import system-or-systems)
        (export (external-symbols system-or-systems)))))

(export 'detach)
(defun detach (system-or-systems)
  (if (listp system-or-systems)
      (dolist (system system-or-systems)
        (progn
          (unexport (external-symbols system))
          (unuse-package system)))
      (progn
        (unexport (external-symbols system-or-systems))
        (unuse-package system-or-systems))))

(export 'test)
(defmacro test (system)
  (let* ((obj (make-instance '<simplisp> :keyword system))
         (current (package-name *package*))
         (base-symbol (simplisp-symbol obj))
         (test-symbol (intern (string+ (symbol-name base-symbol) "-TEST") "KEYWORD"))
         (test-file (merge-pathnames (simplisp-where obj) *test-file*)))
    (if (and (eql :system (simplisp-type obj))
             (file-exist-p test-file))
        `(progn
           (defpackage ,test-symbol
             (:use :cl))
           (in-package ,test-symbol)
           (format t "~%---------Test Start---------~%~S~%" *package*)
           (load ,test-file)
           (format t "~%----------Test End----------~%")
           (in-package ,current)
           t)
        (error (format nil "Not Found ~a.~%" *test-file*)))))

