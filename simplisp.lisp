
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



(cl:defpackage #:simplisp
  (:nicknames #:sl)
  (:use #:cl #:simplisp.utils))

(cl:in-package #:simplisp)

;;===================
;; OPTIONS
;;=================== 

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

(defmethod simplisp-load-object ((simplisp <simplisp>))
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
         (if ,(eql :system type)
             (dolist (pkg ',(mapcar #'simplisp-symbol (nconc (child-system simplisp) (child-module simplisp))))
                 (use-package pkg)))
         (cl:load ,path)
         (if ,(eql :module type)
             (do-symbols (sym)
               (if (and (eq *package* (symbol-package sym))
                        (or (fboundp sym) (boundp sym)))
                   (export sym))))
         (in-package ,current)))))

;;===================
;; SEARCH
;; REQUIRE,IMPORT
;; EXPORT,TEST
;;=================== 

(shadow 'search)
(export 'search)
(defun search (system)
  (let ((obj (make-instance '<simplisp> :keyword system)))
    (if (simplisp-symbol obj)
        (simplisp-where obj))))

(defparameter *require* (make-hash-table :test #'eql))

(shadow 'require)
(export 'require)
(defun require (system &key (force nil))
  (let ((obj (make-instance '<simplisp> :keyword system)))
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
(defun import (system &key (force nil))
  (progn
    (require system :force force)
    (use-package system)))

(shadow 'test)
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
           (require ,base-symbol)
           
           (defpackage ,test-symbol
             (:use :cl))
           (in-package ,test-symbol)
           (format t "~%---------Test Start---------~%")
           (format t "~S~%" *package*)
           (load ,test-file)
           (in-package ,current)
           (format t "~%----------Test End----------~%")
           t)
        (error (format nil "Not Found ~a.~%" *test-file*)))))

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
         (lst (nconc (child-system pkg) (child-module pkg))))
    (dolist (l lst t) (external-symbols-export (simplisp-symbol l)))))



