(simple:import '(:simplisp-test.utils
                 :simplisp-test.options))

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


