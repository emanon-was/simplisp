(sl:import :test.utils.string)

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


