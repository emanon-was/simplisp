(sl:import :simplisp-test.utils)
(sl:import :simplisp-test.options)
(sl:import :simplisp-test.classes)

(shadow 'search)
(export 'search)
(defun search (module)
  (let ((obj (make-instance '<simplisp> :keyword module)))
    (if (simplisp-symbol obj)
        (simplisp-where obj))))

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

(shadow 'test)
(export 'test)
(defmacro test (module)
  (let* ((obj (make-instance '<simplisp> :keyword module))
         (current (package-name *package*))
         (base-symbol (simplisp-symbol obj))
         (test-symbol (intern (string+ (symbol-name base-symbol) "-TEST") "KEYWORD"))
         (test-file (merge-pathnames (simplisp-where obj) *test-file*)))
    (if (and (eql :system (simplisp-type obj))
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

