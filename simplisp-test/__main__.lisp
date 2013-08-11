(simple:import '(:simplisp-test.utils
                 :simplisp-test.tests
                 :simplisp-test.options
                 :simplisp-test.classes))

;;===================
;; SEARCH
;; REQUIRE
;; IMPORT
;; TEST
;;=================== 

(shadow 'search)
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
        (error (format nil "Not Found System ~A.~%" system)))))

(shadow 'require)
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

(shadow 'import)
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
           (defpackage ,test-symbol
             (:use :cl))
           (in-package ,test-symbol)
           (format t "~%---------Test Start---------~%~S~%" *package*)
           (load ,test-file)
           (format t "~%----------Test End----------~%")
           (in-package ,current)
           t)
        (error (format nil "Not Found ~a.~%" *test-file*)))))

