(sl:import :simplisp-test.utils)
(sl:import :simplisp-test.options)
(sl:import :simplisp-test.classes)

(defun external-symbols (package)
  (let (symbols)
    (do-external-symbols (sym (find-package package))
      (push sym symbols))
    symbols))

(defun external-symbols-export (&rest packages)
  (dolist (package packages t)
    (do-external-symbols (sym (find-package package))
      (export sym))))

(defun inherit-export ()
  (let* ((pkg (make-instance '<simplisp> :keyword (intern (package-name *package*) "KEYWORD")))
         (lst (nconc (child-system pkg) (child-module pkg))))
    (dolist (l lst t) (external-symbols-export (simplisp-symbol l)))))
