(sl:import :test.utils)
(sl:import :test.options)
(sl:import :test.locate)

(export 'external-symbols)
(defun external-symbols (package)
  (let (symbols)
    (do-external-symbols (sym (find-package package))
      (push sym symbols))
    symbols))

(export 'external-symbols-export)
(defun external-symbols-export (&rest tpkgname)
  (dolist (package tpkgname t)
     (do-external-symbols (sym (find-package package))
       (export sym))))

(export 'inherit-export)
(defun inherit-export ()
  (let* ((pkg (intern (package-name *package*)))
         (lst (nconc (child-tree pkg) (child-module pkg))))
    (dolist (l lst t) (external-symbols-export l))))

