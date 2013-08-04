
(export '*load-paths*)
(defparameter *load-paths* '("./" "../" "./*/"))

(export '*target-file*)
(defparameter *target-file* "__init__.lisp")

(export '*load-extension*)
(defparameter *load-extension* "lisp")

(export 'add-load-paths)
(defun add-load-paths (&rest load-paths)
  (nconc *load-paths* load-paths))

