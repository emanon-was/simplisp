
(defparameter *load-paths* '("./" "../" "~/" "~/.lisp/"))
(defparameter *main-file* "__main__.lisp")
(defparameter *test-file* "__test__.lisp")
(defparameter *ignore-files* (list *main-file* *test-file*))
(defparameter *extension* "lisp")

(defun add-load-paths (&rest load-paths)
  (nconc *load-paths* load-paths))

