(simplisp:import :simplisp-test.utils)

;;===================
;; OPTIONS
;;=================== 

(defparameter *repository* '("./" "../" "~/" "~/.lisp/"))
(push (dirname (dirname *load-pathname*)) *repository*)
(defparameter *main-file* "__main__.lisp")
(defparameter *test-file* "__test__.lisp")
(defparameter *ignore-files* (list *main-file* *test-file*))
(defparameter *extension* "lisp")

