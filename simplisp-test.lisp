;;
;; in simplisp directory
;;

(if (null (find-package :simplisp))
    (load (merge-pathnames
           (make-pathname :directory (pathname-directory *load-truename*))
           "simplisp.lisp")))

(in-package :simplisp)

(defun same (arg0 arg1)
  (labels ((type-match (type)
             (and (typep arg0 type)
                  (typep arg1 type))))
    (cond ((type-match 'symbol)   (eq arg0 arg1))
          ((type-match 'fixnum)   (= arg0 arg1))
          ((type-match 'string)   (equal arg0 arg1))
          ((type-match 'list)     (equalp arg0 arg1))
          ((type-match 'pathname) (pathname-match-p arg0 arg1))
          (t                      (equalp arg0 arg1)))))

(defmacro minitest (form expect)
  `(let ((result ,form))
     (if (same result ,expect)
         (prog1 t   (format t " [PASS] : ~S~%~10t(SAME ~S ~S)~%" ',form result ,expect))
         (prog1 nil (format t " [FAIL] : ~S~%~10t(NOT (SAME ~S ~S))~%" ',form result ,expect)))))

(minitest (last1 '(1 2 3 4 5)) 5)
(minitest (single '(1)) 1)
(minitest (filter #'(lambda (x) (if (oddp x) x)) '(1 2 3 4 5)) '(1 3 5))
(minitest (string-split "," "abc,def,ghi,jkl") '("abc" "def" "ghi" "jkl"))
(minitest (string-split #\, "abc,def,ghi,jkl") '("abc" "def" "ghi" "jkl"))
(minitest (string-join "," '("abc" "def" "ghi" "jkl")) "abc,def,ghi,jkl")
(minitest (string-join #\, '("abc" "def" "ghi" "jkl")) "abc,def,ghi,jkl")
(minitest (string+ "abc" "def" "efg") "abcdefefg")
(minitest (namestring+ #P"/home/user/" "test" "." "lisp") "/home/user/test.lisp")
(minitest (directory-pathname-p "~/test.lisp") nil)
(minitest (directory-pathname-p "~/") "~/")
(minitest (pathname-as-directory "/bin") #P"/bin/")
(minitest (pathname-exist-p "/bin") #P"/bin/")
(minitest (directory-exist-p "/bin") #P"/bin/")
(minitest (directory-exist-p "/bin/bash") nil)
(minitest (file-exist-p "/bin/bash") #P"/bin/bash")
(minitest (file-exist-p "/bin") nil)
(minitest (dirname "/bin/bash") #P"/bin/")
(minitest (dirname "/bin/") #P"/")
(minitest (dirname "/") #P"/")
(minitest (basename "/bin") "bin")
(minitest (basename "/bin/bash") "bash")

;;---------------------------------

(minitest (defvar slobj1 (make-instance '<simplisp> :keyword :simplisp-test)) 'SLOBJ1)
(minitest (defvar slobj2 (make-instance '<simplisp> :keyword :simplisp-test.utils)) 'SLOBJ2)
(minitest (defvar slobj3 (make-instance '<simplisp> :keyword :simplisp-test.utils.string)) 'SLOBJ3)

(minitest (simplisp-symbol slobj1) :simplisp-test)
(minitest (simplisp-symbol slobj2) :simplisp-test.utils)
(minitest (simplisp-symbol slobj3) :simplisp-test.utils.string)

(minitest (null (child-system slobj1)) nil)
(minitest (child-system slobj2) nil)
(minitest (child-system slobj3) nil)

(minitest (null (child-module slobj1)) nil)
(minitest (null (child-module slobj2)) nil)
(minitest (child-module slobj3) nil)

(minitest (null (simplisp-all slobj1)) nil)
(minitest (null (simplisp-all slobj2)) nil)
(minitest (mapcar #'simplisp-symbol (simplisp-all slobj3))
          '(:simplisp-test.utils.string))

(minitest (require :simplisp-test.utils.string) t)
(minitest (require :simplisp-test.utils) t)
(minitest (require :simplisp-test) t)

