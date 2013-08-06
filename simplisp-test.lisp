;;
;; in simplisp directory
;;

(if (null (find-package :simplisp))
    (load "./simplisp.lisp"))

(in-package :simplisp)

(rem-all-tests)

(deftest last1.1
    (last1 '(1 2 3 4 5)) 5)

(deftest single.1
    (single '(1)) 1)

(deftest filter.1
    (filter #'(lambda (x) (if (oddp x) x)) '(1 2 3 4 5))
  (1 3 5))

(deftest string-split.1
    (string-split "," "abc,def,ghi,jkl")
  ("abc" "def" "ghi" "jkl"))

(deftest string-split.2
    (string-split #\, "abc,def,ghi,jkl")
  ("abc" "def" "ghi" "jkl"))

(deftest string-join.1
    (string-join "," '("abc" "def" "ghi" "jkl"))
  "abc,def,ghi,jkl")

(deftest string-join.2
    (string-join #\, '("abc" "def" "ghi" "jkl"))
  "abc,def,ghi,jkl")

(deftest string+.1
    (string+ "abc" "def" "efg")
  "abcdefefg")

(deftest namestring+.1
    (namestring+ #P"/home/user/" "test" "." "lisp")
  "/home/user/test.lisp")

(deftest directory-pathname-p.1
    (directory-pathname-p "~/test.lisp")
  nil)

(deftest directory-pathname-p.2
    (directory-pathname-p "~/")
  "~/")

(deftest pathname-as-directory.1
    (pathname-as-directory "/bin")
  #P"/bin/")

(deftest pathname-exist-p.1
    (pathname-exist-p "/bin")
  #P"/bin/")

(deftest directory-exist-p.1
    (directory-exist-p "/bin")
  #P"/bin/")

(deftest directory-exist-p.2
    (directory-exist-p "/bin/bash")
  nil)

(deftest file-exist-p.1
    (file-exist-p "/bin/bash")
  #P"/bin/bash")

(deftest file-exist-p.2
    (file-exist-p "/bin")
  nil)

(deftest dirname.1
    (dirname "/bin/bash")
  #P"/bin/")

(deftest dirname.2
    (dirname "/bin/")
  #P"/")

(deftest dirname.3
    (dirname "/")
  #P"/")

(deftest basename.1
    (basename "/bin")
  "bin")

(deftest basename.2
    (basename "/bin/bash")
  "bash")

;;---------------------------------

(defvar slobj1 (make-instance '<simplisp> :keyword :test))
(defvar slobj2 (make-instance '<simplisp> :keyword :test.utils))
(defvar slobj3 (make-instance '<simplisp> :keyword :test.utils.string))

(deftest simplisp-symbol.1
    (simplisp-symbol slobj1)
  :test)
(deftest simplisp-symbol.2
    (simplisp-symbol slobj2)
  :test.utils)
(deftest simplisp-symbol.3
    (simplisp-symbol slobj3)
  :test.utils.string)

(deftest child-tree.1
    (not (null (child-tree slobj1)))
  t)
(deftest child-tree.2
    (child-tree slobj2)
  nil)
(deftest child-tree.3
    (child-tree slobj3)
  nil)

(deftest child-module.1
    (not (null (child-module slobj1)))
  t)
(deftest child-module.2
    (not (null (child-module slobj2)))
  t)
(deftest child-module.3
    (child-module slobj3)
  nil)

(deftest simplisp-load-object.1
    (not (null (simplisp-load-object slobj1)))
  t)
(deftest simplisp-load-object.2
    (not (null (simplisp-load-object slobj2)))
  t)
(deftest simplisp-load-object.3
    (mapcar #'simplisp-symbol (simplisp-load-object slobj3))
  (:test.utils.string))

(deftest require.3
    (require :test.utils.string)
  t)

(deftest require.2
    (require :test.utils)
  t)

(deftest require.1
    (require :test)
  t)

(do-tests)
