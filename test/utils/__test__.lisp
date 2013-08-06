(use-package :sl.test)

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

(do-tests)
(rem-all-tests)
