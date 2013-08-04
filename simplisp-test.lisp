(load "simplisp.lisp")
(in-package :simplisp)

(format t " TEST ... ~S~%"
        (and

         (minitest (last1 '(1 2 3 4 5))
                   5)

         (minitest (single '(1))
                   1)

         (minitest (filter #'(lambda (x) (if (oddp x) x)) '(1 2 3 4 5))
                   '(1 3 5))

         (minitest (string-split "," "abc,def,ghi,jkl")
                   '("abc" "def" "ghi" "jkl"))

         (minitest (string-split #\, "abc,def,ghi,jkl")
                   '("abc" "def" "ghi" "jkl"))

         (minitest (string-join "," '("abc" "def" "ghi" "jkl"))
                   "abc,def,ghi,jkl")

         (minitest (string-join #\, '("abc" "def" "ghi" "jkl"))
                   "abc,def,ghi,jkl")

         (minitest (string+ "abc" "def" "efg")
                   "abcdefefg")

         (minitest (namestring+ #P"/home/user/" "test" "." "lisp")
                   "/home/user/test.lisp")

         (minitest (directory-pathname-p "~/test.lisp")
                   nil)

         (minitest (directory-pathname-p "~/")
                   "~/")

         (minitest (namestring (pathname-as-directory "/bin"))
                   "/bin/")

         (minitest (pathname-exist-p "/bin")
                   #P"/bin/"
                   :test #'pathname-match-p)

         (minitest (directory-exist-p "/bin")
                   #P"/bin/"
                   :test #'pathname-match-p)

         (minitest (directory-exist-p "/bin/bash")
                   nil)

         (minitest (file-exist-p "/bin/bash")
                   #P"/bin/bash"
                   :test #'pathname-match-p)

         (minitest (file-exist-p "/bin")
                   nil)

         (minitest (dirname "/bin/bash")
                   #P"/bin/"
                   :test #'pathname-match-p)

         (minitest (dirname "/bin/")
                   #P"/"
                   :test #'pathname-match-p)

         (minitest (dirname "/")
                   #P"/"
                   :test #'pathname-match-p)

         (minitest (basename "/bin")
                   "bin")

         (minitest (basename "/bin/bash")
                   "bash")

         ;;---------------------------------

         (setf slobj (make-instance '<simplisp> :keyword :test.utils))

         (minitest (simplisp-where slobj)
                   (pathname-exist-p (namestring+ "./test/utils")))

         (minitest (simplisp-symbol slobj)
                   :test.utils)

         (minitest (simplisp-prefix slobj)
                   (pathname-exist-p "."))

         ;; (minitest (mapcar #'simplisp-symbol (child-tree slobj))
         ;;           nil)

         ;; (minitest (mapcar #'simplisp-symbol (child-module slobj))
         ;;           '(:TEST.UTILS.LIST :TEST.UTILS.PATH :TEST.UTILS.STRING))

         ;; (minitest (mapcar #'simplisp-symbol (simplisp-load-object slobj))
         ;;           '(:TEST.UTILS :TEST.UTILS.STRING :TEST.UTILS.PATH :TEST.UTILS.LIST))

         ;; (minitest (simplisp-require-form slobj) t
         ;;           :test #'(lambda (&rest form) t))

         (minitest (package-name (require :test.utils :force t))
                   (package-name *package*))

         ;;----------------------------------          

         (setq slobj (make-instance '<simplisp> :keyword :test))

         (minitest (simplisp-where slobj)
                   (pathname-exist-p (namestring+ "./test/")))

         ;; (minitest (mapcar #'simplisp-symbol (child-tree slobj))
         ;;           '(:TEST.OTHERS :TEST.UTILS))

         ;; (minitest (mapcar #'simplisp-symbol (child-module slobj))
         ;;           '(:TEST.EXPORT :TEST.LOCATE :TEST.OPTIONS :TEST.REQUIRE))

         ;; (minitest (mapcar #'simplisp-symbol (simplisp-load-object slobj))
         ;;           '(:TEST :TEST.REQUIRE :TEST.OPTIONS :TEST.LOCATE :TEST.EXPORT :TEST.UTILS :TEST.UTILS.STRING :TEST.UTILS.PATH :TEST.UTILS.LIST :TEST.OTHERS))

         ;; (minitest (simplisp-require-form slobj) t
         ;;           :test #'(lambda (&rest form) t))

         (minitest (package-name (require :test :force t))
                   (package-name *package*))

         ;;-------------------------

         (setq slobj (make-instance '<simplisp> :keyword :test.utils.string))

         (minitest (simplisp-where slobj)
                   (pathname-exist-p (namestring+ "./test/utils/STRING.lisp")))

         (minitest (mapcar #'simplisp-symbol (child-tree slobj))
                   nil)

         (minitest (mapcar #'simplisp-symbol (child-module slobj))
                   nil)

         (minitest (mapcar #'simplisp-symbol (simplisp-load-object slobj))
                   '(:test.utils.string))

         (minitest (simplisp-require-form slobj) t
                   :test #'(lambda (&rest form) t))

         (minitest (package-name (require :test.utils.string :force t))
                   (package-name *package*))
         ))
