Simplisp
======================
Simple package management system to ensure the name space for each directory or file.

    CL-USER> (load "simplisp.lisp")
 
Example
------

Directory to read have "__main__.lisp"

    in /home/user directory
    test
    ├── __main__.lisp
    ├── __test__.lisp
    ├── export.lisp
    ├── require.lisp
    ├── locate.lisp
    ├── options.lisp
    ├── others
    │   └── test.lisp
    └── utils
        ├── __main__.lisp
        ├── __test__.lisp
        ├── list.lisp
        ├── path.lisp
        └── string.lisp
    
If you change "__main__.lisp" to "__read__.lisp"

    CL-USER> (defparameter sl:*main-file* "__read__.lisp")
 
### Load Paths ###

    CL-USER> sl:*load-paths*
    ("./" "../" "./*/")
    CL-USER> (sl:add-load-paths "~/")
    ("./" "../" "./*/" "~/")

### Require Package ###

    CL-USER> (sl:require :test)
    #<PACKAGE TEST.UTILS.LIST>
    #<PACKAGE TEST.UTILS.PATH>
    ....
    #<PACKAGE TEST.OPTIONS>
    #<PACKAGE TEST>
 
Can also require the partial

    CL-USER> (sl:require :test.utils)
    #<PACKAGE TEST.UTILS.LIST>
    #<PACKAGE TEST.UTILS.PATH>
    #<PACKAGE TEST.UTILS.STRING>
    #<PACKAGE TEST.UTILS>

If you want to reload

    CL-USER> (sl:require :test.utils.string :force t)
    #<PACKAGE TEST.UTILS.STRING>

### Import ###

If you want to write package that depends on

    (sl:import :test.utils)

### Export ###

add this to "__main__.lisp" in package

    (sl:inherit-export)

If you want to inherit-export the partical

    (sl:external-symbols-export :test.utils)


### Test ###

load "__test__.lisp"

    CL-USER> (sl:test :test.utils)
    ---------Test Start---------
    #<PACKAGE TEST.UTILS-TEST>
    ;; Loading file /Users/emanon/.simplisp/test/utils/__test__.lisp ...
    Doing 22 pending tests of 22 tests total.
     LAST1.1 SINGLE.1 FILTER.1 STRING-SPLIT.1 STRING-SPLIT.2 STRING-JOIN.1
     .......
     DIRNAME.1 DIRNAME.2 DIRNAME.3 BASENAME.1 BASENAME.2
    No tests failed.
    ;; Loaded file /Users/emanon/.simplisp/test/utils/__test__.lisp
    ----------Test End----------

### ETC ###

    CL-USER> (sl:search :test.utils)
    #P"/home/user/test/utils/"

    CL-USER> (sl:external-symbols :test.utils)
    (TEST.UTILS.STRING:STRING+
     TEST.UTILS.LIST:FILTER
     TEST.UTILS.LIST:LAST1
     TEST.UTILS.PATH:BASENAME .... )

Depends
------
CLISP,SBCL,ClozureCL,ABCL

Impressions
------
Common Lisp is very interesting! 


