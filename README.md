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
    ├── classes.lisp
    ├── exports.lisp
    ├── options.lisp
    ├── tests
    │   ├── __main__.lisp
    │   └── __test__.lisp
    └── utils
        ├── __main__.lisp
        ├── __test__.lisp
        ├── list.lisp
        ├── macro.lisp
        ├── path.lisp
        └── string.lisp

### Load Paths ###

    CL-USER> simple:*repository*
    ("./" "../" "~/")
    CL-USER> (push  "~/.lisp/" simple:*repository*)
    ("~/.lisp/" "./" "../" "~/")

### Require System ###

    CL-USER> (simple:require :test)
    #<PACKAGE TEST.UTILS.LIST>
    #<PACKAGE TEST.UTILS.PATH>
    ....
    #<PACKAGE TEST.OPTIONS>
    #<PACKAGE TEST>
 
Can also require the partial

    CL-USER> (simple:require :test.utils)
    #<PACKAGE TEST.UTILS.LIST>
    #<PACKAGE TEST.UTILS.PATH>
    ....
    #<PACKAGE TEST.UTILS.MACRO>
    #<PACKAGE TEST.UTILS>

If you want to reload

    CL-USER> (simple:require :test.utils.string :force t)
    #<PACKAGE TEST.UTILS.STRING>

### Import System ###

'require and 'use-package

    (simple:import :test.utils)

### Export Symbols ###

There is no need to write "export" in other than "__main__.lisp".  
But if you want to export all of other package's external symbols

    (simple:attach :test.options)

    (simple:attach '(:test.utils.string
                     :test.utils.list))

### Test System ###

load "__test__.lisp"

    CL-USER> (simple:test :test.utils)
    ---------Test Start---------
    #<PACKAGE TEST.UTILS-TEST>
    ;; Loading file /Users/emanon/.simplisp/test/utils/__test__.lisp ...
     .......
    ;; Loaded file /Users/emanon/.simplisp/test/utils/__test__.lisp
    ----------Test End----------

### Search System ###

    CL-USER> (simple:search :test.utils)
    #P"/home/user/test/utils/"

Implementation
------
CLISP,SBCL,ClozureCL,ABCL  
Maybe ... AllegroCL,LispWorks

Impressions
------
Common Lisp is very interesting! 


