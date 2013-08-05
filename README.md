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
    ├── export.lisp
    ├── require.lisp
    ├── locate.lisp
    ├── options.lisp
    ├── others
    │   └── test.lisp
    └── utils
        ├── __main__.lisp
        ├── list.lisp
        ├── path.lisp
        └── string.lisp
    
If you change "__main__.lisp" to "__read__.lisp"

    CL-USER> (defparameter sl:*target-file* "__read__.lisp")
 
### Load Paths ###

    CL-USER> sl:*load-paths*
    ("./" "../" "./*/")
    CL-USER> (sl:add-load-paths "~/")
    ("./" "../" "./*/" "~/")

### Require Package ###

    CL-USER> (sl:require :test)
    #<PACKAGE TEST.UTILS.LIST>
    #<PACKAGE TEST.UTILS.PATH>
    #<PACKAGE TEST.UTILS.STRING>
    #<PACKAGE TEST.UTILS.STRING>
    #<PACKAGE TEST.UTILS>
    #<PACKAGE TEST.REQUIRE>
    #<PACKAGE TEST.OPTIONS>
    #<PACKAGE TEST.LOCATE>
    #<PACKAGE TEST.EXPORT>
    #<PACKAGE TEST.LOCATE>
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

After "__main__.lisp" is added this, require package

    (sl:inherit-export)

or CUI

    CL-USER> (in-package :test.utils)
    CL-USER> (sl:inherit-export)

### ETC ###

    CL-USER> (sl:search :test.utils)
    #P"/home/user/test/utils/"

    CL-USER> (sl:external-symbols :test.utils)
    (TEST.UTILS.STRING:STRING+
     TEST.UTILS.LIST:FILTER
     TEST.UTILS.LIST:LAST1
     TEST.UTILS.PATH:BASENAME
     TEST.UTILS.PATH:PATHNAME-ABSOLUTE
     TEST.UTILS.PATH:FILE-EXIST-P TEST.UTILS.PATH:DIRNAME
     TEST.UTILS.PATH:LIST-DIRECTORY
     TEST.UTILS.LIST:SINGLE TEST.UTILS.STRING:STRING-JOIN
     TEST.UTILS.STRING:STRING-GSUB
     TEST.UTILS.STRING:STRING-SPLIT
     TEST.UTILS.PATH:NAMESTRING+
     TEST.UTILS.PATH:DIRECTORY-EXIST-P
     TEST.UTILS.PATH:PATHNAME-EXIST-P)

Depends
------
CLISP,SBCL,ClozureCL,ABCL

Impressions
------
Common Lisp is very interesting! 


