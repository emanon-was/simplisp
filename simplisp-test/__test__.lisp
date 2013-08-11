(simple:require :simplisp-test)
(in-package :simplisp-test)

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

(minitest (or (require :simplisp-test.utils.string)
              (not (null (gethash :simplisp-test.utils.string *provide*)))) t)
(minitest (or (require :simplisp-test.utils)
              (not (null (gethash :simplisp-test.utils *provide*)))) t)
(minitest (or (require :simplisp-test)
              (not (null (gethash :simplisp-test *provide*)))) t)

