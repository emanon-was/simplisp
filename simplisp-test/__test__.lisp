(in-package :simplisp-test.classes)
(use-package :simplisp-test.tests)

(defvar slobj1 (make-instance '<simplisp> :keyword :simplisp-test))
(defvar slobj2 (make-instance '<simplisp> :keyword :simplisp-test.utils))
(defvar slobj3 (make-instance '<simplisp> :keyword :simplisp-test.utils.string))

(deftest simplisp-symbol.1
    (simplisp-symbol slobj1)
  :simplisp-test)
(deftest simplisp-symbol.2
    (simplisp-symbol slobj2)
  :simplisp-test.utils)
(deftest simplisp-symbol.3
    (simplisp-symbol slobj3)
  :simplisp-test.utils.string)

(deftest child-system.1
    (not (null (child-system slobj1)))
  t)
(deftest child-system.2
    (child-system slobj2)
  nil)
(deftest child-system.3
    (child-system slobj3)
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
  (:simplisp-test.utils.string))

(in-package :simplisp-test)

(deftest require.3
    (require :simplisp-test.utils.string)
  t)

(deftest require.2
    (require :simplisp-test.utils)
  t)

(deftest require.1
    (require :simplisp-test)
  t)

(do-tests)
(rem-all-tests)
