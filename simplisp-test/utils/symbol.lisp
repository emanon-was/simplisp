;;===================
;; Symbol
;;=================== 

(defun external-symbols (package)
  (let (symbols)
    (do-external-symbols (sym (find-package package))
      (push sym symbols))
    symbols))

(defun typespecp (sym)
  (if (or (multiple-value-bind (x y)
              (ignore-errors (subtypep 't sym))
            (and (null x) (eq 't y)))
          (eq 't (ignore-errors (typep nil sym))))
      (progn (print sym) t)))

(defun defined-symbols (symbol)
  (and
    (eq *package* (symbol-package symbol))
    (or (fboundp symbol)
        (boundp symbol)
        (find-class symbol nil)
        (typespecp symbol))))

