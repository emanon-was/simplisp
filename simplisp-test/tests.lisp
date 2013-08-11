
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

