;;===================
;; String
;;=================== 

;; CL-USER> (string-split "," "abc,def,ghi,jkl")
;; ("abc" "def" "ghi" "jkl")
;; CL-USER> (string-split #\, "abc,def,ghi,jkl")
;; ("abc" "def" "ghi" "jkl")
(defun string-split (separator string &key (test #'char=))
  (let* ((sep (if (stringp separator) separator (princ-to-string separator)))
         (str (if (stringp string) string (princ-to-string string)))
         (sep-length (if (stringp separator) (length separator) 1)))
    (do* ((start 0 (+ end sep-length))
          (end (search sep str :start2 start :test test)
               (search sep str :start2 start :test test))
          (acc (cons (subseq str start end) nil)
               (cons (subseq str start end) acc)))
         ((null end) (nreverse acc)))))

;; CL-USER> (string-join "," '("abc" "def" "ghi" "jkl"))
;; "abc,def,ghi,jkl"
;; CL-USER> (string-join #\, '("abc" "def" "ghi" "jkl"))
;; "abc,def,ghi,jkl"
(defun string-join (delimiter list)
  (if (listp list)
      (let ((del (if (stringp delimiter) delimiter (princ-to-string delimiter))))
        (format nil (concatenate 'string "~{~a~^" del "~}") list))))

;; CL-USER> (string-gsub "e" "0" "sleep")
;; "sl00p"
;; CL-USER> (string-gsub #\e #\0 "sleep")
;; "sl00p"
(defun string-gsub (match replacement string &key (test #'char=))
  (string-join replacement (string-split match string :test test)))

;; CL-USER> (string+ "abc" "def" "efg")
;; "abcdefefg"
(defun string+ (&rest other-args)
  (string-join "" other-args))

;; CL-USER> (namestring+ #P"/home/user/" "test" "." "lisp")
;; "/home/user/test.lisp"
(defun namestring+ (&rest other-args)
  (string-join "" (mapcar #'(lambda (x) (cond ((null x) "")
                                              ((stringp x) x)
                                              (t (namestring x)))) other-args)))

