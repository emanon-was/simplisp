;;===================
;; List
;;=================== 

;; CL-USER> (last1 '(1 2 3 4 5))
;; 5
(defun last1 (lst)
  (car (last lst)))

;; CL-USER> (single '(1))
;; 1
(defun single (lst)
  (and (consp lst) (null (cdr lst)) (car lst)))

;; CL-USER> (filter #'(lambda (x) (if (oddp x) x)) '(1 2 3 4 5))
;; (1 3 5)
(defun filter (func lst)
  (let ((acc nil))
    (dolist (tmp lst (nreverse acc))
      (let ((val (funcall func tmp)))
        (if val (push val acc))))))


