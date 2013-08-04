(sl:inherit-export)

;;===================
;; Test
;;=================== 

;; CL-USER> (performance 1000 (+ 1 2 3))
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000119 seconds of total run time (0.000024 user, 0.000095 system)
;;   100.00% CPU
;;   2,232 processor cycles
;;   0 bytes consed
(export 'performance)
(defmacro performance (times &body body)
  `(time (dotimes (x ,times) ,@body)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) (list s '(gensym))) syms)
     ,@body))

(export 'minitest)
(defmacro minitest (form result &key (test #'equal))
  `(let* ((real ,form)
          (bool (funcall ,test real ,result)))
     (format t " ~:[FAIL~;PASS~] ... ~S => ~S~%~10t~S~%"
             bool ',form real ,result)
     bool))

