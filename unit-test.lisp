(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

(defmacro combine-results (&body forms)
  (let ((result (gensym)))
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(defun test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 3) 9)
    (= (* -1 7) -7)))

(defun test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))
