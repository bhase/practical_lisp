(defun primep (num)
  (when (> num 1)
    (loop for fac from 2 to (isqrt num) never (zerop (mod num fac)))))

(defun next-prime (num)
  (loop for n from num when (primep n) return n))

(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,ending-value-name))
       ,@body)))
