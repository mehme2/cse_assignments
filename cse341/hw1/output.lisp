


(declaim (ftype (function (integer integer) integer) sum))

(defun sum (a b)
(+ a b))

(defun main ()
(let ((x 10))
(let ((y 20))
(let ((result (sum x y)))

(if (> result 25)
(progn
(format t "Result is greater than 25~%")
(setf x 5)))

(let ((i 0))
(loop while (< i 10) do
(if (>= i 0)
(progn
(format t "~d~%" (incf i)))))

0)))))

(main)