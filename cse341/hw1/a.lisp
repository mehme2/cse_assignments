(defun read-file (f)
  (read-line f nil))

(defun read-entire-file (f)
  (let ((line (read-file f)))
    (if line (concatenate 'string line " " (read-entire-file f)) nil)))

(defun write-file (p s)
  (with-open-file (stream p :direction :output :if-exists :supersede)
    (format stream s)
    (terpri stream)))

(defun is-var-special-char (c)
  (or (eq c #\_) (eq c #\$)))

(defun is-operator-char (c)
  (find c "()[]{}.,;=+-*/><?%:!"))

(defun assemble-string-list (l)
  (if (or (car l) (cdr l))
    (concatenate 'string (car l) (assemble-string-list (cdr l)))
    ""))

(defun starts-with-any-of (s l)
  (if l
    (if (eq 0 (search (car l) s))
      (car l)
      (starts-with-any-of s (cdr l)))
    ""))

(defun is-one-of (s l)
  (if l
    (if (string= s (car l))
      (car l)
      (is-one-of s (cdr l)))
    nil))

(defun list-index (l i)
  (if (> i 0) (list-index (cdr l) (- i 1)) (car l)))

(defun tokenize-alphanumeric (input)
  (let ((first-char (char input 0)))
    (if (or (alphanumericp first-char) (is-var-special-char first-char))
      (concatenate 'string
        (string first-char)
          (tokenize-alphanumeric (subseq input 1)))
      "")))

(defun tokenize-numeric (input)
  (let ((first-char (char input 0)))
    (if (digit-char-p first-char)
      (concatenate 'string
        (string first-char)
          (tokenize-numeric (subseq input 1)))
      "")))

(defun tokenize-operator (input)
  (let ((first-char (char input 0)))
    (if (is-operator-char first-char)
      (let ((special-check (starts-with-any-of input (list "==" ">=" "<=" "+=" "-=" "*=" "/=" "++" "--" "->" "!="))))
        (if (eq (length special-check) 0)
          (string first-char)
          special-check))
      "")))

(defun tokenize-string (input)
  (let ((first-char (char input 0)))
    (if (eq first-char #\")
      (let ((next-pos (position #\" input :start 1)))
        (if next-pos
          (subseq input 0 (+ next-pos 1))
          ""))
      "")))

(defun tokenize (input)
  (let ((clean (string-left-trim " " input)))
    (if (eq (length clean) 0)
      (list nil)
      (let ((first-char (char clean 0)))
        (if (or (alpha-char-p first-char) (is-var-special-char first-char))
          (let ((token (tokenize-alphanumeric clean)))
            (cons token (tokenize (subseq clean (length token)))))
          (if (digit-char-p first-char)
            (let ((token (tokenize-numeric clean)))
              (cons token (tokenize (subseq clean (length token)))))
            (if (is-operator-char first-char)
              (let ((token (tokenize-operator clean)))
                (cons token (tokenize (subseq clean (length token)))))
              (if (eq first-char #\")
                (let ((token (tokenize-string clean)))
                  (cons token (tokenize (subseq clean (length token)))))
                nil))))))))

(defun check-const (tokens)
  (if (string= "const" (car tokens)) (cdr tokens) tokens))

(defun check-typename (tokens)
  (if (is-one-of (car tokens) (list "char" "int" "void" "float" "long"))
    (cdr tokens) nil))

(defun check-string (s tokens)
  (if (string= s (car tokens)) (cdr tokens) nil))

(defun convert-identifier (tokens)
  (format t "~a ~a~%" "convert-identifier" tokens)
  (if tokens
    (let ((id (car tokens)))
      (if (and (or (alpha-char-p (char id 0)) (is-var-special-char (char id 0)))
               (not (is-one-of id (list "int" "char" "float" "void" "double" "if" "for" "enum"
                                       "else" "while" "switch" "case" "struct" "typedef"
                                       "sizeof" "static" "extern" "short" "long" "unsigned"
                                       "const" "break" "default" "do" "signed" "auto" "union"
                                       "return"))))
        (list id (cdr tokens))
        nil))
    nil))

(defun convert-binary-op (tokens)
  (format t "~a ~a~%" "convert-binary-op"  tokens)
  (let ((left (convert-expression-noop tokens)))
    (if left
      (let ((operator (car (list-index left 1))))
        (if (is-one-of operator (list "+" "-" "*" "/" "==" "<" ">" "<=" ">=" "!="))
          (let ((right (convert-expression (cdr (list-index left 1)))))
            (if right
              (list (concatenate 'string "(" operator " " (car left) " " (car right) ")") (list-index right 1))
              nil))
          nil))
      nil)))

(defun convert-call-params (tokens)
  (format t "~a ~a~%" "convert-call-params"  tokens)
  (let ((param (convert-expression tokens)))
    (if param
      (let ((other-params (convert-call-params (check-string "," (list-index param 1)))))
        (if (car other-params)
          (list (concatenate 'string (car param) " " (car other-params)) (list-index other-params 1))
          param))
      (list nil tokens))))

(defun convert-func-call (tokens)
  (format t "~a ~a~%" "convert-func-call"  tokens)
  (let ((name (convert-identifier tokens)))
    (if name
      (let ((params-start (check-string "(" (list-index name 1))))
        (if params-start
          (let ((params (convert-call-params params-start)))
            (if params
              (let ((params-end (check-string ")" (list-index params 1))))
                (if params-end
                  (list (concatenate 'string "(" (car name) " " (car params) ")") params-end)
                  nil))
              (let ((params-end (check-string ")" params-start)))
                (if params-end
                  (list (concatenate 'string "(" (car name) ")") params-end)
                  nil))))
          nil))
      nil)))

(defun convert-var-assign (tokens)
  (format t "~a ~a~%" "convert-var-assign"  tokens)
  (let ((name (convert-identifier tokens)))
    (if name
      (let ((val (convert-expression (check-string "=" (list-index name 1)))))
        (if val
          (list (concatenate 'string "(setf " (car name) " " (car val) ")") (list-index val 1))
          nil))
      nil)))

(defun convert-literal (tokens)
  (format t "~a ~a~%" "convert-literal"  tokens)
  (if tokens
    (if (or (digit-char-p (char (car tokens) 0)) (eq #\" (char (car tokens) 0)))
      (list (car tokens) (cdr tokens))
      nil)
    nil))

(defun convert-expression-noop (tokens)
  (format t "~a ~a~%" "convert-expression-noop"  tokens)
  (let ((try-literal (convert-literal tokens))) (if try-literal try-literal
    (let ((try-func-call (convert-func-call tokens))) (if try-func-call try-func-call
      (let ((try-assign (convert-var-assign tokens))) (if try-assign try-assign
        (let ((try-identifier (convert-identifier tokens))) (if try-identifier try-identifier nil)))))))))

(defun convert-expression (tokens)
  (format t "~a ~a~%" "convert-expression"  tokens)
  (let ((try-binary-op (convert-binary-op tokens))) (if try-binary-op try-binary-op
    (convert-expression-noop tokens))))

(defun convert-if (tokens)
  (format t "~a ~a~%" "convert-if"  tokens)
  (let ((remaining (check-string "(" (check-string "if" tokens))))
    (if remaining
      (let ((condition (convert-expression remaining)))
        (if condition
          (let ((body (convert-statement (check-string ")" (list-index condition 1)))))
            (if body
              (let ((after-else (check-string "else" (list-index body 1))))
                (if after-else
                  (let ((else-block (convert-statement after-else)))
                    (list (concatenate 'string "(if " (car condition) '(#\linefeed)
                                       (car body) '(#\linefeed) (car else-block) ")")
                          (list-index else-block 1)))
                  (list (concatenate 'string "(if " (car condition) '(#\linefeed)
                                     (car body) '(#\linefeed) "nil)")
                        (list-index body 1))))
              nil))
          nil))
      nil)))

(defun convert-var-def (tokens)
  (format t "~a ~a~%" "convert-var-def" tokens)
  (let ((name (convert-identifier (check-typename (check-const tokens)))))
    (if name
      (let ((val-start (check-string "=" (list-index name 1))))
        (if val-start
          (let ((val (convert-expression val-start)))
            (if val
              (let ((statements-start (check-string ";" (list-index val 1))))
                (if statements-start
                  (let ((statements (convert-statement-list statements-start)))
                    (list (concatenate 'string "(let ((" (car name) " " (car val) "))" '(#\linefeed)
                                       (car statements) ")" '(#\linefeed)) (list-index statements 1)))
                  nil))
              nil))
          (let ((statements (convert-statement-list (check-string ";" (list-index name 1)))))
            (if statements
              (list (concatenate 'string "(let ((" (car name) "))" '(#\linefeed)
                                 (car statements) ")" '(#\linefeed)) (list-index statements 1))
              nil))))
      nil)))

(defun convert-statement-block (tokens)
  (format t "~a ~a~%" "convert-statement-block" tokens)
  (if (string= (car tokens) "{")
    (let ((statements (convert-statement-list (check-string "{" tokens))))
      (let ((block-end (check-string "}" (list-index statements 1))))
        (if block-end
          (list (concatenate 'string "(progn" '(#\linefeed) (car statements) ")") block-end)
          nil)))
    nil))

(defun convert-return (tokens)
  (format t "~a ~a~%" "convert-return" tokens)
  (let ((exp-start (check-string "return" tokens)))
    (if exp-start
      (let ((expression (convert-expression exp-start)))
        (if expression
          (let ((remaining (check-string ";" (list-index expression 1))))
            (if remaining
              (list (car expression) remaining)
              nil))
          nil))
      nil)))

(defun convert-while (tokens)
  (format t "~a ~a~%" "convert-while" tokens)
  (let ((condition (convert-expression (check-string "(" (check-string "while" tokens)))))
    (if condition
      (let ((body (convert-statement (check-string ")" (list-index condition 1)))))
        (if body
          (list (concatenate 'string "(loop" '(#\linefeed) "(when (not " (car condition) ") (return))" '(#\linefeed)
                             (car body) ")") (list-index body 1))
          nil))
      nil)))

(defun convert-for-var-def (tokens)
  (format t "~a ~a~%" "convert-for-var-def" tokens)
  (let ((name (convert-identifier (check-typename (check-const tokens)))))
    (if name
      (let ((val-start (check-string "=" (list-index name 1))))
        (if val-start
          (let ((val (convert-expression val-start)))
            (if val
              (list (concatenate 'string "((" (car name) " " (car val) "))") (list-index val 1))
              nil))
          (list (concatenate 'string "((" (car name) "))") (list-index name 1))))
      nil)))

(defun convert-for (tokens)
  (format t "~a ~a~%" "convert-for" tokens)
  (let ((init-start (check-string "(" (check-string "for" tokens))))
    (let ((init (convert-expression init-start)))
      (if init
        (let ((condition (convert-expression (check-string ";" (list-index init 1)))))
          (if condition
            (let ((iterate (convert-expression (check-string ";" (list-index condition 1)))))
              (if iterate
                (let ((body (convert-statement (check-string ")" (list-index iterate 1)))))
                  (if body
                    (list (concatenate 'string (car init) '(#\linefeed) "(loop" '(#\linefeed)
                                       "(when (not " (car condition) ") (return))" '(#\linefeed)
                                       (car body) '(#\linefeed) (car iterate) ")") (list-index body 1))
                    nil))
                nil))
            nil))
        (let ((defs (convert-for-var-def init-start)))
          (if defs
            (let ((condition (convert-expression (check-string ";" (list-index defs 1)))))
              (if condition
                (let ((iterate (convert-expression (check-string ";" (list-index condition 1)))))
                  (if iterate
                    (let ((body (convert-statement (check-string ")" (list-index iterate 1)))))
                      (if body
                        (list (concatenate 'string "(let " (car defs) (car init) '(#\linefeed)
                                           "(loop" '(#\linefeed) "(when (not " (car condition) ") (return))"
                                           '(#\linefeed) (car body) '(#\linefeed) (car iterate) "))")
                              (list-index body 1))
                        nil))
                    nil))
                nil))
            nil))))))


(defun convert-statement (tokens)
  (format t "~a ~a~%" "convert-statement"  tokens)
  (let ((try-if (convert-if tokens))) (if try-if try-if
    (let ((try-var-def (convert-var-def tokens))) (if try-var-def try-var-def
      (let ((try-block (convert-statement-block tokens))) (if try-block try-block
        (let ((try-return (convert-return tokens))) (if try-return try-return
          (let ((try-while (convert-while tokens))) (if try-while try-while
            (let ((try-for (convert-for tokens))) (if try-for try-for
              (let ((try-expression (convert-expression tokens))) 
                (if try-expression
                  (let ((remaining (check-string ";" (list-index try-expression 1))))
                    (if remaining
                      (list (car try-expression) remaining)
                      nil))
                  nil)))))))))))))))

(defun convert-statement-list (tokens)
  (format t "~a ~a~%" "convert-statement-list"  tokens)
  (let ((statement (convert-statement tokens)))
    (if statement
      (let ((other-statements (convert-statement-list (list-index statement 1))))
        (if (car other-statements)
          (list (concatenate 'string (car statement) '(#\linefeed) (car other-statements)) (list-index other-statements 1))
          statement))
      (list nil tokens))))

(defun convert-func-params (tokens)
  (format t "~a ~a~%" "convert-func-params"  tokens)
  (let ((remaining (check-typename (check-const tokens))))
    (if remaining
      (let ((name (car remaining)) (other-params (convert-func-params (check-string "," (cdr remaining)))))
        (if (car other-params)
          (list (concatenate 'string name " " (car other-params)) (list-index other-params 1))
          (list name (cdr remaining))))
      (list nil tokens))))

(defun convert-func-def (tokens)
  (format t "~a ~a~%" "convert-func-def" tokens)
  (let ((remaining (check-typename (check-const tokens))))
    (if remaining
      (let ((name (car remaining)) (params (convert-func-params (check-string "(" (cdr remaining)))))
        (let ((body (convert-statement-list (check-string "{" (check-string ")" (list-index params 1))))))
          (if (and body (check-string "}" (list-index body 1)))
            (list (concatenate 'string "(defun " name " (" (car params) ")"
              '(#\linefeed) (car body) ")" '(#\linefeed))
                  (check-string "}" (list-index body 1)))
            nil)))
      nil)))

(defun convert-global (tokens)
  (format t "~a ~a~%" "convert-global" tokens)
  (let ((try-func-def (convert-func-def tokens))) (if try-func-def try-func-def nil)))

(defun convert-program (tokens)
  (format t "~a ~a~%" "convert-program" tokens)
  (let ((global (convert-global tokens)))
    (if global
      (let ((the-rest (convert-program (list-index global 1))))
        (if the-rest
          (concatenate 'string (car global) '(#\linefeed) the-rest)
          (car global)))
      nil)))

(let ((in (open "a.c")))
  (let ((tokens (tokenize ((substitute #\Space #\tab read-entire-file in))))
    (write-file "out.lisp" 
      (concatenate 'string (if (not (is-one-of "printf" tokens)) ""
        (concatenate 'string "(defun printf (f &rest vals) (apply #'format t f vals))" '(#\linefeed #\linefeed)))
        (convert-program tokens) '(#\linefeed) "(main)")))
  (close in))
