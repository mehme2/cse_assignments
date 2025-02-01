(defconstant keywords (list
                        '("and"      "KW_AND")
                        '("or"       "KW_OR")
                        '("not"      "KW_NOT")
                        '("equal"    "KW_EQUAL")
                        '("less"     "KW_LESS")
                        '("nil"      "KW_NIL")
                        '("list"     "KW_LIST")
                        '("append"   "KW_APPEND")
                        '("concat"   "KW_CONCAT")
                        '("deffun"   "KW_DEFFUN")
                        '("for"      "KW_FOR")
                        '("if"       "KW_IF")
                        '("exit"     "KW_EXIT")
                        '("load"     "KW_LOAD")
                        '("print"    "KW_DISP")
                        '("true"     "KW_TRUE")
                        '("false"    "KW_FALSE")))

(defconstant operators (list
                        '("+"        "OP_PLUS")
                        '("-"        "OP_MINUS")
                        '("/"        "OP_DIV")
                        '("*"        "OP_MULT")
                        '("("        "OP_OP")
                        '(")"        "OP_CP")
                        '(","        "OP_COMMA")))

(defun cross (a b)
  (if (and a b)
    (append
      (append (list (list (car a) (car b))) (cross (list (car a)) (cdr b)))
      (cross (cdr a) b))))

(defun strDiff (a b)
  (subseq a 0 (- (length a) (length b))))

(defun checkRule (rule code)
  (eval (append rule (list code))))

(defun processToken (type val)
  (let ((realType (if (string= type "IDENTIFIER")
                    (eval (append '(or) (mapcar (lambda (x) (if (string= (car x) val) (car (cdr x)))) keywords) (list type)))
                    (if (string= type "OPERATOR")
                      (eval (append '(or) (mapcar (lambda (x) (if (string= (car x) val) (car (cdr x)))) operators) (list type)))
                      type))))
    (format t "~a~%" realType)))

(defun ruleZeroOrMore (rule code)
  (let ((remain (checkRule rule code)))
    (if remain (ruleZeroOrMore rule remain) code)))

(defun ruleOneOrMore (rule code)
  (let ((remain (checkRule rule code)))
    (if remain (ruleZeroOrMore rule remain))))

(defun ruleOr (rules code)
  (if (and code rules)
    (let ((remain (checkRule (car rules) code)))
      (or remain (ruleOr (cdr rules) code)))))

(defun ruleChain (rules code)
  (if rules
    (let ((remain (checkRule (car rules) code)))
      (if remain (ruleChain (cdr rules) remain)))
    code))

(defun ruleChar (c code)
  (and (> (length code) 0) (eq (char code 0) c) (subseq code 1)))

(defun ruleCharNot (c code)
  (and (> (length code) 0) (not (eq (char code 0) c)) (subseq code 1)))

(defun ruleAlpha (code)
  (and (> (length code) 0) (alpha-char-p (char code 0)) (subseq code 1)))

(defun ruleDigit (code)
  (and (> (length code) 0) (digit-char-p (char code 0)) (subseq code 1)))

(defun ruleString (s code)
  (if code
    (if (> (length s) 0)
      (ruleString (subseq s 1) (ruleChar (char s 0) code))
      code)))

(defun ruleInteger (code)
  (ruleOneOrMore '(ruleDigit) code))

(defun ruleWhiteSpace (code)
  (ruleOneOrMore '(ruleOr (cross '(ruleChar) '(#\linefeed #\Space #\tab))) code))

(defun ruleComment (code)
  (let ((remain (ruleChain '((ruleString ";;")
                             (ruleZeroOrMore '(ruleCharNot #\linefeed))
                             (ruleChar #\linefeed)) code)))
    (if remain (processToken "COMMENT" (strDiff code remain)))
    remain))

(defun ruleOp (l code)
  (let ((remain (ruleOr (cross '(ruleString) (mapcar 'car operators)) code)))
    (if remain (processToken "OPERATOR" (strDiff code remain)))
    remain))

(defun ruleIdentifier (code)
  (let ((remain (ruleChain
                  '((ruleAlpha)
                    (ruleZeroOrMore
                      '(ruleOr '((ruleAlpha)
                                 (ruleDigit)
                                 (ruleChar #\_)))))
                           code)))
    (if remain (processToken "IDENTIFIER" (strDiff code remain)))
    remain))

(defun ruleValueI (code)
  (let ((remain (ruleInteger code)))
    (if remain 
      (progn (processToken "VALUEI" (strDiff code remain))
             (if (not (ruleOr (append '((ruleAlpha)) (cross '(ruleChar) '(#\_ #\:))) remain))
               remain)))))

(defun ruleValueF (code)
  (let ((remain (ruleChain
                  '((ruleInteger)
                    (ruleOr (cross '(ruleChar) '(#\: #\f)))
                    (ruleInteger))
                  code)))
    (if remain (processToken "VALUEF" (strDiff code remain)))
    remain))

(defun analyze (f)
  (let ((line (read-line f nil)))
    (if line
      (let ((remain (ruleZeroOrMore
                      '(ruleOr '((ruleWhiteSpace)
                                 (ruleOp operators)
                                 (ruleIdentifier)
                                 (ruleValueF)
                                 (ruleValueI)
                                 (ruleComment)))
                      (concatenate 'string line '(#\linefeed)))))
        (if (> (length remain) 0)
          (format t "SYNTAX_ERROR Invalid token at:~%V~%~a" remain))
        (analyze f)))))

(defun gppinterpreter (&optional file)
  (if file (with-open-file (in file) (analyze in)) (analyze t)))

(gppinterpreter)
