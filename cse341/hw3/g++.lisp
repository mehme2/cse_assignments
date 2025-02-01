(defun checkRule (rule code)
  (eval (append rule (list code))))

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

(defun bestMatch (rules code)
  (if rules
    (let ((remain (checkRule (car (car rules)) code))
          (bestOfRest (bestMatch (cdr rules) code)))
      (if remain
        (let ((res (subseq code 0 (- (length code) (length remain)))))
          (if (> (length (car (cdr bestOfRest))) (length res))
            bestOfRest
            (list (car (cdr (car rules))) res)))
        bestOfRest))))

(defconstant DT_ANY -1)
(defconstant DT_FRACTION 1)
(defconstant DT_INTEGER 2)
(defconstant DT_BOOLEAN 3)
(defconstant DT_LIST 4)
(defconstant DT_STRING 5)
(defconstant DT_FUNCTION 6)

(defun castData (type data)
  (let ((orgType (car data)))
    (cond
      ((equal orgType type) data)
      ((equal DT_ANY type) data)
      ((and (equal type DT_FRACTION) (equal orgType DT_INTEGER)) (list DT_FRACTION (car (cdr data)) 1))
      ((and (equal type DT_INTEGER) (equal orgType DT_FRACTION)) (list DT_INTEGER (floor (/ (car (cdr data)) (car (cdr (cdr data)))))))
      ((and (equal type DT_BOOLEAN) (equal orgType DT_LIST)) (list DT_BOOLEAN (> (length (car (cdr data))) 0)))
      ((equal type DT_BOOLEAN) (list DT_BOOLEAN t)))))

(defun compareData (a b)
  (if (not (equal (car a) (car b)))
    (let ((cast1 (castData (car a) b))
          (cast2 (castData (car b) a)))
      (if cast1 (compareData cast1 a)
        (if cast2 (compareData cast2 b))))
    (equal a b)))

(defun printDataList (lst)
  (printData (car lst))
  (if (cdr lst) 
    (progn (format t ", ")
           (printDataList (cdr lst)))))

(defun printData (data)
  (let ((type (car data)))
    (cond
      ((equal type DT_FRACTION) (format t "~a:~a" (car (cdr data)) (car (cdr (cdr data)))))
      ((equal type DT_INTEGER) (format t "~a" (car (cdr data))))
      ((equal type DT_BOOLEAN) (format t "~a" (if (car (cdr data)) "TRUE" "FALSE")))
      ((equal type DT_LIST) (if (car (cdr data))
                              (progn
                                (format t "(")
                                (printDataList (car (cdr data)))
                                (format t ")"))
                              (format t "NIL")))
      ((equal type DT_STRING) (format t "\"~a\"" (car (cdr data)))))))

(defun simplifyFrac (a)
  (let ((top (car (cdr a)))
        (bottom (car (cdr (cdr a)))))
    (let ((d (gcd top bottom)))
      (list DT_FRACTION (floor (/ top d)) (floor (/ bottom d))))))

(defun addFrac (a b)
  (let ((atop (car (cdr a)))
        (abottom (car (cdr (cdr a))))
        (btop (car (cdr b)))
        (bbottom (car (cdr (cdr b)))))
    (simplifyFrac (list DT_FRACTION (+ (* atop bbottom) (* btop abottom)) (* abottom bbottom)))))

(defun subFrac (a b)
  (let ((atop (car (cdr a)))
        (abottom (car (cdr (cdr a))))
        (btop (car (cdr b)))
        (bbottom (car (cdr (cdr b)))))
    (simplifyFrac (list DT_FRACTION (- (* atop bbottom) (* btop abottom)) (* abottom bbottom)))))

(defun mulFrac (a b)
  (let ((atop (car (cdr a)))
        (abottom (car (cdr (cdr a))))
        (btop (car (cdr b)))
        (bbottom (car (cdr (cdr b)))))
    (simplifyFrac (list DT_FRACTION (* atop btop) (* abottom bbottom)))))

(defun divFrac (a b)
  (let ((atop (car (cdr a)))
        (abottom (car (cdr (cdr a))))
        (btop (car (cdr b)))
        (bbottom (car (cdr (cdr b)))))
    (if (not (= bbottom 0))
      (simplifyFrac (list DT_FRACTION (* atop bbottom) (* abottom btop)))
      (format t "ERROR: Division by zero~%"))))

(defun lessFrac (a b)
  (let ((atop (car (cdr a)))
        (abottom (car (cdr (cdr a))))
        (btop (car (cdr b)))
        (bbottom (car (cdr (cdr b)))))
    (< (* atop bbottom) (* btop abottom))))

(defun addFracList (lst)
  (if lst
    (addFrac (car lst) (addFracList (cdr lst)))
    (list DT_FRACTION 0 1)))

(defun mulFracList (lst)
  (if lst
    (mulFrac (car lst) (mulFracList (cdr lst)))
    (list DT_FRACTION 1 1)))

(defun evaluateExpression (context expression)
  (if context
    (eval (append (list (car expression)) (list (list 'quote context)) (mapcar (lambda (x) (list 'quote x)) (cdr expression))))))

(defun andList (lst)
  (if lst
    (list DT_BOOLEAN (and (car (cdr (car lst))) (car (cdr (andList (cdr lst))))))
    (list DT_BOOLEAN t)))

(defun orList (lst)
  (if lst
    (list DT_BOOLEAN (or (car (cdr (car lst))) (car (cdr (orList (cdr lst))))))
    (list DT_BOOLEAN nil)))

(defun checkArguments (arglist checks)
  (let ((check (car checks))
        (expression (car arglist)))
    (if (and check expression)
      (let ((checkType (car check))
            (checkAmt (car (cdr check))))
        (let ((casted (castData checkType expression)))
          (if casted
            (let ((nextCheck (if (equal checkAmt 1)
                               (cdr checks)
                               (if checkAmt
                                 (append (list (list checkType (- checkAmt 1)))
                                         (cdr checks))
                                 checks))))
              (if nextCheck
                (let ((other (checkArguments (cdr arglist) nextCheck)))
                  (if (and checkAmt (> checkAmt 1) (not other)) nil
                    (append (list casted) other)))
                (if (cdr arglist) nil
                  (list casted))))))))))

(defun VALUEF (context tb) (list context (append (list DT_FRACTION) tb)))

(defun VALUEI (context val) (list context (list DT_INTEGER val)))

(defun KW_AND (context explist)
  (let ((args (evaluateExpression context explist)))
    (let ((casted (checkArguments (car (cdr args)) (list (list DT_BOOLEAN)))))
      (if (equal (length casted) (length (car (cdr args))))
        (list (car args) (andList casted))
        (format t "ERROR: Invalid arguments~%")))))

(defun KW_OR (context explist)
  (let ((args (evaluateExpression context explist)))
    (let ((casted (checkArguments (car (cdr args)) (list (list DT_BOOLEAN)))))
      (if (equal (length casted) (length (car (cdr args))))
        (list (car args) (orList casted))
        (format t "ERROR: Invalid arguments~%")))))

(defun KW_NOT (context explist)
  (let ((args (evaluateExpression context explist)))
    (let ((casted (checkArguments (car (cdr args)) (list (list DT_BOOLEAN 1)))))
      (if casted
        (list (car args) (list DT_BOOLEAN (not (car (cdr (car casted))))))
        (format t "ERROR: Invalid arguments~%")))))

(defun KW_EQUAL (context explist)
  (let ((args (evaluateExpression context explist)))
    (if (not (equal 2 (length (car (cdr args)))))
      (format t "ERROR: Invalid arguments~%")
      (list (car args) (list DT_BOOLEAN (compareData (car (car (cdr args))) (car (cdr (car (cdr args))))))))))

(defun KW_LESS (context explist)
  (let ((args (evaluateExpression context explist)))
    (let ((casted (checkArguments (car (cdr args)) (list (list DT_FRACTION 2)))))
      (if casted
        (list (car args) (list DT_BOOLEAN (apply #'lessFrac casted)))
        (format t "ERROR: Invalid arguments~%")))))

(defun KW_NIL (context) (list context (list DT_LIST nil)))

(defun KW_LIST (context explist)
  (let ((args (evaluateExpression context explist)))
    (if args
      (list (car args) (list DT_LIST (car (cdr args)))))))

(defun KW_APPEND (context explist)
  (let ((args (evaluateExpression context explist)))
    (let ((casted (checkArguments (car (cdr args)) (list (list DT_LIST)))))
      (if casted
        (list (car args) (list DT_LIST (apply 'append (mapcar (lambda (x) (car (cdr x))) casted))))
        (format t "ERROR: Invalid arguments~%")))))

(defun KW_CONCAT (context explist)
  (let ((args (evaluateExpression context explist)))
    (let ((casted (checkArguments (car (cdr args)) (list (list DT_STRING)))))
      (if casted
        (list (car args) (list DT_STRING (apply 'concatenate (append '(string) (mapcar (lambda (x) (car (cdr x))) casted)))))
        (format t "ERROR: Invalid arguments~%")))))

(defun getVarSub (subcontext name)
  (if subcontext
    (let ((var (car subcontext)))
      (if (equal name (car var))
        (car (cdr var))
        (getVarSub (cdr subcontext) name)))))

(defun getVar (context name)
  (if context
    (let ((sub (car context)))
      (let ((res (getVarSub sub name)))
        (if res res
          (getVar (cdr context) name))))))

(defun insertVarSub (subcontext name data)
  (append (list (list name data)) subcontext))

(defun insertVar (context name data)
  (append (list (insertVarSub (car context) name data)) (cdr context)))

(defun setVarSub (subcontext name data)
  (if subcontext
    (let ((var (car subcontext)))
      (if (equal name (car var))
        (let ((casted (castData (car (car (cdr var))) data)))
          (if casted
            (append (list (list name casted)) (cdr subcontext))))
        (let ((other (setVarSub (cdr subcontext) name data)))
          (if other
            (append (list var) other)))))))

(defun setVar (context name data)
  (if context
    (let ((sub (car context)))
      (let ((res (setVarSub sub name data)))
        (if res
          (append (list res) (cdr context))
          (let ((other (setVar (cdr context) name data)))
            (if other
              (append (list sub) other))))))))

(defun KW_DEFFUN (context identifier idlist explist)
  (if (getVarSub (car context) (car (cdr identifier)))
    (format t "ERROR: Variable already exists~%")
    (let ((func (list DT_FUNCTION idlist explist)))
      (list (insertVar context (car (cdr identifier)) func) func))))

(defun KW_DEFVAR (context identifier explist)
  (let ((args (evaluateExpression context explist)))
    (if (equal 1 (length (car (cdr args))))
      (if (getVarSub (car (car args)) (car (cdr identifier)))
        (format t "ERROR: Variable already exists~%")
        (list (insertVar (car args) (car (cdr identifier)) (car (car (cdr args)))) (car (car (cdr args)))))
      (format t "ERROR: Invalid arguments~%"))))

(defun KW_SET (context identifier explist)
  (let ((args (evaluateExpression context explist)))
    (if (equal 1 (length (car (cdr args))))
      (let ((newContext (setVar (car args) (car (cdr identifier)) (car (car (cdr args))))))
        (if newContext
          (list newContext (car (car (cdr args)))))))))

(defun forSub (context name idx to explist)
  (if (< idx to)
    (let ((res (evaluateExpression (append (list (list (list name (list DT_INTEGER idx)))) context) explist)))
      (if res
        (let ((next (+ 1 idx)))
          (if (< next to)
            (forSub (cdr (car res)) name next to explist)
            (list (cdr (car res)) (car (last (car (cdr res)))))))))
    (list context (list DT_LIST nil))))

(defun KW_FOR (context identifier from to explist)
  (let ((args (evaluateExpression context (list 'EXPLIST (list from to)))))
    (let ((casted (checkArguments (car (cdr args)) (list (list DT_INTEGER 2)))))
      (if casted
        (forSub (car args) (car (cdr identifier)) (car (cdr (car casted))) (car (cdr (car (cdr casted)))) explist)))))

(defun KW_WHILE (context condition explist &optional last)
  (let ((condev (evaluateExpression (if last context (append (list nil) context)) condition)))
    (if condev
      (if (car (cdr (car (cdr condev))))
        (let ((res (evaluateExpression (car condev) explist)))
          (if res
            (evaluateExpression (car res) (list 'KW_WHILE condition explist (car (last (car (cdr res))))))))
        (if last
          (list (cdr (car condev)) (car (cdr last)))
          (list (cdr (car condev)) (DT_LIST nil))))
      (format t "ERROR: Invalid arguments~%"))))

(defun KW_IF (context condition explist)
  (let ((condev (evaluateExpression context condition))
        (nExp (length (car (cdr explist)))))
    (if (and condev (> nExp -1) (< nExp 3))
      (if (car (cdr (car (cdr condev))))
        (evaluateExpression (car condev) (car (car (cdr explist))))
        (if (equal nExp 2)
          (evaluateExpression (car condev) (car (cdr (car (cdr explist)))))
          (list (car condev) (list DT_LIST nil))))
      (format t "ERROR: Invalid arguments~%"))))

(defun KW_EXIT (context explist) (list nil (list DT_INTEGER 0)))

(defun KW_LOAD (context explist)
  (let ((args (evaluateExpression context explist)))
    (let ((casted (checkArguments (car (cdr args)) (list (list DT_STRING 1)))))
      (if casted (gppinterpreter (car (cdr (car casted))) context)))))

(defun KW_DISP (context explist)
  (let ((args (evaluateExpression context explist)))
    (if (not (equal (length (car (cdr args))) 1))
      (format t "ERROR: Invalid arguments~%")
      (progn
        (printData (car (car (cdr args))))
        (format t "~%")
        (list (car args) (car (cdr args)))))))

(defun KW_TRUE (context) (list context (list DT_BOOLEAN t)))

(defun KW_FALSE (context) (list context (list DT_BOOLEAN nil)))

(defun OP_PLUS (context explist)
  (let ((args (evaluateExpression context explist)))
    (let ((casted (checkArguments (car (cdr args)) (list (list DT_FRACTION)))))
      (if (equal (length casted) (length (car (cdr args))))
        (list (car args) (addFracList casted))
        (format t "ERROR: Invalid arguments~%")))))

(defun OP_MINUS (context explist)
  (let ((args (evaluateExpression context explist)))
    (let ((casted (checkArguments (car (cdr args)) (list (list DT_FRACTION 2) (list DT_FRACTION)))))
      (if (and (equal (length casted) (length (car (cdr args)))) (> (length casted) 1))
        (list (car args) (subFrac (car casted) (addFracList (cdr casted))))
        (format t "ERROR: Invalid arguments~%")))))

(defun OP_MULT (context explist)
  (let ((args (evaluateExpression context explist)))
    (let ((casted (checkArguments (car (cdr args)) (list (list DT_FRACTION)))))
      (if (equal (length casted) (length (car (cdr args))))
        (list (car args) (mulFracList casted))
        (format t "ERROR: Invalid arguments~%")))))

(defun OP_DIV (context explist)
  (let ((args (evaluateExpression context explist)))
    (let ((casted (checkArguments (car (cdr args)) (list (list DT_FRACTION 2) (list DT_FRACTION)))))
      (if (and (equal (length casted) (length (car (cdr args)))) (> (length casted) 1))
        (list (car args) (divFrac (car casted) (mulFracList (cdr casted))))
        (format t "ERROR: Invalid arguments~%")))))

(defun OP_OP (context) (format t "OP_OP~a~%" (list context)))
(defun OP_CP (context) (format t "OP_CP~a~%" (list context)))
(defun OP_COMMA (context) (format t "OP_COMMA~a~%" (list context)))

(defun TOKENSTR (context name) (list context (list DT_STRING (subseq name 1 (- (length name) 1)))))

(defun IDENTIFIER (context name)
  (let ((data (getVar context name)))
    (if data
      (list context data)
      (format t "ERROR: Variable is not found~%"))))

(defun defineParameters (context idlist arglist)
  (if (and idlist arglist)
    (defineParameters (insertVar context (car (cdr (car idlist))) (car arglist)) (cdr idlist) (cdr arglist))
    (if (or idlist arglist) nil
      context)))

(defun CALL (context identifier explist)
  (let ((func (evaluateExpression context identifier)))
    (let ((args (evaluateExpression (car func) explist)))
      (if (and args (equal (car (car (cdr func))) DT_FUNCTION))
        (let ((newContext (defineParameters (append (list nil) (car args)) (car (cdr (car (cdr (car (cdr func)))))) (car (cdr args)))))
          (if newContext
            (let ((res (evaluateExpression newContext (car (cdr (cdr (car (cdr func))))))))
              (if res
                (list (cdr (car res)) (car (last (car (cdr res)))))))))
        (format t "ERROR: Invalid call~%")))))

(defun EXPLIST (context lst)
  (if lst
    (let ((res (evaluateExpression context (car lst))))
      (if res
        (if (cdr lst)
          (let ((other (evaluateExpression (car res) (list 'EXPLIST (cdr lst)))))
            (if other
              (list (car other) (append (cdr res) (car (cdr other))))))
          (list (car res) (cdr res)))))
    (list context nil)))

(defun IDLIST (context lst) (format t "IDLIST~a~%" (list context lst)))

(defun nextToken (code)
  (bestMatch (list
               (list '(ruleOneOrMore '(ruleDigit))
                     (lambda (res) (list 'VALUEI (parse-integer res))))
               (list '(ruleChain '((ruleOneOrMore '(ruleDigit))
                                   (ruleOr '((ruleChar #\f) (ruleChar #\:)))
                                   (ruleOneOrMore '(ruleDigit))))
                     (lambda (res) (list 'VALUEF
                                         (list (parse-integer res :junk-allowed t)
                                               (parse-integer (subseq res (1+ (or (position #\f res) (position #\: res)))))))))
               (list '(ruleChain '((ruleChar #\")
                                   (ruleZeroOrMore '(ruleOr '((ruleString "\\\"")
                                                              (ruleCharNot #\"))))
                                   (ruleChar #\")))
                     (lambda (res) (list 'TOKENSTR res)))
               (list '(ruleString "and") (lambda (res) '(KW_AND)))
               (list '(ruleString "or") (lambda (res) '(KW_OR)))
               (list '(ruleString "not") (lambda (res) '(KW_NOT)))
               (list '(ruleString "equal") (lambda (res) '(KW_EQUAL)))
               (list '(ruleString "less") (lambda (res) '(KW_LESS)))
               (list '(ruleString "nil") (lambda (res) '(KW_NIL)))
               (list '(ruleString "list") (lambda (res) '(KW_LIST)))
               (list '(ruleString "append") (lambda (res) '(KW_APPEND)))
               (list '(ruleString "concat") (lambda (res) '(KW_CONCAT)))
               (list '(ruleString "deffun") (lambda (res) '(KW_DEFFUN)))
               (list '(ruleString "defvar") (lambda (res) '(KW_DEFVAR)))
               (list '(ruleString "set") (lambda (res) '(KW_SET)))
               (list '(ruleString "for") (lambda (res) '(KW_FOR)))
               (list '(ruleString "while") (lambda (res) '(KW_WHILE)))
               (list '(ruleString "if") (lambda (res) '(KW_IF)))
               (list '(ruleString "exit") (lambda (res) '(KW_EXIT)))
               (list '(ruleString "load") (lambda (res) '(KW_LOAD)))
               (list '(ruleString "print") (lambda (res) '(KW_DISP)))
               (list '(ruleString "true") (lambda (res) '(KW_TRUE)))
               (list '(ruleString "false") (lambda (res) '(KW_FALSE)))
               (list '(ruleString "+") (lambda (res) '(OP_PLUS)))
               (list '(ruleString "-") (lambda (res) '(OP_MINUS)))
               (list '(ruleString "/") (lambda (res) '(OP_DIV)))
               (list '(ruleString "*") (lambda (res) '(OP_MULT)))
               (list '(ruleString "(") (lambda (res) '(OP_OP)))
               (list '(ruleString ")") (lambda (res) '(OP_CP)))
               (list '(ruleString ",") (lambda (res) '(OP_COMMA)))
               (list '(ruleString "'") (lambda (res) '(OP_QUOTE)))
               (list '(ruleChain '((ruleAlpha)
                                   (ruleZeroOrMore '(ruleOr '((ruleAlpha) (ruleDigit) (ruleChar #\_))))))
                     (lambda (res) (list 'IDENTIFIER res))))
             (ruleZeroOrMore '(ruleOr '((ruleChain '((ruleString ";;")
                                                     (ruleZeroOrMore '(ruleCharNot #\linefeed))))
                                        (ruleChar #\Space) (ruleChar #\tab))) code)))

(defun tokenize (code)
  (let ((token (nextToken code)))
    (if token
      (let ((text (car (cdr token))) (node (apply (car token) (cdr token))))
        (let ((remain (subseq code (+ (length text) (search text code)))))
          (if (> (length remain) 0)
            (let ((other (tokenize remain)))
              (if other (cons node other)))
            (list node)))))))

(defun cdrn (n lst)
  (if (> n 0)
    (cdrn (- n 1) (cdr lst))
    lst))

(defun checkSentence (sentence tokens)
;;(format t "(checkSentence ~a)~%" (list sentence tokens))
  (eval (append sentence (list (list 'quote tokens)))))

(defun sentenceTokenList (checks tokens)
;(format t "(sentenceTokenList ~a)~%" (list checks tokens))
  (if (eq (car (car tokens)) (car checks))
    (if (cdr checks)
      (let ((other (sentenceTokenList (cdr checks) (cdr tokens))))
        (if (listp other)
          (list (append (list (car tokens)) (car other)))
          (1+ other)))
      (list (list (car tokens))))
    0))

(defun bestSentenceMatch (rules tokens)
;(format t "(bestSentenceMatch ~a)~%" (list rules tokens))
  (let ((res (checkSentence (car (car rules)) tokens))
        (other (if (cdr rules) (bestSentenceMatch (cdr rules) tokens) 0)))
    ;;(format t "~a~%" (car rules))
    ;;(format t "~a vs ~a~%" res other)
    (if (listp res)
      (if (and (listp other) (> (length (car other)) (length (car res))))
        other
        (list (car res) (apply (car (cdr (car rules))) res)))
      (if (listp other)
        other
        (max res other)))))

(defun sentenceChain (sentences tokens)
;(format t "(sentenceChain ~a)~%" (list sentences tokens))
  (let ((sentence (car sentences)))
    (if sentence
      (let ((res (checkSentence sentence tokens)))
        (if (listp res)
          (if (cdr sentences)
            (let ((other (sentenceChain (cdr sentences) (cdrn (length (car res)) tokens))))
              (if (listp other)
                (list (append (car res) (car other)) (append (cdr res) (cdr other)))
                (+ (length (car res)) other)))
            res)
          res)))))

(defun sentenceEmpty (tokens)
;(format t "(sentenceEmpty ~a)~%" (list tokens))
nil)

(defun sentenceExpressionList (tokens)
;(format t "(sentenceExpressionList ~a)~%" (list tokens))
  (bestSentenceMatch (list
                       (list '(sentenceEmpty)
                             (lambda (&optional tokens children) '(EXPLIST nil)))
                       (list '(sentenceChain '((sentenceExpression)
                                               (sentenceExpressionList)))
                             (lambda (tokens children)
                               (list 'EXPLIST (append (list (car children)) (car (cdr (car (cdr children)))))))))
                     tokens))

(defun sentenceIdentifierList (tokens)
;(format t "(sentenceIdentifierList ~a)~%" (list tokens))
  (bestSentenceMatch (list
                       (list '(sentenceEmpty)
                             (lambda (&optional tokens children) '(IDLIST nil)))
                       (list '(sentenceChain '((sentenceTokenList '(IDENTIFIER))
                                               (sentenceIdentifierList)))
                             (lambda (tokens children)
                               (list 'IDLIST (append (list (car tokens)) (car (cdr (car children))))))))
                     tokens))

(defun sentenceValues (tokens)
;(format t "(sentenceExpressionList ~a)~%" (list tokens))
  (bestSentenceMatch (list
                       (list '(sentenceEmpty)
                             (lambda (&optional tokens children) '(EXPLIST nil)))
                       (list '(sentenceExpression)
                             (lambda (tokens children) (list 'EXPLIST (list children))))
                       (list '(sentenceChain '((sentenceExpression)
                                               (sentenceTokenList '(OP_COMMA))
                                               (sentenceValues)))
                             (lambda (tokens children)
                               (list 'EXPLIST (append (list (car children)) (car (cdr (car (car (cdr children))))))))))
                     tokens))

(defun sentenceOperation (tokens)
;(format t "(sentenceOperation ~a)~%" (list tokens))
  (bestSentenceMatch (list
                       (list '(sentenceChain '((sentenceTokenList '(KW_DEFFUN IDENTIFIER OP_OP))
                                               (sentenceIdentifierList)
                                               (sentenceTokenList '(OP_CP))))
                             (lambda (tokens children)
                               (append '(KW_DEFFUN) (list (car (cdr tokens))) (car children))))
                       (list '(sentenceTokenList '(KW_DEFVAR IDENTIFIER))
                             (lambda (tokens) 
                               (append '(KW_DEFVAR) (cdr tokens))))
                       (list '(sentenceTokenList '(KW_SET IDENTIFIER))
                             (lambda (tokens) 
                               (append '(KW_SET) (cdr tokens))))
                       (list '(sentenceChain '((sentenceTokenList '(KW_FOR OP_OP IDENTIFIER))
                                               (sentenceExpression)
                                               (sentenceExpression)
                                               (sentenceTokenList '(OP_CP))))
                             (lambda (tokens children)
                               (list 'KW_FOR (car (cdrn 2 tokens)) (car (car children)) (car (car (cdr (car children)))))))
                       (list '(sentenceChain '((sentenceTokenList '(KW_WHILE))
                                               (sentenceExpression)))
                             (lambda (tokens children)
                               (append '(KW_WHILE) children)))
                       (list '(sentenceChain '((sentenceTokenList '(KW_IF))
                                               (sentenceExpression)))
                             (lambda (tokens children)
                               (append '(KW_IF) children)))
                       (list '(sentenceTokenList '(OP_PLUS))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(OP_MINUS))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(OP_MULT))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(OP_DIV))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(KW_AND))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(KW_OR))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(KW_NOT))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(KW_EQUAL))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(KW_LESS))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(KW_LIST))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(KW_APPEND))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(KW_CONCAT))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(KW_EXIT))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(KW_LOAD))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(KW_DISP))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceExpression)
                             (lambda (tokens children)
                               (list 'CALL children))))
                     tokens))

(defun sentenceExpression (tokens)
;(format t "(sentenceExpression ~a)~%" (list tokens))
  (bestSentenceMatch (list
                       (list '(sentenceTokenList '(VALUEF))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(VALUEI))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(TOKENSTR))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(IDENTIFIER))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(KW_TRUE))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(KW_FALSE))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceTokenList '(KW_NIL))
                             (lambda (tokens) (car tokens)))
                       (list '(sentenceChain '((sentenceTokenList '(OP_QUOTE OP_OP))
                                               (sentenceValues)
                                               (sentenceTokenList '(OP_CP))))
                             (lambda (tokens children)
                               (append '(KW_LIST) (car children))))
                       (list '(sentenceChain '((sentenceTokenList '(OP_OP))
                                               (sentenceOperation)
                                               (sentenceExpressionList)
                                               (sentenceTokenList '(OP_CP))))
                             (lambda (tokens children)
                               ;(print (list tokens children))
                               (append (car (car children)) (car (cdr (car children)))))))
                     tokens))

(defun analyze (f prevTokens context res)
  (let ((line (read-line f nil)))
    (if line
      (let ((tokens (append prevTokens (tokenize line))))
        (if tokens
          (let ((explist (sentenceExpressionList tokens)))
            (if (listp explist)
              (let ((res (evaluateExpression context (car (cdr explist))))
                    (remain (cdrn (length (car explist)) tokens)))
                (analyze f remain (car res) res))
              (if (equal (length tokens) explist)
                (analyze f tokens context res)
                (analyze f nil context res))))
          (analyze f nil context res)))
      (list context (car (cdr res))))))

(defun gppinterpreter (&optional file context)
  (let ((cont (if context context (list nil))))
    (if file (with-open-file (in file) (analyze in nil cont nil)) (analyze t nil cont nil))))

(gppinterpreter)
