;line types
(defconstant typeFuncDef       0) ;function declarations
(defconstant typeFuncDec       1) ;function prototypes
(defconstant typeVarDefLocal   2) ;local variable definitions
(defconstant typeVarDefGlobal  3) ;global variable definitions
(defconstant typeVarDef       23) ;used when whether if the variable definition is local or global is unknown
(defconstant typeIf            4) ;if statements
(defconstant typeFor           5) ;for statements
(defconstant typeWhile         6) ;while statements
(defconstant typeExpression    7) ;expressions (anything that returns a value e.g. function calls, operations, identifiers)
(defconstant typeReturn        8) ;return statements
(defconstant typeBlockStart    9) ;start of a statement block {
(defconstant typeBlockEnd     10) ;end of a statement block
(defconstant typeNone         11) ;lines to be ignored

;keywords and operators
;even though many keywords are included, some of them are not supported
;type definitions, structs, arrays, and pointers are also not supported
(defconstant keywords (list "int" "char" "float" "void" "double" "if" "for"
                            "enum" "else" "while" "switch" "case" "struct"
                            "typedef" "sizeof" "static" "extern" "short"
                            "long" "unsigned" "const" "break" "default"
                            "do" "signed" "auto" "union" "return"))
(defconstant typeModifiers (list "const" "unsigned" "short" "long" "*"
                                 "static" "extern" "signed"))
(defconstant typeNames (list "int" "char" "void" "float" "double" "short" "long"))
(defconstant typeNamesConverted (list "integer" "character" "null"
                                      "float" "float"
                                      "integer" "integer"))
(defconstant binaryOps (list "*" "/" "+=" "-=" "==" "<=" ">=" "=" "+" "-"
                             "<" ">" "!=" "&&" "||" "&" "|" "^"))
(defconstant binaryOpsConverted (list "*" "/" "incf" "decf" "eq" "<=" ">=" "setf" "+" "-"
                                      "<" ">" "not (eq" "and" "or" "logand" "logior" "logxor"))

;returns ith element of list l
(defun listIndex (l i)
  (if i (if (> i 0) (listIndex (cdr l) (- i 1)) (car l)) nil))

;returns index of string s in l or nil if it does not exist
(defun findIndex (s l)
  (if l
    ;check first element
    (if (string= s (car l)) 0
      ;check next element if not found
      (let ((subIndex (findIndex s (cdr l))))
        (if subIndex
          (+ 1 subIndex)
          nil)))
    nil))

;returns s if string s is a member of l
(defun isOneOf (s l)
  (if l
    ;check first element
    (if (string= s (car l))
      (car l)
      ;check next element if not found
      (isOneOf s (cdr l)))
    nil))

;cleans whitespace from left of string s
(defun cleanLeft (s)
  (string-left-trim '(#\Space #\tab) s))

;cleans whitespace from left of string s
(defun cleanRight (s)
  (string-right-trim '(#\Space #\tab) s))

;count unclosed parantheses in given string
(defun countUnclosed (s)
  (if (= 0 (length s)) 0
    (let ((next (char s 0)))
      (if (eq next #\()
        (let ((remain (skipParanthesis (subseq s 1))))
          (if remain
            (countUnclosed remain)
            (+ 1 (countUnclosed (subseq s 1)))))
        (countUnclosed (subseq s 1))))))

;recursive part of extractName
;the reason for this function is because
;identifiers can have numbers in their names but not at the start
(defun extractNameRest (s)
  (if (not (= 0 (length s)))
    (let ((firstChar (char s 0)))
      (if (or (eq firstChar #\$) (eq firstChar #\_) (alphanumericp firstChar))
        (concatenate 'string (string firstChar)
                     (extractNameRest (subseq s 1)))
        nil))))

;returns the next word in s if it is a identifier or keyword
(defun extractName (s)
  ;if s is not empty
  (if (not (= 0 (length s)))
    (let ((clean (cleanLeft s)))
      ;if clean is not empty
      (if (not (= 0 (length clean)))
        (let ((firstChar (char clean 0)))
          ;if the first char is alphabetic or it is $ or _ (can be used for identifiers)
          (if (or (eq firstChar #\$) (eq firstChar #\_) (alpha-char-p firstChar))
            ;call the recursive part for next characters and add to end
            (concatenate 'string (string firstChar)
                         (extractNameRest (subseq clean 1)))
            nil))))
    nil))

;returns the type with modifiers at start of s if it exists
(defun extractType (s)
  ;get next word
  (let ((name (extractName s)))
    (let ((remaining (subseq (cleanLeft s) (length name))))
      ;if name is a modifier
      (if (isOneOf name typeModifiers)
        ;call with remaining string again to get the full type
        (concatenate 'string name " " (checkType remaining))
        ;if it is a type name
        (if (isOneOf name typeNames)
          name
          nil)))))

;skip functions
;they return the given string s without an element ignoring whitespace
;returns nil if wanted element is not found

;skips the first identifier
(defun skipIdentifier (s)
  (let ((id (extractName s)))
    (if (and id (not (isOneOf id keywords)))
      (subseq (cleanLeft s) (length id))
      nil)))

;skips the first type including its modifiers
(defun skipType (s)
  (let ((type (extractType s)))
    (if type
      (subseq (cleanLeft s) (length type))
      nil)))

;skips the first instance of given string w
;skips even if w is at start of a word
(defun skipAny (w s)
  (let ((clean (cleanLeft s)))
    (if (> (length w) (length clean)) nil
      ;check the same number of characters as w
      (if (string= w (subseq clean 0 (length w)))
        (subseq clean (length w))
        nil))))

;skipAny but from last
(defun skipFromLast (w s)
  (let ((clean (cleanRight s)))
    ;get the ending substring of same length as w
    (let ((start (- (length clean) (length w))))
      (if (string= w (subseq clean start))
        (subseq clean 0 start)
        nil))))

;skips the first identifier or keyword if it matches w
(defun skipName (w s)
  (let ((name (extractName s)))
    (let ((remaining (subseq (cleanLeft s) (length name))))
      (if (string= w name)
        remaining
        nil))))

;skips the first match in list l
(defun skipOneOf (l s)
  (if (not l) nil
    (let ((tryNext (skipAny (car l) s)))
      (if tryNext tryNext
        (skipOneOf (cdr l) s)))))

;skips inside and ending double quote of a string
;s must not include the starting double quote
(defun skipString (s)
  (if (= 0 (length s))
    nil
    (let ((next (char s 0)))
      (if (eq next #\")
        (subseq s 1)
        ;ignore the character after \ (can be double quote)
        (if (eq next #\\)
          (skipString (subseq s 2))
          (skipString (subseq s 1)))))))

;skips inside and ending of a paranthesis
;s must not include the left paranthesis
(defun skipParanthesis (s)
  (if (= 0 (length s))
    nil
    (let ((next (char s 0)))
      (if (eq next #\))
        (subseq s 1)
        ;skip sub parantheses
        (if (eq next #\()
          (skipParanthesis (skipParanthesis (subseq s 1)))
          ;ignore parantheses inside strings
          (if (eq next #\")
            (skipParanthesis (skipString (subseq s 1)))
            (skipParanthesis (subseq s 1))))))))

;skips until after the first instance of w
(defun skipUntil (w s)
  (if (= 0 (length (cleanLeft s)))
    nil
    (let ((check (skipAny w s)))
      (if check check
        ;ignore string
        (if (eq #\" (char s 0))
          (skipUntil w (skipString (subseq (cleanLeft s) 1)))
          (skipUntil w (subseq (cleanLeft s) 1)))))))

;skipConvert functions
;returns a list in the following format:
;(*wanted element converted* *rest of the string after the element*)
;some of them assume the element exists

;skips and converts expressions inside a paranthesis
;seperated by commas to following format: "exp1 exp2 ... expn"
;returns empty string if paranthesis is empty
(defun skipConvertCallParams (s)
  ;if paranthesis ended
  (if (skipAny ")" s) (list "" (skipAny ")" s))
    (let ((param (skipConvertExpression s)))
      (let ((otherStart (skipAny "," (listIndex param 1))))
        (if otherStart
          (let ((other (skipConvertCallParams otherStart)))
            (list (concatenate 'string (car param) " " (car other))
                  (listIndex other 1)))
          param)))))

;skips and converts a function call
(defun skipConvertCall (s)
  (let ((params (skipConvertCallParams (skipUntil "(" s))))
    (let ((name (extractName s)))    ;convert printf to format t
      (list (concatenate 'string "(" (if (string= name "printf") "format t" name)
                         " " (car params) ")")
            (listIndex params 1)))))

;skips and converts a decimal number literal
;does not ignore whitespace
(defun skipConvertNumber (s)
  ;if s is not empty
  (if (> (length s) 0)
    (let ((firstChar (char s 0)))
      (if (digit-char-p firstChar)
        ;rest of the number
        (let ((other (skipConvertNumber (subseq s 1))))
          (if other
            (list (concatenate 'string (string firstChar) (car other))
                  (listIndex other 1))
            (list (string firstChar) (subseq s 1))))
        (if (eq #\. firstChar)
          (let ((other (skipConvertNumber (subseq s 1))))
            (list (concatenate 'string "." (car other))
                  (listIndex other 1)))
          (if (eq #\f firstChar)
            (list "" (subseq s 1))
            (list "" s)))))))

;skips and converts a char literal
;does not ignore whitespace
(defun skipConvertChar (s)
  (let ((firstChar (char s 1)))
    (if (eq firstChar #\\)
      (let ((secondChar (char s 2)))
        (list
          (cond
            ((eq secondChar #\\) "#\\\\")
            ((eq secondChar #\n) "#\\linefeed")
            ((eq secondChar #\t) "#\\tab")
            ((eq secondChar #\Space) "#\\Space")
            (t (concatenate 'string "#\\" (string secondChar))))
          (subseq s 4)))
      (list
        (cond
          ((eq firstChar #\Space) "#\\Space")
          ((eq firstChar #\~) "#\\~~")
          (t (concatenate 'string "#\\" (string firstChar))))
        (subseq s 3)))))

;skips and converts inside and ending of a string
;also converts the c format to lisp format
;s must not include the starting double quote
;does not ignore whitespace
;returns (nil nil) if string is empty
(defun skipConvertString (s)
  ;if string is not empty
  (if (> (length s) 0)
    (let ((next (char s 0)))
      (cond 
        ;if string ended
        ((eq next #\")
         (list "\"" (subseq s 1)))
        ;check the character after \ (skips double quote)
        ((eq next #\\)
         (let ((nextNext (char s 1))
               (remaining (skipConvertString (subseq s 2))))
           (list (concatenate 'string (cond
                                        ;convert new line to lisp new line
                                        ((eq nextNext #\n) "~~%")
                                        (t (concatenate 'string "\\"
                                                        (string nextNext))))
                              (car remaining))
                 (listIndex remaining 1))))
        ;convert c format to lisp format
        ((eq next #\%)
         (let ((nextNext (char s 1))
               (remaining (skipConvertString (subseq s 2))))
           (list (concatenate 'string (cond
                                        ((eq nextNext #\%) "%")
                                        ((eq nextNext #\s) "~~a")
                                        (t (concatenate 'string "~~" (string nextNext))))
                              (car remaining))
                 (listIndex remaining 1))))
        (t (let ((remaining (skipConvertString (subseq s 1))))
             (list (concatenate 'string (if (eq next #\~) "~~" (string next))
                                (car remaining)) (listIndex remaining 1))))))
  (list nil nil)))

;checks the type of next literal in s
;and skips and converts it
;can return nil
(defun skipConvertLiteral (s)
  (let ((clean (cleanLeft s)))
    (if (> (length clean) 0)
      (let ((firstChar (char clean 0)))
        (if (or (digit-char-p firstChar) (eq #\. firstChar))
          (skipConvertNumber clean)
          (if (eq #\" firstChar)
            (let ((str (skipConvertString (subseq clean 1))))
              (list (concatenate 'string "\"" (car str)) (listIndex str 1)))
            (if (eq #\' firstChar)
              (skipConvertChar clean)
              nil)))))))

;expression conversion
;by expression i mean anything that returns a value

;skips and converts the next expression in s
;excluding binary operations
;can return nil or (nil nil)
(defun skipConvertExpressionNoBinOp (s)
  (let ((testId (skipIdentifier s))
        (id (extractName s)))
    ;if s starts with a identifier
    (if testId
      ;if parantesis after identifier (function call)
      (if (skipAny "(" testId)
        (skipConvertCall s)
        (let ((testInc (skipAny "++" testId)))
          ;if post increment
          (if testInc
            (list (concatenate 'string "(let ((prev " id ")) (incf " id") prev)")
                  testInc)
            (let ((testDec (skipAny "--" testId)))
              ;if post decrement
              (if testDec
                (list (concatenate 'string "(let ((prev " id ")) (decf " id") prev)")
                      testDec)
                ;return the identifier alone if none of the above matches
                (list id testId)))))
        )
      (let ((testInc (skipAny "++" s)))
        ;if pre increment
        (if testInc
          (list (concatenate 'string "(incf " (extractName testInc) ")")
                (skipIdentifier testInc))
          (let ((testDec (skipAny "--" s)))
            ;if pre decrement
            (if testDec
              (list (concatenate 'string "(decf " (extractName testDec) ")")
                    (skipIdentifier testDec))
              (let ((testPar (skipAny "(" s)))
                ;if expression inside paranthesis
                (if testPar
                  ;c can have multiple expressions inside paranthesis like following:
                  ;(exp1, exp2, ... ,expn)
                  ;and paranthesis returns value of the final expression
                  (list (concatenate 'string "(progn " (car (skipConvertCallParams testPar)) ")")
                        (skipParanthesis testPar))
                  ;try to convert literal if none of the above matches
                  (skipConvertLiteral s))))))))))

;skips and converts the next expression in s
;including binary operations
;can return nil or (nil nil)
(defun skipConvertExpression (s)
  ;left operand if it is a binary op
  ;the expression itself if no binary op
  (let ((left (skipConvertExpressionNoBinOp s)))
    ;check if it is a binary op
    (let ((rightStart (skipOneOf binaryOps (listIndex left 1))))
      (if rightStart
        (let ((clean (cleanLeft (listIndex left 1))))
          ;the operator symbol and the right operand
          (let ((op (subseq clean 0 (- (length clean) (length rightStart))))
                (right (skipConvertExpression rightStart)))
            (list (concatenate 'string "(" (listIndex binaryOpsConverted (findIndex op binaryOps))
                         " " (car left) " " (car right) ")" (if (string= "!=" op) ")"))
                  (listIndex right 1))))
        left))))

;convert functions
;returns the lisp code converted from c code
;most of them assume the element exists

;returns the next expression converted
;can return nil
(defun convertExpression (line)
  (car (skipConvertExpression line)))

;converts the following string:
;"ctype1 identifier1, ctype2 identifier2, ... ctypen identifiern"
;to:
;"identifier1, identifier2, ... identifiern"
;returns empty string if there are no parameters
(defun convertFuncDefParams (s)
  (let ((name (extractName (skipType s)))
        (other (skipAny "," (skipIdentifier (skipType s)))))
    (if name
      (if other
        (concatenate 'string name " " (convertFuncDefParams other))
        name)
      "")))

;converts the given function definition
;assumes the type, name and parameters are in the given line
(defun convertFuncDef (line)
  (concatenate 'string "(defun " (extractName (skipType line)) " ("
               (convertFuncDefParams (skipAny "(" (skipIdentifier (skipType line)))) ")"))

;converts the given c type in s to lisp type
;ignores the modifiers before the type (just reads the final word after the seperating space)
(defun convertType (s)
  (let ((type (extractType s)))
    (let ((lastSpace (position #\Space type :from-end t)))
      (let ((raw (if lastSpace (subseq type (+ 1 lastSpace)) type)))
        (listIndex typeNamesConverted (findIndex raw typeNames))))))

;converts the following string:
;"ctype1 identifier1, ctype2 identifier2, ... ctypen identifiern" (identifiers are optional)
;to:
;"lisptype1, lisptype2, ... lisptypen"
;returns empty string if there are no parameters
(defun convertFuncDecParams (s)
  (let ((type (convertType s))
        (other (skipAny "," (or (skipIdentifier (skipType s)) (skipType s)))))
    (if other
      (concatenate 'string type " " (convertFuncDecParams other))
      type)))

;converts the given function declaration prototype
;assumes the type, name and parameters are in the given line
(defun convertFuncDec (line)
  (concatenate 'string "(declaim (ftype (function ("
               (convertFuncDecParams (skipAny "(" (skipIdentifier (skipType line))))
               ") " (convertType line) ") " (extractName (skipType line)) "))"))

;converts the given local variable definition
;assumes there are only one variable per line
;(will only convert the first variable in: int a = 1, b = 2;)
;gives a seperate let statement for each variable definition line
;because the following does not work in lisp:
;(let ((x 5)
;      (y 1)
;      (sum (+ x y)))
;because x and y are not initalized while initalizing sum
(defun convertVarDefLocal (line)
  (let ((name (extractName (skipType line)))
        (valStart (skipAny "=" (skipIdentifier (skipType line)))))
    (concatenate 'string "(let ((" name
                 ;if there is an assignment
                 (if valStart (concatenate 'string " " (convertExpression valStart)) "")
                 "))")))

;converts the given global variable definition
;same rules for the local version applies here as well
(defun convertVarDefGlobal (line)
  (let ((name (extractName (skipType line)))
        (valStart (skipAny "=" (skipIdentifier (skipType line)))))
    (concatenate 'string "(defvar " name
                 ;if there is an assignment
                 (if valStart (concatenate 'string " " (convertExpression valStart)) "")
                 ")")))

;converts the given line after checking its type
;this function is only used for converting the
;statement after if, while, or for
;to convert the statement at the end of same line
;(they are always assumed to have { or a statement at the end of line)
;the main loop has a seperate but very similar conversion process
(defun convertStatement (line)
  (let ((converted (convert (conversion-foo (line-type line)) line)))
    ;close the parent statement's starting paranthesis if it is a single line statement
    (if (> (countUnclosed converted) 0) converted
      (concatenate 'string converted ")"))))

;converts the given if statement to lisp if
(defun convertIf (line)
  (let ((conditionStart (skipAny "(" (skipName "if" line))))
  (concatenate 'string "(if " (convertExpression conditionStart) '(#\linefeed)
               (convertStatement (skipParanthesis conditionStart)))))

;converts the given while statement to lisp loop while
(defun convertWhile (line)
  (let ((conditionStart (skipAny "(" (skipName "while" line))))
  (concatenate 'string "(loop while " (convertExpression conditionStart) " do~%"
               (convertStatement (skipParanthesis conditionStart)))))

;converts the given for statement to lisp do
;the -unusedVar- definition in output is used for
;the increment part and initalizer part if there is no variable definition
;reasons for using do:
;can have variable definitions (similar to c)
;can have intializer, conditional, and increment parts as expressions (similar to c)
;the said parts are all placed before the statements (perfect for reading line by line)
(defun convertFor (line)
  (let ((initStart (skipAny "(" (skipName "for" line))))
    (let ((nameStart (skipType initStart)))
      ;if there is a variable definition in initalizer part
      ;cannot convert multiple variable definitions
      (if nameStart
        (let ((name (extractName nameStart))
              (valStart (skipAny "=" (skipIdentifier nameStart))))
          (let ((val (if valStart (convertExpression valStart) "nil")))
                                      ;variable declaration part
            (concatenate 'string "(do ((" name " " val ")" '(#\linefeed) "(-unusedVar- nil "
                         ;the increment part
                         (convertExpression (skipUntil ";" (skipUntil ";" line))) "))"
                                                ;the condition part
                         '(#\linefeed) "((not " (convertExpression (skipUntil ";" line))
                         "))" '(#\linefeed) (convertStatement (skipParanthesis initstart)))))
        (concatenate 'string "(do ((-unusedVar- " (or (convertExpression initStart) "nil") " "
                     ;the ncrement part
                     (convertExpression (skipUntil ";" (skipUntil ";" line))) "))"
                                            ;the condition part
                     '(#\linefeed) "((not " (convertExpression (skipUntil ";" line))
                     "))" '(#\linefeed) (convertStatement (skipParanthesis initstart)))))))

;converts the given return statement
;return statements should always be at the end of function
;because lisp returns the value of the final statement
(defun convertReturn (line)
  (convertExpression (skipName "return" line)))

;converts a { block start
(defun convertBlockStart (line) "(progn")

;converts a { block start
;returns blank because the closing paratheses are added in the main loop
(defun convertBlockEnd (line) "")

;converts lines to be ignored
;e.g. preprocessor macros, comment lines, blank lines
(defun convertBlank (line) "")

;returns if whether the line is blank or not
;returns t if it is not blank
(defun isCLine (line)
  (let ((clean (cleanLeft line)))
    (if (= 0 (length clean))
      nil
      (let ((firstChar (char clean 0)))
        ;check if first character is # (preprocessor macro)
        ;or / (assumed to be comment line)
        (if (or (eq firstChar #\#) (eq firstChar #\/))
          nil
          t)))))

;returns the type of the given line
(defun line-type(line)
  ;whether the line should be converted
  (if (isCLine line)
    ;check if the line is a declaration (if starts with *type* *identifier*)
    (let ((testDec (skipIdentifier (skipType line))))
      (if testDec
        ;if there is a paranthesis
        (let ((testFunc (skipAny "(" testDec)))
          (if testFunc
            ;if ends in one line (prototype)
            (if (skipFromLast ";" testFunc)
              typeFuncDec
              typeFuncDef)
            ;we dont know if the scope is global or local
            typeVarDef))
        (if (skipName "if" line)
          typeIf
          (if (skipName "for" line)
            typeFor
            (if (skipName "while" line)
              typeWhile
              (if (skipName "return" line)
                typeReturn
                (if (skipAny "{" line)
                  typeBlockStart
                  (if (skipAny "}" line)
                    typeBlockEnd
                    typeExpression))))))))
    ;return none if it is to be ignored
    typeNone))

;returns the conversion function corresponding to the type
(defun conversion-foo (type)
  (listIndex (list #'convertFuncDef #'convertFuncDec #'convertVarDefLocal #'convertVarDefGlobal
                   #'convertIf #'convertFor #'convertWhile #'convertExpression #'convertReturn
                   #'convertBlockStart #'convertBlockEnd #'convertBlank) type))

;calls the given conversion function on the given line
(defun convert (func line)
  (funcall func line))

;read the next line from the given file stream
(defun read_file (f)
  (read-line f nil))

;write the entire string s to the file in path p
(defun write_file (p s)
  (with-open-file (stream p :direction :output :if-exists :supersede)
    (format stream s)))

;the main recursive conversion loop
;f: file stream to read
;c: unclosed paranthesis count list (must be passed as nil at start)
(defun c2clisp (f c)
  (let ((line (read_file f)))
    (if line
      ;can be typeVarDef (must be converted)
      (let ((typeRaw (line-type line)))
        ;we can get the scope by checking c
        (let ((type (if (eq typeRaw typeVarDef) (if c typeVarDefLocal typeVarDefGlobal) typeRaw)))
          (let ((func (conversion-foo type)))
            (let ((converted (convert func line)))
              (if converted
                (let ((unclosed (countUnclosed converted)))
                  ;next count list to pass to next recursion if a new block starts
                  (let ((nextScope (if (> unclosed 0)
                                     ;do not start a new block for local variable definitions
                                     (if (eq type typeVarDefLocal)
                                       (cons (+ unclosed (if c (car c) 0)) (cdr c))
                                       (cons unclosed c))
                                     c)))
                    (if (eq type typeBlockEnd)
                      (concatenate 'string converted 
                                   ;append closing parantheses
                                   (make-string (car c) :initial-element #\))
                                   ;next recursion remove the first in c since the block ended
                                   (c2clisp f (cdr c)))
                      (concatenate 'string '(#\linefeed) converted (c2clisp f nextScope)))))
                nil)))))
      nil)))

(let ((in (open "input.c")))
  (write_file "output.lisp" 
              (concatenate 'string (c2clisp in nil) "~%~%(main)"))
  (close in))
