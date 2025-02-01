; if the given word is a variable name
(defun variablep (word)
  ;(format t "variablep ~a~%" (list word))
  (upper-case-p (char word 0)))

; if the given axiom is a rule like H < B
(defun rulep (axiom)
  ;(format t "rulep ~a~%" (list axiom))
  (string= "<" (cadr axiom)))

; returns true if a substitution can be made between axioms a and b
(defun headmatchp (a b)
  ;(format t "headmatchp ~a~%" (list a b))
  (if (and a b)
    ; if first both words in a and b are not variables they must match
    (and (if (not (or (variablep (car a)) (variablep (car b))))
           (string= (car a) (car b))
           t)
         (headmatchp (cdr a) (cdr b)))
    ; return false if axiom lengths does not match
    (not (or a b))))

; renames each variable in the given axiom with {variable name} {depth}
; used to avoid variable name clashes in recursive searches
(defun avoid_conflict (axiom depth)
  ;(format t "avoid_conflict ~a~%" (list axiom depth))
  (mapcar
    ; a: each sub axiom in the given axiom group
    (lambda (a)
      ; return a itself if a is "<" or something
      (if (listp a)
        (mapcar
          ; b: each word in a
          (lambda (b)
            ; return b itself if it is not a variable
            (if (variablep b)
              (concatenate 'string b (write-to-string depth))
              b))
          a)
        a))
    axiom))

; generate a unifier between two axioms a and b
; each member in the returned list is a variable in a to the corresponding value in b
(defun unify (a b)
  ;(format t "unify ~a~%" (list a b))
  (if (and a b)
    (append
      (if (variablep (car a))
        (list (list (car a) (car b))))
      (unify (cdr a) (cdr b)))))

; same as unify but does not make substitutions between two variables
(defun unify_no_var2var (a b)
  ;(format t "unify ~a~%" (list a b))
  (if (and a b)
    (append
      (if (and (variablep (car a)) (not (variablep (car b))))
        (list (list (car a) (car b))))
      (unify_no_var2var (cdr a) (cdr b)))))

; removes the duplicate variable substitutions in a solution
(defun remove_duplicates (lst)
  ;(format t "remove_duplicates ~a~%" (list lst))
  (if lst
    ; just return t if query is true and there are no substitutions
    (if (equal (car lst) t) t
      (append
        ; do not include current element if it appears later in the list
        (if (not (member (car lst) (cdr lst) :test (lambda (a b) (and (string= (car a) (car b)) (string= (cadr a) (cadr b))))))
          (list (car lst)))
        ; check rest of the list
        (remove_duplicates (cdr lst))))))

; substitutes the variables in rule with given unifier
(defun match (rule unifier)
  (mapcar
    ; x: each axiom in the given rule
    (lambda (x)
      ; return x itself if x "<" or something
      (if (listp x)
        (mapcar
          ; y: each word in x
          (lambda (y)
            ; find a suitable substitution in the given unifier
            (let ((u (car (member y unifier :test (lambda (a b) (string= a (car b)))))))
              ; return the word itself if no substitution is found
              (if u
                (cadr u)
                y)))
          x)
        x))
    rule))

; removes the unifiers that does not appear in query
(defun get_relevant (unifier query)
  ;(format t "get_relevant~a~%" (list unifier query))
  (mapcan 
    ; x: each axiom in the query
    (lambda (x)
      (mapcan
        ; y: each word in x
        (lambda (y)
          ; return nil if y is not a variable
          (if (variablep y)
            (mapcan
              ; z: each variable substitution in the unifier
              (lambda (z)
                ; if y matches the variable name in z and substitution in z is not a variable too
                (if (and (string= y (car z)) (not (variablep (cadr z))))
                  (list (list y (cadr z)))))
              unifier)))
        x))
    query))

(defun fix (solutions)
  (if (equal (car solutions) t) t
    solutions))

; returns t or valid variable values if the given query is true according to the axioms
; can handle more than one variable as a side effect of recursion
; nests the resulting variables one more time than the example because of that
(defun prove_limit (limit axioms query)
  ;(format t "prove_limit ~a~%" (list limit query))
  ; return true if query is empty
  (if query
    ; return nil if limit is reached and could not reduce the query to zero
    (if (> limit 0)
      (fix
        ; get every possible solution by testing every axiom
        (mapcan
          ; a: each axiom
          (lambda (a)
            ; if the head of the axiom mathces the first axiom in the query
            (if (headmatchp (car query) (car a))
              ; recursively check the query by reducing it every step
              (let ((res (prove_limit 
                           (- limit 1) ; reduce the limit in the next recursion
                           axioms ; pass the same axioms
                           (append
                             ; add the body to the query if a is an axiom like H < B
                             (if (rulep a)
                               ; add the limit to the end of every variable in a to avoid conflicts with query
                               (let ((rule (avoid_conflict a limit)))
                                 ; remove the part after ">" and substitute the variables in rule with the first query's
                                 (cddr (match rule (unify (car rule) (car query))))))
                             ; rest of the query substituted with values in head of a if there are any
                             (match (cdr query) (unify_no_var2var (car query) (car a)))))))
                ; return nil if the recursion failed
                (if res 
                  ; add the unifier to the result
                  (let ((u (unify_no_var2var (car query) (car a))))
                    ; return (t) if res does not have any unifiers and is true
                    (if (equal res t) (list (or u t))
                      ; if res has unifiers that might be relevant to the query
                      (mapcar
                        ; b: each unifier group in res
                        (lambda (b)
                          ; remove the duplicate unifiers and get relevant ones to the query
                          ; return t if none remains after the filters
                          (or (remove_duplicates (get_relevant (append u b) query)) t))
                        res)))))))
          axioms)))
    t))

; wrapper to prove_limit
; adjust the limit according to the needed recursion depth
; higher limits tend to get laggy after some amount
(defun prolog_prove (axioms query)
  (prove_limit 20 axioms query))

; the example given in the homework pdf
(let (
      (axioms '(
                ( ("father" "jim" "jill") )
                ( ("mother" "mary" "jill") )
                ( ("father" "samm" "jim") )
                ( ("ancestor" "X" "Y") "<" ("parent" "X" "Y") )
                ( ("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y") )
                ( ("parent" "X" "Y") "<" ("mother" "X" "Y") )
                ( ("parent" "X" "Y") "<" ("father" "X" "Y") ) ) )
      (query1 '( ("ancestor" "X" "jill") ) )
      (query2 '( ("ancestor" "X" "jill") ("mother" "X" "bob") ) ) )
  (print (prolog_prove axioms query1))
  (print (prolog_prove axioms query2))
  )

; some other tests

(let (
  (axioms '(
    ( ("father" "jim" "jill") )
    ( ("mother" "mary" "jill") )
    ( ("father" "samm" "jim") )
    ( ("sibling" "samm" "bob") )
    ( ("father" "bob" "alice") )
  
    ( ("uncle" "X" "Y") "<" ("sibling" "X" "Z") ("father" "Z" "Y") )
    ( ("ancestor" "X" "Y") "<" ("parent" "X" "Y") )
    ( ("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y") )
    ( ("parent" "X" "Y") "<" ("mother" "X" "Y") )
    ( ("parent" "X" "Y") "<" ("father" "X" "Y") ) ) )
  (query '( ("ancestor" "bob" "jill") ))
  )
  (print (prolog_prove axioms query))
)

(let (
  (axioms '(
    ( ("father" "jim" "jill") )
    ( ("mother" "mary" "jill") )
    ( ("father" "samm" "jim") )
    ( ("sibling" "samm" "bob") )
    ( ("father" "bob" "alice") )
  
    ( ("uncle" "X" "Y") "<" ("sibling" "X" "Z") ("father" "Z" "Y") )
    ( ("ancestor" "X" "Y") "<" ("parent" "X" "Y") )
    ( ("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y") )
    ( ("parent" "X" "Y") "<" ("mother" "X" "Y") )
    ( ("parent" "X" "Y") "<" ("father" "X" "Y") ) ) )
  (query '( ("uncle" "samm" "alice") ))
  )
  (print (prolog_prove axioms query))
)

(let (
  (axioms '(
    ( ("father" "jim" "jill") )
    ( ("mother" "mary" "jill") )
    ( ("father" "samm" "jim") )
    ( ("sibling" "samm" "bob") )
    ( ("father" "bob" "alice") )
  
    ( ("uncle" "X" "Y") "<" ("sibling" "X" "Z") ("father" "Z" "Y") )
    ( ("sibling" "X" "Y") "<" ("sibling" "Y" "X") )
    ( ("ancestor" "X" "Y") "<" ("parent" "X" "Y") )
    ( ("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y") )
    ( ("parent" "X" "Y") "<" ("mother" "X" "Y") )
    ( ("parent" "X" "Y") "<" ("father" "X" "Y") ) ) )
  (query '( ("uncle" "bob" "jim") ))
  )
  (print (prolog_prove axioms query))
)

(let (
  (axioms '(
    ( ("father" "jim" "jill") )
    ( ("mother" "mary" "jill") )
    ( ("father" "samm" "jim") )
    ( ("sibling" "samm" "bob") )
    ( ("father" "bob" "alice") )
  
    ( ("uncle" "X" "Y") "<" ("sibling" "X" "Z") ("father" "Z" "Y") )
    ( ("ancestor" "X" "Y") "<" ("parent" "X" "Y") )
    ( ("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y") )
    ( ("parent" "X" "Y") "<" ("mother" "X" "Y") )
    ( ("parent" "X" "Y") "<" ("father" "X" "Y") ) ) )
  (query '( ("uncle" "jim" "alice") ))
  )
  (print (prolog_prove axioms query))
)
(let (
  (axioms '(
    ( ("father" "jim" "jill") )
    ( ("mother" "mary" "jill") )
    ( ("father" "samm" "jim") )
    ( ("sibling" "samm" "bob") )
    ( ("father" "bob" "alice") )
  
    ( ("uncle" "X" "Y") "<" ("sibling" "X" "Z") ("father" "Z" "Y") )
    ( ("ancestor" "X" "Y") "<" ("parent" "X" "Y") )
    ( ("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y") )
    ( ("parent" "X" "Y") "<" ("mother" "X" "Y") )
    ( ("parent" "X" "Y") "<" ("father" "X" "Y") ) ) )
  (query '( ("ancestor" "X" "alice") ("sibling" "X" "bob") ))
  )
  (print (prolog_prove axioms query))
)
(let (
  (axioms '(
    ( ("father" "jim" "jill") )
    ( ("mother" "mary" "jill") )
    ( ("father" "samm" "mary") )
    ( ("parent" "jim" "alice") )
    ( ("parent" "mary" "alice") )
  
    ( ("grandparent" "X" "Y") "<" ("parent" "X" "Z") ("parent" "Z" "Y") )
    ( ("parent" "X" "Y") "<" ("mother" "X" "Y") )
    ( ("parent" "X" "Y") "<" ("father" "X" "Y") )
    ( ("sibling" "X" "Y") "<" ("father" "Z" "X") ("father" "Z" "Y") )
    ( ("ancestor" "X" "Y") "<" ("parent" "X" "Y") )
    ( ("ancestor" "X" "Y") "<" ("ancestor" "X" "Z") ("ancestor" "Z" "Y") ) ) )
  (query '( ("grandparent" "X" "Y") ))
  )
  (print (prolog_prove axioms query))
)
(let (
  (axioms '(
    ( ("father" "jim" "jill") )
    ( ("father" "bob" "alice") )
    ( ("parent" "bob" "alice") )
    ( ("sibling" "jim" "bob") )
  
    ( ("cousin" "X" "Y") "<" ("sibling" "Z" "W") ("father" "Z" "X") ("father" "W" "Y") )
    ( ("cousin" "X" "Y") "<" ("sibling" "W" "Z") ("father" "Z" "X") ("father" "W" "Y") )
    ( ("sibling" "X" "Y") "<" ("father" "Z" "X") ("father" "Z" "Y") ) ) )
  (query '( ("cousin" "X" "Y") ))
  )
  (print (prolog_prove axioms query))
)

(let (
      (axioms '((("legs" "X" "2") "<" ("mammal" "X") ("arms" "X" "2"))
                (("legs" "X" "4") "<" ("mammal" "X") ("arms" "X" "0"))
                (("mammal" "horse"))
                (("mammal" "human"))
                (("arms" "human" "2"))
                (("arms" "horse" "0"))))
      (query '(("legs" "X" "Y"))))
  (print (prolog_prove axioms query)))

 ;; Axioms
(let (
	(axioms '(
		(("father" "jim" "jill"))
		(("mother" "anna" "jill"))
		(("father" "sam" "jim"))
		(("mother" "lisa" "anna"))
		(("father" "john" "sam"))
		(("parent" "X" "Y") "<" ("father" "X" "Y"))
		(("parent" "X" "Y") "<" ("mother" "X" "Y"))
		(("ancestor" "X" "Y") "<" ("parent" "X" "Y"))
		(("ancestor" "X" "Y") "<" ("parent" "X" "Z") ("ancestor" "Z" "Y"))))
	(query1 '( ("ancestor" "X" "jill") ) )
	(query2 '( ("parent" "X" "jill") ) ))
	(print (prolog_prove axioms query1))
	(print (prolog_prove axioms query2))
)


;; output: (("X" "jim") ("X" "anna") ("X" "sam") ("X" "lisa") ("X" "john"))
;; output-2: (("X" "jim") ("X" "anna"))     (print (prolog_prove axioms query)))
;; Axioms
(let (
	(axioms '(
		(("manager" "alice" "bob"))
		(("manager" "bob" "charlie"))
		(("boss" "X" "Y") "<" ("manager" "X" "Y"))
		(("boss" "X" "Y") "<" ("manager" "X" "Z") ("boss" "Z" "Y"))))
	(query1 '( ("boss" "X" "charlie") ) ))
  (print (prolog_prove axioms query1))
)

;; output: (("X" "bob") ("X" "alice"))
