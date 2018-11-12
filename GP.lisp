;Program Name: GP.lisp
;Team: AKJ
;Group names: Amy Nguyen-Dang, Kiren Syed, Jason Lieu
;Contact Info: amyndang@csu.fullerton.edu, kirensyed@csu.fullerton.edu, jasonlieu@csu.fullerton.edu
;Class number: CPSC 481
;This is a genetic programing problem. We will be using genetic programming to find a good approximation to a unknown quadratice arithmetic expression in several variables.
;Each individual in our pool or population will be arithmetic expression. And expression can be composed of operators (+, -, *), variable names (x, y, z) and constant integers (positive and negative).

(DEFUN GENERATE(level)
    (SETF *random-state* (make-random-state t))
    (SETQ continue (RANDOM 2))                                      ;1 to 3 = nested, 0 = constant/var
    (IF (OR (= 0 level) (AND (> continue 0) (> 3 level)))           ;limits degree of tree to 1 - 3
        (LET ((operator (RANDOM 3)))                                  
            (IF (= operator 0)
                (RETURN-FROM GENERATE (LIST '+ (GENERATE(+ level 1)) (GENERATE(+ level 1))))
            )
            (IF (= operator 1)
                (RETURN-FROM GENERATE (LIST '- (GENERATE(+ level 1)) (GENERATE(+ level 1))))
            )
            (IF (= operator 2)
                (RETURN-FROM GENERATE (LIST '* (GENERATE(+ level 1)) (GENERATE(+ level 1))))
            )
        )
        (LET ((choice (RANDOM 8)))                                  ;higher (RANDOM #), higher chance for constants
            (IF (= choice 0)
                (RETURN-FROM GENERATE 'x)
            (IF (= choice 1)
                (RETURN-FROM GENERATE 'y)
            (IF (= choice 2)
                (RETURN-FROM GENERATE 'z)
            (LET ((negative (RANDOM 2)))                             ;return constant -9 to 9
                (IF (= negative 1)
                    (RETURN-FROM GENERATE (- 0 (RANDOM 9)))
                    (RETURN-FROM GENERATE (RANDOM 9))
                )
            ))))
        )
    )
)

(DEFUN PURGE(original expected survivorCount)
    (SETF fitList ())
    (LOOP for count from 0 to 49                        ;create list of fitness of pool
        do
        (SETF fitList (APPEND fitList (LIST (ABS (- expected (EVAL (NTH count original)))))))   ;fitness = |expected - result|, 0 is best
    )
    (SORT fitList '<)                                   ;sort fitness list, to get nth fitness
    (SETF survivors ())
    (LOOP for count from 0 to 49                        ;populate n survivors with exprs with fitness <= topFit
            WHILE (<= (LIST-LENGTH survivors) survivorCount)
        do
        (IF (<= (ABS (- expected (EVAL (NTH count original)))) (NTH survivorCount fitList)) ;if less than n survivors and survivor has acceptable fitness
            (SETF survivors (APPEND survivors (LIST (NTH count original))))
        )
    )
    (RETURN-FROM PURGE survivors)
)

(DEFUN MUTATE(expression)
    (IF (LISTP expression)                              ;only mutate lists, not leaves
        (PROGN
            (SETF *random-state* (make-random-state t))                  
            (IF (= 0 (RANDOM 20))                            ;0 = mutation, 1 - 19 = no mutation, 5%
                (IF (= 0 (RANDOM 2))                  ;0 = leaf, 1 = op
                    (PROGN                              ;leaf mutation
                        (IF (AND (LISTP (NTH 1 expression)) (LISTP (NTH 2 expression)))     ;if no leaves, keep recursing
                            (RETURN-FROM MUTATE (LIST (NTH 0 expression) (MUTATE (NTH 1 expression)) (MUTATE (NTH 2 expression))))
                            (PROGN
                                (SETF leafXYZConst (RANDOM 4))  ;choose leaf replacement: X Y Z [-9 to 9]
                                (IF (= leafXYZConst 0)
                                    (SETF replacement 'X)
                                (IF (= leafXYZConst 1)
                                    (SETF replacement 'Y)
                                (IF (= leafXYZConst 2)
                                    (SETF replacement 'Z)
                                (IF (= (RANDOM 2) 0)
                                        (SETF replacement (RANDOM 9))
                                        (SETF replacement (- 0 (RANDOM 9)))
                                ))))
                                (IF (= (RANDOM 2) 0)            ;choose which leaf to replace
                                    (IF (LISTP (NTH 1 expression))  ;only replace if NTH 1 is a leaf
                                        (RETURN-FROM MUTATE (LIST (NTH 0 expression) (MUTATE (NTH 1 expression)) (MUTATE (NTH 2 expression))))
                                        (RETURN-FROM MUTATE (LIST (NTH 0 expression) replacement (MUTATE (NTH 2 expression))))
                                    )
                                    (IF (LISTP (NTH 2 expression))  ;only replace if NTH 2 is a leaf
                                        (RETURN-FROM MUTATE (LIST (NTH 0 expression) (MUTATE (NTH 1 expression)) (MUTATE (NTH 2 expression))))
                                        (RETURN-FROM MUTATE (LIST (NTH 0 expression) (MUTATE (NTH 1 expression)) replacement ))
                                    )
                                )
                            )
                        )
                    )
                    (PROGN                 ;op mutation
                        (SETF operatorChoice (RANDOM 3))    ;0 = +, 1 = -, 2 = *
                        (IF (= operatorChoice 0)
                            (RETURN-FROM MUTATE (LIST '+ (MUTATE (NTH 1 expression)) (MUTATE (NTH 2 expression))))
                        (IF (= operatorChoice 1)
                            (RETURN-FROM MUTATE (LIST '- (MUTATE (NTH 1 expression)) (MUTATE (NTH 2 expression))))
                        (IF (= operatorChoice 2)
                            (RETURN-FROM MUTATE (LIST '* (MUTATE (NTH 1 expression)) (MUTATE (NTH 2 expression))))
                        )))

                    )
                )
                (RETURN-FROM MUTATE (LIST (NTH 0 expression) (MUTATE (NTH 1 expression)) (MUTATE (NTH 2 expression)))) ;no mutation, keep recursing
            )
        )
        (RETURN-FROM MUTATE expression) ;expression is already a leaf, stop mutation
    )
)

(DEFUN BestAvgWorst (pool expected)
    (SETF fitList ())
    (SETF total 0)
    (SETF baw ())
    (LOOP for count from 0 to 49                        ;create list of fitness of pool
        do
        (SETF fitList (APPEND fitList (LIST (ABS (- expected (EVAL (NTH count pool)))))))   ;fitness = |expected - result|, 0 is best
        (SETF total (+ total (ABS (- expected (EVAL (NTH count pool))))))
    )
    (SORT fitList '<)
    (LOOP for count from 0 to 49
            WHILE (= 0 (LIST-LENGTH baw))
        do
        (IF (= (ABS (- expected (EVAL (NTH count pool)))) (NTH 0 fitList))
            (SETF baw (APPEND baw (LIST (EVAL (NTH count pool)))))
        )
    )
    (SETF baw (APPEND baw (LIST (FLOAT (/ total 50)))))
    (LOOP for count from 0 to 49
            WHILE (= (LIST-LENGTH baw) 2)
        do
        (IF (= (ABS (- expected (EVAL (NTH count pool)))) (NTH 49 fitList))
            (SETF baw (APPEND baw (LIST (EVAL (NTH count pool)))))
        )
    )
    (RETURN-FROM BestAvgWorst baw)
)

(DEFUN CROSSPOINT(p)
    (SETF crossp (RANDOM(- (LIST-LENGTH p) 1)))
    (SETF subl (NTH crossp p))         ; grab node from sub-list for corssover

    ;; return the crosspoint and sublist
    (VALUES crossp subl)
)

(DEFUN FIND-NODE (l pt subnode)
    (COND
        ((NULL l) ())
        ((LISTP l)
            (IF (EQ (NTH pt l) subnode)
                    (PROGN
                    (RETURN-FROM FIND-NODE (NTH pt l))
                ))
        )
        (T (FIND-NODE (CAR l) pt subnode)
            (FIND-NODE (CDR l) pt subnode))
    )
)

(DEFUN MATE (l1 l2 pt1 pt2 subn1 subn2)
    (COND
        ((NULL l1) ())
        ( (LISTP l1)
            (IF (EQ (NTH pt1 l1) subn1)
            (PROGN
                (SETF found (FIND-NODE l2 pt2 subn2))
                ;; if p1 crosspoint is a operator and p2 isn't
                (IF (AND (MEMBER (NTH pt1 l1) `(+ - *)) (NOT (MEMBER found `(+ - *))))
                        (SETF pt1 (+ pt1 2))

                        ;; if p1 crosspoint is not a operator but p2 is
                        (IF (AND (NOT (MEMBER (NTH pt1 l1) `(+ - *)))  (MEMBER found `( + - *)))
                            (SETF pt1 0)
                ))

                (SETF newkid (INSERT-N l1 pt1 found))
                (IF (> (LIST-LENGTH newkid) 3)
                    (PROGN
                        (SETF rm (NTH pt1 l1))
                        (RETURN-FROM MATE (REMOVE rm newkid :count 1))
                    )
                    (RETURN-FROM MATE newkid)
            ))
        ))
         
        (T 
            (MATE (CAR l1) l2 pt1 pt2 subn1 subn2)
            (MATE (CDR l1) l2 pt1 pt2 subn1 subn2)
        )
    )
)

(DEFUN INSERT-N (l n elem)
    (COND
        ((NULL l) ())
        ((= n 0) (CONS elem l))
        (T (CONS (CAR l) (INSERT-N (CDR l) (- n 1) elem)))
    )
)

(DEFUN CROSSOVER(p1 p2)         ; returns a kid

    (SETF *random-state* (make-random-state t))

    ;; SELECT Crosspoint for p1 and p2 and save the sublist
    (SETF crossp1 NIL)
    (SETF crossp2 NIL)

    (SETF sublist1 NIL)
    (SETF sublist2 NIL)

    (MULTIPLE-VALUE-BIND (a b) (CROSSPOINT p1)
        (SETF crossp1 a)
        (SETF sublist1 b)
    )

    (MULTIPLE-VALUE-BIND (a b) (CROSSPOINT p2)
        (SETF crossp2 a)
        (SETF sublist2 b)
    )

    (SETF kid1 (MATE p1 p2 crossp1 crossp2 sublist1 sublist2))
    (SETF kid2 (MATE p2 p1 crossp2 crossp1 sublist2 sublist1))

    (VALUES kid1 kid2)
)

;MAIN
(SETF X 1)                  ;set XYZ and expected
(SETF Y 0)
(SETF Z 2)
(SETF expected 2)
(SETF pool ())                          ;initial pool
(SETF best ())
(SETF BstAvgWrt ())
(LOOP for count from 0 to 49            ;initial population
    do
    (SETF pool (APPEND pool (LIST (GENERATE 0))))
)
(LOOP for generation from 0 to 49
    do
    (SETF best (APPEND best (LIST (PURGE pool expected 0))))   ;purge 49, put top fit of generation in best
    (SETF BstAvgWrt (APPEND BstAvgWrt (LIST (BestAvgWorst pool expected))))
    (SETF nextGen ())                   ;clear out nextGen
    (LOOP for count from 0 to 24        ;mate 0 with 49, 1 with 48, ... 24 with 25
        do
        (MULTIPLE-VALUE-BIND (a b) (CROSSOVER (NTH count pool) (NTH (- 49 count) pool))

            ;; add kids to the next pool
            (SETF nextGen (append nextGen (LIST (MUTATE a))))
            (SETF nextGen (append nextGen (LIST (MUTATE b))))
        )
    )
    (SETF pool nextGen)
)
(LOOP for count from 0 to 49
    do
    (format t "~a. expression: ~a value: ~a  average: ~a  worst: ~a~%" count (NTH count best) (EVAL (NTH 0 (NTH count best))) (NTH 1 (NTH count BstAvgWrt)) (NTH 2 (NTH count BstAvgWrt)))
    ;(format t "    fitness: ~a ---- ~a ---- ~a~%" (ABS (- expected (NTH 0 (NTH count BstAvgWrt)))) (NTH 1 (NTH count BstAvgWrt)) (ABS (- expected (NTH 2 (NTH count BstAvgWrt)))))
)
