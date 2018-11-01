(DEFUN GENERATE(level)
    (SETF *random-state* (make-random-state t))
    (SETQ continue (RANDOM 4))                                      ;1 to 3 = nested, 0 = constant/var
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
(DEFUN PURGE(original survivorCount)
    (SETF fitList ())
    (LOOP for count from 0 to 49                        ;create list of fitness of pool
        do
        (SETF fitList (APPEND fitList (LIST (ABS (EVAL (NTH count original))))))
    )
    (SORT fitList '<)                                   ;sort fitness list, to get nth fitness
    (SETF survivors ())
    (LOOP for count from 0 to 49                        ;populate n survivors with exprs with fitness <= topFit
            WHILE (<= (LIST-LENGTH survivors) survivorCount)
        do
        (IF (<= (ABS (EVAL (NTH count original))) (NTH survivorCount fitList)) ;if less than n survivors and survivor has acceptable fitness
            (SETF survivors (APPEND survivors (LIST (NTH count original))))
        )
    )
    (RETURN-FROM PURGE survivors)
)

;MAIN
(SETF Y 4)                  ;set XYZ and expected
(SETF X 2)
(SETF Z 3)
(SETF expected 15)
(SETF pool ())                          ;initial pool
(LOOP for count from 0 to 49            ;initial population
    do
    (SETF pool (APPEND pool (LIST (GENERATE 0))))
)
(SETF nextGen (purge pool 24))          ;purge, keep 25
(LOOP for count from 0 to 24            ;test, print survivors
    do
    (PRINT (NTH count nextGen))
)