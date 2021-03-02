(load "functions.lisp")
(setf *random-state* (make-random-state t)) ;Seed random number generator

(defun run-tests()
    "Runs unit tests.  Returns true if all succeed.  Nil if something fails."
    (format t "~%~%RUNNING TESTS......~%")
    (setq success 0)
    (setq fail 0)


    (setq function-name "create-card")
    (format t "  Testing ~a~%" function-name)
    (setq card (create-card "spades" "ace"))
    (setq suit (getf card :suit))
    (setq val (getf card :val))
    (setq test-string "Suit is spades.")
    (if (string-equal suit "spades")
        (progn 
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq test-string "Val is ace.")
    (if (string-equal val "ace")
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf success)) 
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))


    (setq function-name "create-deck")
    (format t "  Testing ~a~%" function-name)
    (setq deck (create-deck))
    (setq test-string "Deck has 52 cards.")
    (if (= (list-length deck) 52)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))


    (setq function-name "create-shoe")
    (format t "  Testing ~a~%" function-name)
    (setq shoe (create-shoe 8))
    (setq test-string "Shoe has 416 cards.")
    (if (= (list-length shoe) 416)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))


    (setq function-name "remove-item-from-list")
    (format t "  Testing ~a~%" function-name)
    (setq new-deck (remove-item-from-list deck 0))
    (setq test-string "Removed item from deck.")
    (if (= (list-length new-deck) 51)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq new-shoe (remove-item-from-list shoe 0))
    (setq test-string "Removed item from shoe.")
    (if (= (list-length new-shoe) 415)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))


    (setq function-name "shuffle-cards")
    (format t "  Testing ~a~%" function-name)
    (setq new-deck (shuffle-cards deck))
    (setq test-string "Deck is shuffled.")
    (if (not (equal deck new-deck))
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq test-string "Shuffled deck has same num cards.")
    (if (= (list-length deck) (list-length new-deck))
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))


    (setq total (+ success fail))
    (format t "~%")
    (format t "  ~a of ~a tests passed.  ~a% pass rate.~%" success total (* (/ success total) 100))
    (if (> fail 0)
        (format t "  ~a of ~a tests failed.  ~a% failure rate.~%" fail total (* (/ fail total) 100)))
    (format t "~%~%")
    (if (> fail 0) 
        (return-from run-tests nil) 
        (return-from run-tests T)))

(run-tests)
