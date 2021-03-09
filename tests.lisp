(load "functions.lisp")
(setf *random-state* (make-random-state t)) ;Seed random number generator

(defun run-tests()
    "Runs unit tests.  Returns true if all succeed.  Nil if something fails."
    (format t "~%~%RUNNING TESTS......~%")
    (setq success 0)
    (setq fail 0)


    (setq function-name "create-card")
    (format t "  Testing ~a~%" function-name)
    (setq card (create-card `spades `ace))
    (setq suit (getf card :suit))
    (setq val (getf card :val))
    (setq test-string "Suit is spades.")
    (if (eq suit (quote spades))
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


    (setq function-name "remove-card-from-deck")
    (format t "  Testing ~a~%" function-name)
    (setq new-deck (remove-card-from-deck deck 0))
    (setq test-string "Removed item from deck.")
    (if (= (list-length new-deck) 51)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq new-shoe (remove-card-from-deck shoe 0))
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


    (setq function-name "deal-card")
    (format t "  Testing ~a~%" function-name)
    (multiple-value-setq (card deck) (deal-card (create-deck)))
    (setq test-string "Card was dealt.")
    (if card
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq test-string "Deck has 1 less card.")
    (if (= (list-length deck) 51)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))


    (setq function-name "get-num-val-card")
    (format t "  Testing ~a~%" function-name)
    (multiple-value-setq (val-1 val-2) (get-num-val-card (create-card `spades `ace)))
    (setq test-string "Correct value(s) returned.")
    (if (and (= val-1 1) (= val-2 11))
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (multiple-value-setq (val-1 val-2) (get-num-val-card (create-card `clubs `9)))
    (setq test-string "Correct value(s) returned.")
    (if (and (= val-1 9) (eq val-2 nil))
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))


    (setq function-name "calculate-hand-value")
    (format t "  Testing ~a~%" function-name)
    (setq hand ())
    (push (create-card `spades `jack) hand)
    (push (create-card `spades `ace) hand)
    (multiple-value-setq (val-1 val-2) (calculate-hand-value hand))
    (setq test-string "Should be blackjack")
    (if (and (= val-1 21) (eq val-2 nil))
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq hand ())
    (push (create-card `spades `jack) hand)
    (push (create-card `spades `6) hand)
    (push (create-card `spades `7) hand)
    (multiple-value-setq (val-1 val-2) (calculate-hand-value hand))
    (setq test-string "Should be a bust hand.")
    (if (and (= val-1 23) (eq val-2 nil))
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq hand ())
    (push (create-card `spades `ace) hand)
    (push (create-card `spades `ace) hand)
    (push (create-card `spades `3) hand)
    (push (create-card `spades `ace) hand)
    (push (create-card `spades `10) hand)
    (push (create-card `spades `10) hand)
    (multiple-value-setq (val-1 val-2) (calculate-hand-value hand))
    (setq test-string "Should be a bust hand.")
    (if (and (= val-1 26) (eq val-2 nil))
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq hand ())
    (push (create-card `spades `ace) hand)
    (push (create-card `spades `3) hand)
    (multiple-value-setq (val-1 val-2) (calculate-hand-value hand))
    (setq test-string "Should be a soft 14.")
    (if (and (= val-1 4) (= val-2 14))
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq hand ())
    (push (create-card `spades `jack) hand)
    (push (create-card `spades `queen) hand)
    (multiple-value-setq (val-1 val-2) (calculate-hand-value hand))
    (setq test-string "Should be a hard 20.")
    (if (and (= val-1 20) (eq val-2 nil))
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))



    (setq function-name "get-hand-vals")
    (format t "  Testing ~a~%" function-name)
    (setq hand ())
    (push (create-card `spades `2) hand)
    (push (create-card `spades `ace) hand)
    (push (create-card `spades `jack) hand)
    (push (create-card `spades `10) hand)
    (push (create-card `spades `8) hand)
    (setq vals (get-hand-vals hand))
    (setq test-string "List should contain 2 ACE JACK 10 8.")
    (setq val-1 (find '2 vals))
    (setq val-1 (find 'ace vals))
    (setq val-1 (find 'jack vals))
    (setq val-1 (find '10 vals))
    (setq val-1 (find '8 vals))
    (if val-1
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))




    (setq total (+ success fail))
    (format t "~%")
    (format t "  ~c[32m~a of ~a tests passed.  ~a% pass rate.~c[0m~%" #\ESC success total (round (* (/ success total) 100.0)) #\ESC)
    (if (> fail 0)
        (format t "  ~c[31m~a of ~a tests failed.  ~a% failure rate.~c[0m~%" #\ESC fail total (round (* (/ fail total) 100.0)) #\ESC))
    (format t "~%~%")
    (if (> fail 0) 
        (return-from run-tests nil) 
        (return-from run-tests T)))

(run-tests)
