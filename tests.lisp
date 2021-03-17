(load "functions.lisp")
(setf *random-state* (make-random-state t)) ;Seed random number generator

(defun run-tests()
    "Runs unit tests.  Returns true if all succeed.  Nil if something fails."
    (format t "~%RUNNING TESTS......~%")
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
    (setq test-string "Invalid suit raised exception.")
    (handler-case 
        (progn 
            (create-card 'stupid '10)
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))
    (setq test-string "Invalid val raised exception.")
    (handler-case 
        (progn 
            (create-card 'clubs '15)
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))


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
    (setq test-string "Invalid num decks raised exception.")
    (handler-case 
        (progn 
            (create-shoe 15)
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))
    (handler-case 
        (progn 
            (create-shoe 0)
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))


    (setq function-name "remove-card-from-deck")
    (format t "  Testing ~a~%" function-name)
    (setq new-deck (remove-card-from-deck deck 0))
    (setq test-string "Removed card from deck.")
    (if (= (list-length new-deck) 51)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq new-shoe (remove-card-from-deck (create-shoe 8) 0))
    (setq test-string "Removed item from shoe.")
    (if (= (list-length new-shoe) 415)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq test-string "Removed item from empty deck.")
    (handler-case 
        (progn 
            (remove-card-from-deck () 0)
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))
    (setq test-string "Removed item using negative index.")
    (handler-case 
        (progn 
            (remove-card-from-deck (create-deck) -1)
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))
    (setq test-string "Removed item using to great an index.")
    (handler-case 
        (progn 
            (remove-card-from-deck (create-deck) 52)
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))


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
    (setq test-string "Shuffle an empty deck.")
    (handler-case 
        (progn 
            (shuffle-cards ())
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))



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
    (setq test-string "Empty deck raised exception")
    (handler-case 
        (progn 
            (multiple-value-setq (card deck) (deal-card ()))
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))



    (setq function-name "get-num-val-card")
    (format t "  Testing ~a~%" function-name)
    (multiple-value-setq (val-1 val-2) (get-num-val-card (create-card `spades `ace)))
    (setq test-string "Returned 1 and 11.")
    (if (and (= val-1 1) (= val-2 11))
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (multiple-value-setq (val-1 val-2) (get-num-val-card (create-card `clubs `9)))
    (setq test-string "Returned 9 and nil.")
    (if (and (= val-1 9) (eq val-2 nil))
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq test-string "Threw exception on nil card.")
    (handler-case 
        (progn 
            (get-num-val-card ())
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))


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
    (setq test-string "Threw exception on empty hand.")
    (handler-case 
        (progn 
            (calculate-hand-value ())
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))
    
    



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
    (setq val-2 (find 'ace vals))
    (setq val-3 (find 'jack vals))
    (setq val-4 (find '10 vals))
    (setq val-5 (find '8 vals))
    (if (and val-1 val-2 val-3 val-4 val-5)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq test-string "Threw exception on empty hand.")
    (handler-case 
        (progn 
            (get-hand-vals ())
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))






    (setq function-name "deck-has-at-least-n-cards")
    (format t "  Testing ~a~%" function-name)
    (setq deck (create-deck))
    (setq test-string "Deck has at least 52 cards.")
    (if (deck-has-at-least-n-cards deck 52)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq test-string "Deck has at least 51 cards.")
    (if (deck-has-at-least-n-cards deck 51)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq test-string "Deck does not have at least 53 cards.")
    (if (not (deck-has-at-least-n-cards deck 53))
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))


    (setq function-name "deal-player-and-dealer")
    (format t "  Testing ~a~%" function-name)
    (setq shoe (create-shoe 2))
    (setq initial-shoe-size (list-length shoe))
    (multiple-value-setq (player-hand dealer-hand shoe) (deal-player-and-dealer shoe))
    (setq test-string "Player was dealt 2 cards.")
    (if ( = (list-length player-hand) 2)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq test-string "Dealer was dealt 2 cards.")
    (if ( = (list-length dealer-hand) 2)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq test-string "Deck has 4 cards less than before deal.")
    (if (= (- initial-shoe-size 4) (list-length shoe))
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))

    (setq function-name "get-player-bet")
    (format t "  Testing ~a~%" function-name)
    (setq test-string "Threw exception on empty chip stack.")
    (handler-case 
        (progn 
            (get-player-bet ())
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))


    (setq function-name "player-play")
    (format t "  Testing ~a~%" function-name)
    (setq card-1 (create-card 'spades 'ace))
    (setq card-2 (create-card 'spades 'ace))
    (setq player-hand ())
    (push card-1 player-hand)
    (setq dealer-hand ())
    (push card-1 dealer-hand)
    (push card-2 dealer-hand)
    (setq shoe ())
    (push card-1 shoe)
    (push card-2 shoe)
    (setq test-string "Threw exception on player-hand with less than 2 cards.")
    (handler-case 
        (progn 
            (player-play player-hand dealer-hand shoe)
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))
    (setq card-1 (create-card 'spades 'ace))
    (setq card-2 (create-card 'spades 'ace))
    (setq player-hand ())
    (push card-1 player-hand)
    (push card-2 player-hand)
    (setq dealer-hand ())
    (push card-1 dealer-hand)
    (setq shoe ())
    (push card-1 shoe)
    (push card-2 shoe)
    (setq test-string "Threw exception on dealer-hand with less than 2 cards.")
    (handler-case 
        (progn 
            (player-play player-hand dealer-hand shoe)
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))
    (setq card-1 (create-card 'spades 'ace))
    (setq card-2 (create-card 'spades 'ace))
    (setq player-hand ())
    (push card-1 player-hand)
    (push card-2 player-hand)
    (setq dealer-hand ())
    (push card-1 dealer-hand)
    (push card-2 dealer-hand)
    (setq shoe ())
    (setq test-string "Threw exception on empty shoe.")
    (handler-case 
        (progn 
            (player-play player-hand dealer-hand shoe)
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))


    (setq function-name "dealer-play")
    (format t "  Testing ~a~%" function-name)
    (setq card-1 (create-card 'spades 'ace))
    (setq card-2 (create-card 'spades 'ace))
    (setq dealer-hand ())
    (push card-1 dealer-hand)
    (setq shoe ())
    (push card-1 shoe)
    (push card-2 shoe)
    (setq test-string "Threw exception on dealer-hand with less than 2 cards.")
    (handler-case 
        (progn 
            (dealer-play dealer-hand shoe)
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))
        (setq card-1 (create-card 'spades 'ace))
    (setq dealer-hand ())
    (push card-1 dealer-hand)
    (push card-2 dealer-hand)
    (setq shoe ())
    (setq test-string "Threw exception on empty shoe.")
    (handler-case 
        (progn 
            (dealer-play dealer-hand shoe)
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))



    (setq function-name "get-best-hand-value")
    (format t "  Testing ~a~%" function-name)
    (setq test-string "Threw exception on empty hand.")
    (handler-case 
        (progn 
            (get-best-hand-value ())
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))
    (setq hand nil)
    (push (create-card 'spades '10) hand)
    (push (create-card 'spades '7) hand)
    (setq test-string "Returns 17.")
    (if (= (get-best-hand-value hand) 17)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq hand nil)
    (push (create-card 'spades '10) hand)
    (push (create-card 'spades 'ace) hand)
    (setq test-string "Returns 21.")
    (if (= (get-best-hand-value hand) 21)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq hand nil)
    (push (create-card 'spades '10) hand)
    (push (create-card 'spades 'ace) hand)
    (push (create-card 'spades 'ace) hand)
    (push (create-card 'spades '10) hand)
    (setq test-string "Returns 22.")
    (if (= (get-best-hand-value hand) 22)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq hand nil)
    (push (create-card 'spades '10) hand)
    (push (create-card 'spades 'ace) hand)
    (push (create-card 'spades 'ace) hand)
    (setq test-string "Returns 12.")
    (if (= (get-best-hand-value hand) 12)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))


    (setq function-name "get-winner")
    (format t "  Testing ~a~%" function-name)
    (setq player-hand ())
    (push (create-card `spades `7) player-hand)
    (push (create-card `spades `10) player-hand)
    (setq dealer-hand ())
    (push (create-card `spades `jack) dealer-hand)
    (push (create-card `spades `10) dealer-hand)
    (setq test-string "Winner is dealer. (no bust)")
    (if (equal (get-winner player-hand dealer-hand) 'dealer)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq player-hand ())
    (push (create-card `spades `7) player-hand)
    (push (create-card `spades `10) player-hand)
    (push (create-card `spades `7) player-hand)
    (setq dealer-hand ())
    (push (create-card `spades `jack) dealer-hand)
    (push (create-card `spades `10) dealer-hand)
    (setq test-string "Winner is dealer. (player bust)")
    (if (equal (get-winner player-hand dealer-hand) 'dealer)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq player-hand ())
    (push (create-card `spades `ace) player-hand)
    (push (create-card `spades `10) player-hand)
    (setq dealer-hand ())
    (push (create-card `spades `jack) dealer-hand)
    (push (create-card `spades `10) dealer-hand)
    (setq test-string "Winner is player. (no bust)")
    (if (equal (get-winner player-hand dealer-hand) 'player)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq player-hand ())
    (push (create-card `spades `4) player-hand)
    (push (create-card `spades `10) player-hand)
    (setq dealer-hand ())
    (push (create-card `spades `jack) dealer-hand)
    (push (create-card `spades `5) dealer-hand)
    (push (create-card `spades `10) dealer-hand)
    (setq test-string "Winner is player. (dealer bust)")
    (if (equal (get-winner player-hand dealer-hand) 'player)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq player-hand ())
    (push (create-card `spades `8) player-hand)
    (push (create-card `spades `10) player-hand)
    (setq dealer-hand ())
    (push (create-card `spades `ace) dealer-hand)
    (push (create-card `spades `5) dealer-hand)
    (push (create-card `spades `2) dealer-hand)
    (setq test-string "It's a push.")
    (if (equal (get-winner player-hand dealer-hand) 'push)
        (progn
            (format t "    ~c[32mPassed: ~a~c[0m~%" #\ESC test-string #\ESC) 
            (incf success))
        (progn
            (format t "    ~c[31mFailed: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail)))
    (setq test-string "Threw exception on empty player-hand.")
    (handler-case 
        (progn 
            (get-winner nil dealer-hand)
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))
    (setq test-string "Threw exception on empty dealer-hand.")
    (handler-case 
        (progn 
            (get-winner player-hand nil)
            (format t "    ~c[31mFailed: Exception test: ~a~c[0m~%" #\ESC test-string #\ESC)
            (incf fail))
        (t (error)
            (format t "    ~c[32mPassed: Exception test: ~a~c[0m~%" #\ESC error #\ESC) 
            (incf success)))



    (setq total (+ success fail))
    (format t "~%")
    (format t "  ~c[32m~a of ~a tests passed.  ~a% pass rate.~c[0m~%" #\ESC success total (round (* (/ success total) 100.0)) #\ESC)
    (if (> fail 0)
        (format t "  ~c[31m~a of ~a tests failed.  ~a% failure rate.~c[0m~%" #\ESC fail total (round (* (/ fail total) 100.0)) #\ESC))
    (format t "~%~%")
    (if (> fail 0) 
        (return-from run-tests nil) 
        (return-from run-tests T)))


(format t "~%")
(format t " _________  ________   ______   _________   ______  ~%")
(format t "|  _   _  ||_   __  |.' ____ \\ |  _   _  |.' ____ \\ ~%")
(format t "|_/ | | \\_|  | |_ \\_|| (___ \\_||_/ | | \\_|| (___ \\_|~%")
(format t "    | |      |  _| _  _.____`.     | |     _.____`. ~%")
(format t "   _| |_    _| |__/ || \\____) |   _| |_   | \\____) |~%")
(format t "  |_____|  |________| \\______.'  |_____|   \\______.'~%")


(if (run-tests) (quit 0) (quit 1))

