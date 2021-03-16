(setf *random-state* (make-random-state t)) ;Seed random number generator

(defun create-card(suit val)
    "Creates a card, i.e list with suit and value"
    (if (not (find suit '(spades diamonds hearts clubs))) 
        (error "Cards must have suit of spades diamonds hearts clubs."))
    (if (not (find val `(ace king queen jack 10 9 8 7 6 5 4 3 2))) 
        (error "Cards must have val of ace king queen jack 10 9 8 7 6 5 4 3 2."))
    (list :suit suit :val val))


(defun create-deck()
    "Creates a deck of cards"
    (setq deck nil)
    (dolist (suit `(spades diamonds hearts clubs))
        (dolist (val `(ace king queen jack 10 9 8 7 6 5 4 3 2))
            (push (create-card suit val) deck)))
    (return-from create-deck deck))


(defun create-shoe(num-decks)
    "Creates an n deck shoe of card decks"
    (if (< num-decks 1) 
        (error "Can not create a shoe with less than 1 deck."))
    (if (> num-decks 8)
        (error "Can not create a shoe with more than 8 decks."))
    (setq shoe nil)
    (loop repeat num-decks do 
        (setq deck (create-deck))
        (dolist (card deck) (push card shoe)))
    (return-from create-shoe shoe))

(defun remove-card-from-deck(deck index)
    "Removes the list item at index and returns
    the resulting list"
    (setq cards-in-deck (list-length deck))
    (if (< cards-in-deck 1) 
        (error "Can't remove a card from deck with less than 1 card."))
    (if (< index 0) 
        (error "Index can't be less than zero."))
    (if (> index (- cards-in-deck 1)) 
        (error (format nil "Index can't be greater than ~a." (- cards-in-deck 1))))
    (setq new-deck nil)
    (loop for x in deck
        for i from 0
        if (/= i index) do (push x new-deck))
    (return-from remove-card-from-deck (reverse new-deck)))

(defun shuffle-cards(cards)
    "Shuffles a list of cards"
    (setq cards-in-deck (list-length cards))
    (if (< cards-in-deck 1) 
        (error "Can't shuffle an empty deck."))
    (setq shuffled-cards nil)
    (loop
        until (< (list-length cards) 1)
        do  (setq index (random (list-length cards)))
            (push (nth index cards) shuffled-cards)
            (setq cards (remove-card-from-deck cards index)))
    (return-from shuffle-cards shuffled-cards))

(defun deck-has-at-least-n-cards(deck n)
    "Returns true if deck has at least n cards"
    (setq num-cards-in-deck (list-length deck))
    (>= num-cards-in-deck n))

(defun deal-card(cards)
    "Removes the first card from the deck and returns it
     along with the modified deck"
    (if (not (deck-has-at-least-n-cards cards 1)) 
        (error "Can't deal a card from a deck with less than one card."))
    (setq first-card (car cards))
    (setq cards (cdr cards))
    (values first-card cards))


(defun get-num-val-card(card)
    "Returns the number value for a card.  Returns both 1 and 11 for an ace."
    (if (not card) 
        (error "Can't get num value for a nil card."))
    (setq string-val (getf card :val))
    (if (eq string-val `2) (return-from get-num-val-card 2))
    (if (eq string-val `3) (return-from get-num-val-card 3))
    (if (eq string-val `4) (return-from get-num-val-card 4))
    (if (eq string-val `5) (return-from get-num-val-card 5))
    (if (eq string-val `6) (return-from get-num-val-card 6))
    (if (eq string-val `7) (return-from get-num-val-card 7))
    (if (eq string-val `8) (return-from get-num-val-card 8))
    (if (eq string-val `9) (return-from get-num-val-card 9))
    (if (eq string-val `10) (return-from get-num-val-card 10))
    (if (eq string-val `jack) (return-from get-num-val-card 10))
    (if (eq string-val `queen) (return-from get-num-val-card 10))
    (if (eq string-val `king) (return-from get-num-val-card 10))
    (if (eq string-val `ace) (values 1 11)))




(defun get-hand-vals(hand)
    "Returns a list of only values for a hand.  Does not include suits"
    (if (not hand) 
        (error "Can't get list of values for empty hand."))
    (setq vals nil)
    (loop for card in hand do
        (push (getf card :val) vals))
    (return-from get-hand-vals vals))
   


(defun calculate-hand-value(hand)
    "Calculates the value of a blackjack hand.  Returns 2 values.
     If it's not a soft hand (contains ace), second value is nil"
     (if (not hand) 
        (error "Can't calculate hand value for empty hand."))
    (setq total-1 0)
    (setq total-2 0)

    (setq hand-contains-ace nil)
    (loop for card in hand do
        (if (eq (getf card :val) `ace) (setq hand-contains-ace T) )
        (incf total-1 (get-num-val-card card)))
    (if hand-contains-ace 
        (setq total-2 (+ total-1 10))
        (setq total-2 nil))
    (if total-2
        (if (> total-2 21) 
            (setq total-2 nil)
            (if (= total-2 21) 
                (progn 
                    (setq total-2 nil) 
                    (setq total-1 21)))))
    (values total-1 total-2))

(defun deal-player-and-dealer(shoe)
    "Deals the initial hand to player and dealer.
     Returns both hands and the modified deck"
    (setq cards-in-deck (list-length deck))
    (if (< cards-in-deck 4) 
        (error "Can't deal player and dealer with less than 4 cards in deck."))
    (setq player-hand nil)
    (setq dealer-hand nil)
    (multiple-value-setq (card shoe) (deal-card shoe))
    (push card player-hand)
    (multiple-value-setq (card shoe) (deal-card shoe))
    (push card dealer-hand)
    (multiple-value-setq (card shoe) (deal-card shoe))
    (push card player-hand)
    (multiple-value-setq (card shoe) (deal-card shoe))
    (push card dealer-hand)
    (values player-hand dealer-hand shoe))

(defun get-player-bet(player-stack)
    "Prompts for the bet, decrements the player chip stack and returns both"
    (if (not player-stack) 
        (error "Can't bet with an empty chip stack."))
    (setq done nil)
    (loop while (not done) do
        (format t "You have $~a.  How much do you want to bet?:  " player-stack)
        (setq bet (read))
        (if (not (numberp bet)) (format t "That ain't a number. Try again.~%"))
        (if (> bet player-stack) (format t "Can't bet more than you have. Try again.~%"))
        (if (< bet 1) (format t "You have to be at least $1. Try again.~%"))
        (return-from get-player-bet (values bet (- player-stack bet)))
    ))


(defun player-play (player-hand dealer-hand shoe)
    "Displays player hand and prompts to hit stand or double"
    (if (< (list-length player-hand) 2)
        (error "Can't play player-hand has less than 2 cards."))
    (if (< (list-length dealer-hand) 2)
        (error "Can't play dealer-hand has less than 2 cards."))
    (if (not shoe)
        (error "Can't play with an empty shoe."))
    (format t "Dealer shows a ~:(~a~).~%" (getf (car dealer-hand) :val))
    (setq player-action nil)
    (loop while (not (string-equal player-action "s"))
        do 
            (multiple-value-setq (player-hand-val-1 player-hand-val-2) (calculate-hand-value player-hand))
            (if (and (>= player-hand-val-1 21) (not player-hand-val-2)) (setq player-action "s") (progn
                (format t "You have a ~a~@[ or ~a~]. (~{~a~^ ~})~%" player-hand-val-1 player-hand-val-2 (get-hand-vals player-hand))
                (format t "Do you want to Hit[h] Stand[s] Double Down[d]:  ")
                (setq player-action (read))
                (if (not (string-equal player-action "s")) (progn
                    (multiple-value-setq (card shoe) (deal-card shoe))
                    (push card player-hand))))))
    (values player-hand shoe))

(defun dealer-play (dealer-hand shoe)
    "Plays out the dealer hand, hits on soft 17"
    (if (< (list-length dealer-hand) 2)
        (error "Can't play dealer-hand has less than 2 cards."))
    (if (not shoe)
        (error "Can't play with an empty shoe."))
    (setq done nil)
    (multiple-value-setq (dealer-hand-val-1 dealer-hand-val-2) (calculate-hand-value dealer-hand))
    (loop while (not done) do 
        (if dealer-hand-val-2 ;Soft hand
            (if (and (> dealer-hand-val-2 17) (<= dealer-hand-val-2 21)) (setq done T)))
        (if (>= dealer-hand-val-1 17) (setq done T)) ;Hard hand

        (if (not done) (progn
            ;Deal another card
            (multiple-value-setq (card shoe) (deal-card shoe))
            (push card dealer-hand)
            (multiple-value-setq (dealer-hand-val-1 dealer-hand-val-2) (calculate-hand-value dealer-hand))
        )))
    (values dealer-hand shoe))

;(setq shoe (shuffle-cards (create-deck)))
;(setq dealer-hand nil)
;(push (create-card 'spades 'ace) dealer-hand)
;(push (create-card 'spades '6) dealer-hand)
;(multiple-value-setq (dealer-hand shoe) (dealer-play dealer-hand shoe))
;(format t "Dealer ended up with ~a~@[ or ~a~]. (~{~a~^ ~})~%" dealer-hand-val-1 dealer-hand-val-2 (get-hand-vals dealer-hand))
;(format t "Shoe has: ~a~%" (list-length shoe))

;(setq dealer-hand nil)
;(push (create-card 'spades 'ace) dealer-hand)
;(push (create-card 'spades '6) dealer-hand)
;(multiple-value-setq (dealer-hand-val-1 dealer-hand-val-2) (calculate-hand-value dealer-hand))
;(format t "~a  ~a~%" dealer-hand-val-1 dealer-hand-val-2)
