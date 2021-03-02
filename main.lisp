(load "functions.lisp")
(setf *random-state* (make-random-state t)) ;Seed random number generator

(setq shuffled-deck (shuffle-cards (create-deck)))
;(setq shuffled-deck (shuffle-cards `(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
(format t "Shuffled deck: ~a~%" shuffled-deck)
