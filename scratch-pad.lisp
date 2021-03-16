(setq og-output *standard-output*)
(setq og-input *standard-input*)
(setq string-output (make-string-output-stream))
(setq *standard-output* string-output)
(setq *standard-input* (make-string-input-stream "15"))

(format t "You have $100.  How much do you want to bet?:  ")
(setq a (get-output-stream-string *standard-output*))
(setq player-bet (read))

(setq *standard-output* og-output)
(setq *standard-input* og-input)
(format t "You bet $~a~%" player-bet)
(format t "~a~%" a)