(define death_eater
  (lambda (i)
    (if (= i 0)
      0
      (begin
        "death"
        "hoooge"
        "morisan"
        "yanyanyn"
        "tyometyme"
        "dedde---nn"
        "morikuson!!"
        "deddedededee"
        "naynayanayana"
        "nayxynayanayaa"
        "yannaynayanayna"
        "nyaafnaynayanana"
        "yanaynnaynayayana"
        (death_eater (- i 1))))))
(death_eater 1000)
