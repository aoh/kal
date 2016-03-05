(define-library (kal parse)

   (export 
      kal-parse
      kal-parse-string)

   (import
      (owl base)
      (owl date)
      (owl parse))

   (begin

      (define (check-week-info date)
         (lets 
            ((_ d m y wday week date)
             (cweek cwday (week-info d m y))
             (wday (or wday cwday))
             (week (or week cweek)))
            (cond
               ((not (eq? wday cwday))
                  (print-to stderr "incorrect week day")
                  #false)
               ((not (eq? week cweek))
                  (print-to stderr "incorrect week number")
                  #false)
               (else 
                  (tuple 'date d m y wday week)))))

      (define (whitespace? x)
         (or (eq? x #\space) 
             (eq? x #\newline)
             (eq? x #\tab)
             (eq? x #\,)
             (eq? x #\return)))

      (define maybe-whitespace
         (get-greedy* (get-byte-if whitespace?)))

      (define ws-newline
         (let-parses
            ((skip (get-greedy* (get-byte-if (lambda (x) (and (whitespace? x) (not (eq? x #\newline))))))))
            42))

      (define (ascii-digit? x)
         (and (<= #\0 x) (<= x #\9)))

      (define get-nat
         (let-parses
            ((chars (get-greedy+ (get-byte-if ascii-digit?))))
            (fold (λ (n b) (+ (* n 10) (- b 48))) 0 chars)))

      (define get-date
         (let-parses
            ((d get-nat) (skip (get-imm #\.))
             (m get-nat) (skip (get-imm #\.))
             (y get-nat)
             (verify (valid-date? d m y) "This date is not ok."))
            (tuple 'date d m y #f #f)))

      (define (add-week-info date info)
         (if date
            (lets ((_ d m y wday week date))
               (tuple-case info
                  ((week-day n)
                     (if (and wday (not (eq? wday n)))
                        (begin
                           (print-to stderr "wrong week day")
                           #false)
                        (tuple 'date d m y n week)))
                  ((week n)
                     (if (and week (not (eq? week n)))
                        (begin
                           (print-to stderr "wrong week number")
                           #false)
                        (tuple 'date d m y wday n)))
                  (else #false)))
            #false))

      (define get-week-number
         (let-parses
            ((skip maybe-whitespace)
             (skip (get-word "week" 42))
             (skip maybe-whitespace)
             (n get-nat))
            (tuple 'week n)))

      (define get-day-number
         (get-any-of 
            (get-word "maanantai" 1) 
            (get-word "tiistai" 2)
            (get-word "keskiviikko" 3)
            (get-word "torstai" 4)
            (get-word "perjantai" 5)
            (get-word "lauantai" 6)
            (get-word "sunnuntai" 7)
            (get-word-ci "monday" 1)
            (get-word-ci "tuesday" 2)
            (get-word-ci "wednesday" 3)
            (get-word-ci "thursday" 4)
            (get-word-ci "friday" 5)
            (get-word-ci "saturday" 6)
            (get-word-ci "sunday" 7)
            (get-word "ma" 1)
            (get-word "ti" 2)
            (get-word "ke" 3)
            (get-word "to" 4)
            (get-word "pe" 5)
            (get-word "la" 6)
            (get-word "su" 7)))

      (define get-day-name
         (let-parses
            ((skip maybe-whitespace)
             (day get-day-number))
            (tuple 'week-day day)))

      (define get-week-info
         (get-either
            get-week-number
            get-day-name))
            
      (define get-date-info
         (let-parses
            ((d get-date)
             (is (get-greedy* get-week-info))
             (skip ws-newline))
            (check-week-info
               (fold add-week-info d is))))

      (define get-line
         (let-parses
            ((rs (get-greedy* (get-rune-if (lambda (x) (not (eq? x #\newline))))))
             (skip (get-imm #\newline)))
            (list->string rs)))

      (define (get-event date)
         (let-parses
            ((skip (get-greedy+ (get-imm #\space)))
             (skip (get-imm #\-))
             (skip (get-imm #\space))
             (line get-line))
            (tuple 'event date line)))

      (define get-yearly-rec
         (let-parses
            ((skip maybe-whitespace)
             (skip (get-word "year" 0))
             (skip maybe-whitespace)
             (skip (get-either (get-word "at" 0) (get-word "on" 0)))
             (skip maybe-whitespace)
             (d get-nat) (skip (get-imm #\.))
             (m get-nat) (skip (get-imm #\.)))
            (tuple 'yearly d m)))

      ;; every year [in | of | on | during] d.m.
      ;; D = dayname | day
      ;; I = in | of | during
      ;; every D (and D)* (I month (and month)* |
      ;;                   I (odd | even) weeks)

      (define get-recurrence-start
         (let-parses
            ((skip maybe-whitespace)
             (skip (get-any-of (get-word "in" 0) (get-word "at" 0) (get-word "of" 0) (get-word "on" 0) (get-word "during" 0) (get-word "every" 0))))
            0))

      (define get-recurrence-weeks
         (let-parses
            ((skip get-recurrence-start)
             (skip maybe-whitespace)
             (parity (get-either (get-word "odd" 'odd) (get-word "even" 'even)))
             (skip maybe-whitespace)
             (skip (get-word "weeks" 0))) ;; could also be others, but this is most essential
            (tuple 'week-parity parity)))

      (define get-daily-rec
         (let-parses
            ((skip maybe-whitespace)
             (day 
               (get-either 
                  get-day-number
                  (get-word "day" #false))) ;; every weekday, possibly further criteria follow
             (crits
               (get-greedy*
                  get-recurrence-weeks)))
            (fold
               (λ (crit next) (tuple 'and crit next))
               (if day (tuple 'daily day) (tuple 'always))
               crits)))

      (define get-recurring 
         (let-parses
            ((skip maybe-whitespace)
             (skip (get-word "every" 42))
             (skip maybe-whitespace)
             (rec
                (get-any-of
                   get-yearly-rec
                   get-daily-rec))
             (skip maybe-whitespace)
             (skip (get-imm #\:))
             (skip maybe-whitespace)
             (evt get-line))
            (list (tuple 'recurring rec evt))))

      (define get-day
         (let-parses
            ((skip maybe-whitespace)
             (d get-date-info)
             (es
               (get-either
                  (let-parses
                     ((skip (get-imm #\newline))
                      (es (get-greedy* (get-event d))))
                     es)
                  (let-parses
                     ((skip (get-imm #\: ))
                      (skip maybe-whitespace)
                      (evt get-line))
                     (list (tuple 'event d evt))))))
            es))

      (define get-comment 
         (let-parses 
            ((skip maybe-whitespace)
             (val (get-imm #\#))
             (line get-line))
            (list (tuple 'comment (str "#" line)))))

      (define kal-grammar
         (let-parses
            ((days
               (get-greedy*
                  (get-any-of
                     get-recurring
                     get-day
                     get-comment)))
             (tail maybe-whitespace))
            (foldr append null days)))

      (define (kal-parse-string str)
         (try-parse kal-grammar (string->list str) #false #false #false))

      (define (kal-parse path)
         (let ((data (file->list path)))
            (if data
               (try-parse kal-grammar data path "bad kal: " #false)
               (begin
                  (print-to stderr "Could not read " path)
                  #false))))))


