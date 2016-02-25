#!/usr/bin/ol --run

(define-library (kal main)

   (import
      (owl base)
      (owl parse)
      (owl date)
      (owl args))

   (export 
      main)

   (begin

      (define version "0.1")

      ;; date = (tuple 'date d m y wday week)

      (define (step date)
         (lets 
            ((_ d m y wd w date)
             (d m y (next-date d m y)))
            (if (eq? wd 7)
               (tuple 'date d m y 1 (+ w 1))
               (tuple 'date d m y (+ wd 1) w))))

      (define (day-of x) (ref x 2))
      (define (month-of x) (ref x 3))
      (define (year-of x) (ref x 4))
      (define (week-day-of x) (ref x 5))
      (define (week-of x) (ref x 6))

      (define (date? x) 
         (and (tuple? x) (eq? (ref x 1) 'date)))

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

      ;;;

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
            ((skip (get-greedy* (get-imm #\space))))
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
            (cons date line)))

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
             (skip (get-imm #\newline))
             (es (get-greedy* (get-event d))))
            es))

      (define kal-grammar
         (let-parses
            ((days
               (get-greedy*
                  (get-any-of
                     get-recurring
                     get-day)))
             (tail maybe-whitespace))
            (foldr append null days)))

      (define (kal-parse path)
         (let ((data (file->list path)))
            (if data
               (try-parse kal-grammar data path "bad kal: " #false)
               (begin
                  (print-to stderr "Could not read " path)
                  #false))))

      ;;;

      (define command-line-rules
         (cl-rules
            `((date "-t" "--time" cook ,string->integer
                  comment "use given instead of current unix time")
              (help "-h" "--help"))))

      (define usage-text 
         "Usage: kal ...")

      (define (print-usage)
         (print usage-text)
         (print (format-rules command-line-rules)))


      ;; any way to order first by year, then month. then day
      (define (date->scalar d)
         (if (and d (eq? (ref d 1) 'date))
            (lets ((_ d m y w1 d2 d))
               (+ d (* m 100) (* y 100000)))
            9999999999999999))

      (define (date< a b)  
         (< (date->scalar a) (date->scalar b)))

      (define (date<= a b)  
         (<= (date->scalar a) (date->scalar b)))


      ;; ((date . evt) ...)
      (define (sort-events evs)
         (sort 
            (lambda (a b) (date< (car a) (car b)))
            evs))

      (define (print-date date)
         (lets ((_ d m y wd w date))
            (print d "." m "." y ", " (ref day-names-en wd) ", week " w)))

      (define (repetition-str rep)
         (tuple-case rep
            ((yearly d m)
               (str "year on " d "." m "."))
            ((daily d)
               (if (number? d)
                  (ref day-names-en d)
                  (error "cannot convert to daily repeptition yet: " d)))
            ((always)
               "day")
            ((week-parity which)
               (str "in " which " weeks"))
            ((and a b)
               (str (repetition-str a) " " (repetition-str b)))
            (else
               (error "odd repetition " rep))))

      (define (print-recurring rec)
         (lets ((_ when evt rec))
            (print "every " (repetition-str when) ": " evt)))

      (define (date-of x)
         (lets 
            ((d m y H M S (date x))
             (cweek cwday (week-info d m y)))
            (tuple 'date d m y cwday cweek)))

      (define (match-date? date rec)
         (tuple-case rec
            ((yearly d m)
               (and (= m (month-of date)) (= d (day-of date))))
            ((daily d)
               (= d (week-day-of date)))
            ((always)
               #true)
            ((week-parity which)
               (if (eq? which 'even)
                  (even? (week-of date))
                  (odd? (week-of date))))
            ((and a b)
               (and (match-date? date a)
                    (match-date? date b)))
            (else
               (error "match-date: what recurrence is " rec))))

      (define (cons-happening-on date)
         (lambda (evs recurring)
            ;(print "Checking if " recurring " happens on " date)
            (if (match-date? date (ref recurring 2))
               (cons (cons date (ref recurring 3)) evs)
               evs)))

      (define (add-recurrences-on date recs tail)
         (fold (cons-happening-on date) tail recs))

      (define (recurring-events date last recs)
         (if (date< last date)
            null
            (add-recurrences-on date recs
               (recurring-events (step date) last recs))))

      (define (kal-output x dict)
         (lets
            ((start-time (get dict 'date (time)))
             (end-time (+ start-time (* 2 week)))
             (now (date-of start-time))
             (end (date-of end-time))
             (evs 
               (keep 
                  (lambda (x) 
                     (and 
                        (date<= now (car x))
                        (date<= (car x) end)))
                  (keep pair? x)))
             (recs
               (remove pair? x))
             (evs 
               (sort-events 
                  (append 
                     (recurring-events now end recs)
                     evs))))

            (fold
               (lambda (date evt)
                  (if (not (equal? date (car evt)))
                     (begin
                        (if date (print ""))
                        (print-date (car evt))))
                  (display " - ")
                  (print (cdr evt))
                  (car evt))
               #false evs)

         (let ((recurs (remove pair? x)))
            (if (pair? recurs)
               (begin
                  (print "")
                  (for-each print-recurring recurs))))))

      (define (kal-read-files paths)
         (fold
            (lambda (cals file)
               (let ((this (kal-parse file)))
                  ;(print "this is " this)
                  (if (and this cals)
                     (append cals this)
                     #false)))
            null paths))

      (define (kal dict args)
         (let ((args (if (null? args) '("-") args)))
            (cond
               ((getf dict 'help)
                  (print-usage)
                  0)
               ((null? args)
                  (print "dict: " dict)
                  (print "args: " args)
                  0)
               ((kal-read-files args) =>
                  (lambda (evs)
                     (kal-output evs dict)
                     0))
               (else
                  1))))

      (define (main args)
         (process-arguments (cdr args) command-line-rules usage-text kal))))

