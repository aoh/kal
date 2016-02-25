(define-library (kal main)

   (import
      (owl base)
      (owl parse)
      (owl date)
      (kal parse)
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

