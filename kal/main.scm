(define-library (kal main)

   (import
      (only (owl sys) getenv)
      (owl base)
      (owl parse)
      (owl date)
      (kal parse)
      (owl args))

   (export 
      kal-string
      main)

   (begin

      (define version "0.1a")

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

      (define (event? node)
         (tuple-case node
            ((event when evt) #true)
            (else #false)))

      (define (event-date x) (ref x 2))
      (define (event-info x) (ref x 3))

      ;;;

      (define command-line-rules
         (cl-rules
            `((date "-t" "--time" cook ,string->integer
                  comment "use given instead of current unix time")
              (days "-n" "--days" cook ,string->integer
                  comment "number of days to show"
                  default "3")
              (show-recurs "-r" "--show-recurs"
                  comment "show also rules of recurring events")
              (show-comments "-c" "--show-comments"
                  comment "show comment lines")
              (everything "-e" "--everything"
                  comment "show all days up to -n or last written down event")
              (version "-V" "--version"
                  comment "show version")
              (help "-h" "--help"))))

      ;; these ought to be autocomputed
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

      (define (sort-events evs)
         (sort 
            (lambda (a b) (date< (event-date a) (event-date b)))
            evs))

      (define (format-date date)
         (lets ((_ d m y wd w date))
            (str d "." m "." y ", " (ref day-names-en wd) ", week " w)))

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

      (define (format-recurring rec)
         (lets ((_ when evt rec))
            (str "every " (repetition-str when) ": " evt)))

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
            (if (match-date? date (ref recurring 2))
               (cons (tuple 'event date (ref recurring 3)) evs)
               evs)))

      (define (add-recurrences-on date recs tail)
         (fold (cons-happening-on date) tail recs))

      (define (recurring-events date last recs)
         (if (date<= last date)
            null
            (add-recurrences-on date recs
               (recurring-events (step date) last recs))))

      (define (grab lst pred)
         (let loop ((lst lst) (match null) (nonmatch null))
            (cond
               ((null? lst)
                  (values (reverse match) (reverse nonmatch)))
               ((pred (car lst))
                  (loop (cdr lst) (cons (car lst) match) nonmatch))
               (else
                  (loop (cdr lst) match (cons (car lst) nonmatch))))))

      (define (comment? node)
         (and (tuple? node) (eq? (ref node 1) 'comment)))

      (define (leading-comments lst)
         (cond
            ((null? lst) (values null lst))
            ((comment? (car lst))
               (lets ((rest tail (leading-comments (cdr lst))))
                  (values (cons (car lst) rest) tail)))
            (else
               (values null lst))))

      (define (date-max a b)
         (if (date< a b) b a))

      (define (last-date d all)
         (fold
            (λ (d node)
               (if (event? node)
                  (date-max d (event-date node))
                  d))
            d all))

      (define (merge-same-day-events evs tail)
         (let loop ((evs evs) (last #false) (out tail))
            (if (null? evs)
               (reverse out)
               (let 
                  ((this (event-date (car evs)))
                   (info (str " - " (event-info (car evs)))))
                  (loop (cdr evs) this
                     (if (equal? last this)
                        (cons info out)
                        (ilist info (format-date this) 
                           (if (null? out)
                              out
                              (cons "" out)))))))))

      (define (remove-duplicates lst)
         (if (null? lst)
            null
            (let loop ((a (car lst)) (lst (cdr lst)))
               (cond 
                  ((null? lst)
                     (list a))
                  ((equal? a (car lst))
                     (loop a (cdr lst)))
                  (else
                     (cons a (loop (car lst) (cdr lst))))))))

      (define (kal-output-ll all dict)
         (if (null? all)
            null
            (lets
               ((start-time (get dict 'date (time)))
                (chosen-end-time 
                  (+ start-time (* day (getf dict 'days))))
                (end (date-of chosen-end-time))
                (end
                  (if (getf dict 'everything)
                     (date-max end (last-date end all))
                     end))
                (now (date-of start-time))
                (prelude-comments all (leading-comments all))
                (comments all (grab all comment?))
                (evs 
                  (keep 
                     (lambda (x) 
                        (and (date<= now (event-date x))
                           (if (getf dict 'everything)
                              #true
                              (date< (event-date x) end))))
                     (keep event? all)))
                (recs (remove event? all))
                (evs 
                  (sort-events 
                     (append (recurring-events now end recs) evs)))
                (evs (remove-duplicates evs)))

               (append
                  (merge-same-day-events evs 
                     (reverse
                        (if (getf dict 'show-comments)
                           (map (lambda (comm) (ref comm 2)) prelude-comments)
                           null)))
                  (if (getf dict 'show-recurs)
                     (if (pair? recs)
                        (cons ""
                           (map format-recurring recs))
                        null)
                     null)
                  (if (getf dict 'show-comments)
                     (map (lambda (comm) (ref comm 2)) comments)
                     null)))))

      ;; todo: return also error info
      (define (kal-string str)
         (let ((es (kal-parse-string str)))
            (if es
               (lets
                  ((args (-> empty
                              (put 'everything 1)
                              (put 'days 8)
                              (put 'show-comments 1)
                              (put 'show-recurs 1)))
                   (res (kal-output-ll es args)))
                  (list->string
                     (foldr append (list #\newline)
                        (interleave (list #\newline)
                           (map string->list res)))))
               #false)))

      (define (kal-output all dict)
         (lfold
            (λ (state line)
               (print line))
            null (kal-output-ll all dict)))

      (define (kal-read-files paths)
         (fold
            (lambda (cals file)
               (let ((this (kal-parse file)))
                  (if (and this cals)
                     (append cals this)
                     #false)))
            null paths))

      (define (default-calendar)
         (str (getenv "HOME") "/.kal"))

      (define (kal dict args)
         (let ((args (if (null? args) (list (default-calendar)) args)))
            (cond
               ((getf dict 'help)
                  (print-usage)
                  0)
               ((getf dict 'version)
                  (print version)
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

