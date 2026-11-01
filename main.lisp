(defun split-csv-line (line)
  (let ((fields '()) (start 0))
    (loop for pos = (position #\, line :start start)
          do (push (string-trim " " (subseq line start (or pos (length line)))) fields)
          while pos
          do (setf start (1+ pos)))
    (nreverse fields)))

(defun parse-field (field)
  (or (ignore-errors (parse-integer field)) field))

(defun read-database (path)
  (with-open-file (stream path :direction :input)
    (loop for line = (read-line stream nil)
          while line
          collect (destructuring-bind (title genre author rating year read-p)
                      (mapcar #'parse-field (split-csv-line line))
                    (list :title title :genre genre :author author
                          :rating rating :year year :read (string-equal read-p "T"))))))

(defun print-book (book)
  (format t "~a - ~a, ~a, rating ~a/10 (~a)~a~%"
          (getf book :title) (getf book :genre) (getf book :author)
          (getf book :rating) (getf book :year)
          (if (getf book :read) " [read]" "")))

(defun print-books (books)
  (if books
      (dolist (book books) (print-book book))
      (format t "Nothing found.~%")))

(defun find-by-title (books title)
  (find title books :key (lambda (book) (getf book :title)) :test #'string-equal))

(defun select-by-author (books author)
  (remove-if-not (lambda (book) (string-equal (getf book :author) author)) books))

(defun sort-by-rating (books)
  (stable-sort (copy-list books) #'> :key (lambda (book) (getf book :rating))))

(defun read-option ()
  (parse-integer (read-line) :junk-allowed t))

(defun prompt-read (prompt)
  (format t "~a" prompt)
  (read-line))

(defun show-menu ()
  (format t "~%1 - All Books~%2 - Find by Title~%3 - Sort by Highest Rating~%4 - Select by Author~%5 - Exit~%> "))

(defun main (&optional (path "database.csv"))
  (let ((books (read-database path)))
    (loop
      (show-menu)
      (case (read-option)
        (1 (print-books books))
        (2 (let ((book (find-by-title books (prompt-read "Enter book's title: "))))
             (if book (print-book book) (format t "Book is not found.~%"))))
        (3 (print-books (sort-by-rating books)))
        (4 (print-books (select-by-author books (prompt-read "Enter book's author: "))))
        (5 (return))
        (otherwise (format t "Unknown option.~%"))))))

(main)
