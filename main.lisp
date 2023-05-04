(defun read-database (database.csv)
  (with-open-file (stream database.csv :direction :input)
    (loop for line = (read-line stream nil)
          while line
          collect (split-sequence ", " line))))

(defun prompt-print (book)
  (format t "~{~a~^ ~}~%" book))

(defun print-books (books)
  (dolist (book books)
    (prompt-print book)))

(defun get-book (books list)
  (find list books 
  :key #'(lambda (book) (nth 0 book)) :test #'string=))

(defun select-by-author (books author)
  (remove-if-not (lambda (book) (string= (nth 2 book) author)) books))

(defun sort-by-rating (books)
  (sort books #'> :key (lambda (rating) (parse-integer (nth 3 rating)))))

(defun main ()
  (let ((filename "database.csv")
        (list (read-database "database.csv"))
        (option nil)
        (author nil)
        (title nil))
    (loop
     (format t "~%Enter:~%1 - All Books;~%2 - Find by Title;~%3 - Sort by Highest Rating;~%4 - Select by Author~%5 - Exit.")
     (setf option (read))
     (cond
      ((= option 1)
       (print-books list))
       ((= option 2)
       (format t "Enter Book's Title: ")
       (setf title (read-line))
       (let ((books (get-book list title)))
         (if books
             (prompt-print books)
           (format t "Book is not found.~%" title))))
      ((= option 3)
       (print-books (sort-by-rating list)))
       ((= option 4)
        (format t "Enter Book's Author: ")
        (setf author (read-line))
        (let ((books (select-by-author list author)))
          (if books 
              (print-books books)
            (format t "Author is not found.~%" author))))
      ((= option 5)
       (return-from main))))))
