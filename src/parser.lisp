(in-package :cl-user)
(defpackage blue-water.parser
  (:use :cl :simple-date-time)
  (:import-from :blue-water.config
                :config)
  (:import-from :blue-water.db
                :insert-movie
                :insert-ratings
                :find-movie)
  (:export :parse))
(in-package :blue-water.parser)

(defun remove-year (title)
  (string-trim '(#\Space)
               (cl-ppcre:regex-replace "\\(\\d+\\)" title "")))

(defun parse-date (date)
  (from-universal-time (date-time-parser:parse-date-time date)))

(defun parse (date-from date-to &key verbose)
  (let ((date-from (parse-date date-from))
        (date-to (parse-date date-to)))
    (loop
       for date = date-from then (day+ date 1)
       while (date<= date date-to)
       do
         (when verbose
           (format t "~&Processing date ~a~%" (yyyy/mm/dd date)))
         (insert-ratings (prepare-records (yyyy-mm-dd date))))))

(defun load-movies-list (&optional (path (config :movie-list-path)))
  (with-open-file (stream path)
    (loop for line = (read-line stream nil)
       while line
       collect (remove-year line))))

(defparameter *movie-list* (load-movies-list))

(defun insert-movies (list)
  (loop for title in list
       do (insert-movie title (detect-lang title))))

(defun get-page (url)
  "Get DOM representation of page"
  (plump:parse (drakma:http-request url)))

(defun get-kp-list (date)
  (let ((url (format nil "http://www.kinopoisk.ru/popular/day/~a/" date)))
    (flet ((fix-en-title (title)
             (let ((suffix-index (search ", The" title)))
               (if suffix-index
                   (concatenate 'string "The " (subseq title 0 suffix-index))
                   title)))
           (parse-node (elem selector fix-fn)
             (let ((node (clss:select selector elem)))
                  (if (plusp (length node))
                      (funcall fix-fn (plump:text (aref node 0)))))))
      (loop
         for elem across (clss:select ".el" (get-page url))
         for rating = 1 then (1+ rating)
         collect
           (list
            :rating rating
            :ru (parse-node elem ">a" #'remove-year)
            :en (parse-node elem "i" #'fix-en-title))))))

(defun get-mojo-list (date)
  (let ((url (format nil "http://www.boxofficemojo.com/daily/chart/?view=1day&sortdate=~a&order=DESC" date)))
    (loop
       for node across (clss:select "#body table table a > b" (get-page url))
       for rating = 1 then (1+ rating)
       collect (list
                :rating rating
                :en (remove-year (plump:text node))))))

(defun compare-titles (title1 title2)
  (let ((score (get-intersection-score title1 title2)))
    (>= score 5/7)))

(defun get-intersection-score (title1 title2)
  (let ((words1 (split-title title1))
        (words2 (split-title title2)))
    (/ (length (intersection words1 words2 :test #'equalp))
       (max (length words1) (length words2)))))

(defun split-title (title)
  (cl-ppcre:all-matches-as-strings "(\\w+)" title))

(defun prepare-records (date &key (movies *movie-list*))
  (let ((kp-list (get-kp-list date))
        (mojo-list (get-mojo-list date)))
    (loop for title in movies
       collect
         (let ((kp-rating (find-rating title kp-list))
               (mojo-rating (find-rating title mojo-list)))
           (list :date date
                 :movie-id (getf (find-movie title :lang (detect-lang title)) :movie-id)
                 :rating (or (calculate-rating kp-rating mojo-rating) 0)
                 :rating-kp (or kp-rating 0)
                 :rating-mojo (or mojo-rating 0))))))

(defun calculate-rating (&rest ratings)
  (let* ((ratings (remove nil ratings))
         (num (length ratings)))
    (when (plusp num)
      (/ (apply #'+ ratings) num))))


(defun find-by-title (title list &key (lang :en))
  (find title list :key (lambda (x) (getf x lang)) :test #'compare-titles))

(defun find-rating (title list)
  (getf (find-by-title title list :lang (detect-lang title)) :rating))


(defun detect-lang (title)
  (if (cl-ppcre:scan "[а-яА-Я]" title)
      :ru
      :en))
