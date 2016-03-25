(in-package :cl-user)
(defpackage blue-water.web
  (:use :cl
        :caveman2
        :blue-water.config
        :blue-water.view
        :blue-water.db
        :datafly
        :sxql
        :simple-date-time)
  (:export :*web*))
(in-package :blue-water.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defun parse-date (date)
  (from-universal-time
   (handler-case (date-time-parser:parse-date-time date)
     (error () (get-universal-time)))))


(defun param (name query)
  (cdr (assoc name query :test #'equalp)))

(defun index (date &optional search)
  (render #P"index.html"
            :search search
            :date (yyyy/mm/dd date)
            :movies (stable-sort (prepare-movie-list (find-movie-ratings (yyyy-mm-dd date)
                                                                         :search-title search))
                                 (lambda (a b)
                                   (if (or (zerop a) (zerop b)) ; move movies without rating into the end
                                       (> a b)
                                       (< a b)))
                                 :key (lambda (movie)
                                        (getf movie :rating)))))

(defroute "/" ()
  (index (today)))

(defroute "/:year/:month/:day" (&key year month day _parsed)
  (let ((date (parse-date (format nil "~a-~a-~a" year month day)))
         (search (param "search" _parsed)))
    (index date search)))

;; TODO: move to upstream
(defun clone-date (date)
  (deserialize (serialize date)))

(defroute "/history/:year/:month/:day" (&key year month day _parsed)
  (let* ((date (parse-date (format nil "~a-~a-~a" year month day)))
         (movie-id (param "movie" _parsed))
         (movie (find-movie-by-id movie-id)))
    (unless movie
      (throw-code 404))
    (render-json (list
                  :title (movie-title movie)
                  :history (mapcar #'alexandria:plist-hash-table
                                   (find-rating-history movie-id
                                                        (yyyy-mm-dd (day+ (clone-date date) -7))
                                                        (yyyy-mm-dd date)))))))


(defun movie-title (movie)
  (or (getf movie :title-en) (getf movie :title-ru)))

(defun prepare-movie-list (movies)
  (loop for movie in movies
     collect `(:title ,(movie-title movie)
                      ,@(prepare-ratings movie))))

(defun prepare-ratings (movie)
  (loop for name in '(:movie-id :rating :rating-kp :rating-mojo)
     append (let ((rating (or (getf movie name) 0)))
              (list name (floor rating)))))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
