(in-package :cl-user)
(defpackage blue-water.db
  (:use :cl :sxql :datafly)
  (:import-from :blue-water.config
                :config)
  (:import-from :datafly
                :*connection*
                :connect-cached)
  (:export :connection-settings
           :db
           :query
           :insert-movie
           :insert-ratings
           :find-movie
           :find-movie-by-id
           :find-movie-ratings
           :find-rating-history
           :with-connection))
(in-package :blue-water.db)

(defun connection-settings (&optional (db :maindb))
  (cdr (assoc db (config :databases))))

(defun db (&optional (db :maindb))
  (apply #'connect-cached (connection-settings db)))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))


(defmacro query (function statement)
  `(with-connection (db)
     (,function ,statement)))

(defun create-schema()
  (with-connection (db)
    (execute (create-table (:movie :if-not-exists t)
                 ((movie_id :type 'integer
                            :primary-key t)
                  (title_en :type 'string
                            :unique t)
                  (title_ru :type 'string
                            :unique t))))
    (execute (create-table (:rating :if-not-exists t)
                 ((movie_id :type 'integer)
                  (date :type 'integer)
                  (rating :type 'integer)
                  (rating_kp :type 'integer)
                  (rating_mojo :type 'integer))
               (unique-key '(:movie_id :date))
               (foreign-key '(:movie_id) :references '(:movie :movie_id))))))

(defun insert-movie (title lang)
  (query execute (insert-into :movie
                   (set= (title-field lang) title))))

(defun title-field (lang)
  (alexandria:make-keyword (format nil "~:@(~a_~a~)" "title" lang)))

;; TODO: memoize?
(defun find-movie (title &key (lang :en) like)
  (query retrieve-one (select :*
                        (from :movie)
                        (where
                         (if like
                             `(:like ,(title-field lang) ,(format nil "%~a%" title))
                             `(:= ,(title-field lang) ,title))))))

(defun find-movie-by-id (id)
  (query retrieve-one (select :*
                        (from :movie)
                        (where (:= :movie_id id)))))


(defun insert-ratings (ratings)
  (loop for record in ratings
     do (query execute (insert-into :rating
                         (destructuring-bind (&key movie-id date rating rating-kp rating-mojo) record
                           (set= :movie_id movie-id
                                 :date date
                                 :rating rating
                                 :rating_kp rating-kp
                                 :rating_mojo rating-mojo))))))

(defun find-movie-ratings (date &key (lang :en) search-title)
  (query retrieve-all
         (select (:*)
           (from :movie)
           (left-join :rating :using (:movie_id))
           (where (if search-title
                      `(:and (:= :date ,date)
                             (:like ,(title-field lang) ,(format nil "%~a%" search-title)))
                      `(:= :date ,date))))))

(defun find-rating-history (movie-id date-from date-to)
  (query retrieve-all
         (select (:*)
           (from :rating)
           (where (:and (:= :movie_id movie-id)
                        (:>= :date date-from)
                        (:<= :date date-to))))))
