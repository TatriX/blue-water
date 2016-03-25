(in-package :cl-user)
(defpackage blue-water.config
  (:use :cl :date-time-parser)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*
           :appenv
           :developmentp
           :productionp))
(in-package :blue-water.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :blue-water))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

(defconfig :common
    `(:application-root ,(asdf:component-pathname (asdf:find-system :blue-water))
                        ;; TODO: force emacs to indent it correctly
                        :movie-list-path ,(merge-pathnames #P"db/movies.list" *application-root*)
                        :parse-from-date (parse-date-time "2016-01-01")))

(defconfig |development|
  `(:debug T
    :databases
    ((:maindb :sqlite3 :database-name ,(merge-pathnames #P"movies.db"
                                                        *application-root*)))))

(defconfig |production|
  '(:databases
    ((:maindb :mysql :database-name "movies" :username "whoami" :password "1234"))))

(defconfig |test|
  '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))

(defun enable-debug ()
  (setf (osicat:environment-variable "APP_ENV") "development"))

;; TODO: remove
(enable-debug)
