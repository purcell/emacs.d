;; nu-country.el --- Nubank country functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Nubank country functions

;;; Code:

(require 'cl-lib)
(require 'eieio)

(defcustom nu-country-current
  (or (getenv "NU_COUNTRY") "br")
  "The id of the current Nubank country."
  :group 'nubank
  :type 'string)

(defclass nu-country ()
  ((id
    :accessor nu-country-id
    :custom string
    :documentation "The id of the country."
    :initarg :id
    :type string)
   (name
    :accessor nu-country-name
    :custom string
    :documentation "The name of the country."
    :initarg :name
    :type string))
  "A class representing a Nubank country.")

(defconst nu-countries
  (list (make-instance 'nu-country :id "br" :name "Brazil")
        (make-instance 'nu-country :id "mx" :name "Mexico"))
  "The list of Nubank countries.")

(defun nu-country-by-id (id)
  "Return the country from `NU-COUNTRIES`' by `ID`."
  (cl-find id nu-countries :test (lambda (id country) (equal (nu-country-id country) id))))

(defun nu-country-by-name (name)
  "Return the country from `NU-COUNTRIES`' by `NAME`."
  (cl-find name nu-countries :test (lambda (name country) (equal (nu-country-name country) name))))

(defun nu-country-completing-read (&optional default)
  "Read the Nubank country, falling back to `DEFAULT`."
  (let ((names (mapcar 'nu-country-name nu-countries)))
    (nu-country-by-name (completing-read "Nubank country: " names nil t nil default))))

(defun nu-country-set-current (country)
  "Set `NU-COUNTRY-CURRENT` to the id of `COUNTRY`."
  (setq nu-country-current (nu-country-id country))
  country)

(defun nu-country-switch ()
  "Ask the user for a country and switch `NU-COUNTRY-CURRENT` to it."
  (interactive)
  (nu-country-set-current (nu-country-completing-read)))

(provide 'nu-country)

;;; nu-country.el ends here
