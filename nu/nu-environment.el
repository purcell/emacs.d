;; nu-environment.el --- Nubank environment functions -*- lexical-binding: t; -*-

;;; Commentary:

;; Nubank environment functions

;;; Code:

(require 'cl-lib)
(require 'eieio)

(defcustom nu-environment-current
  "staging"
  "The id of the current Nubank environment."
  :group 'nubank
  :type 'string)

(defclass nu-environment ()
  ((id
    :accessor nu-environment-id
    :custom string
    :documentation "The id of the environment."
    :initarg :id
    :type string)
   (name
    :accessor nu-environment-name
    :custom string
    :documentation "The name of the environment."
    :initarg :name
    :type string))
  "A class representing a Nubank environment.")

(defconst nu-environments
  (list (make-instance 'nu-environment :id "staging" :name "Staging")
        (make-instance 'nu-environment :id "prod" :name "Production")
        (make-instance 'nu-environment :id "test" :name "Test"))
  "The list Nubank environments.")

(defun nu-environment-by-id (id)
  "Return the environment from `NU-ENVIRONMENTS`' by `ID`."
  (cl-find id nu-environments :test (lambda (id environment) (equal (nu-environment-id environment) id))))

(defun nu-environment-by-name (name)
  "Return the environment from `NU-ENVIRONMENTS`' by `NAME`."
  (cl-find name nu-environments :test (lambda (name environment) (equal (nu-environment-name environment) name))))

(defun nu-environment-completing-read (&optional default)
  "Read the Nubank environment, falling back to `DEFAULT`."
  (let ((names (mapcar 'nu-environment-name nu-environments)))
    (nu-environment-by-name (completing-read "Nubank environment: " names nil t nil nil default))))

(defun nu-environment-set-current (environment)
  "Set `NU-ENVIRONMENT-CURRENT` to the id of `ENVIRONMENT`."
  (setq nu-environment-current (nu-environment-id environment))
  environment)

(defun nu-environment-switch ()
  "Ask the user for a environment and switch `NU-ENVIRONMENT-CURRENT` to it."
  (interactive)
  (nu-environment-set-current (nu-environment-completing-read)))

(provide 'nu-environment)

;;; nu-environment.el ends here
