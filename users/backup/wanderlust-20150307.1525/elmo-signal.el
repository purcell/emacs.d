;;; elmo-signal.el --- "signal-slot" abstraction for routing events

;; Copyright (C) 1998-2003 Daiki Ueno <ueno@unixuser.org>

;; Author: Daiki Ueno <ueno@unixuser.org>
;;	Hiroya Murata <lapis-lazuli@pop06.odn.ne.jp>
;; Keywords: mail, net news

;; This file is part of ELMO (Elisp Library for Message Orchestration).

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:

;;; This module implements Qt like "signal-slot" abstraction for
;;; routing events.

;;; Based on riece-signal.el.

;;; Code:

(eval-when-compile (require 'cl))

(defvar elmo-signal-slot-obarray
  (make-vector 31 0))

(defun elmo-make-slot (source listener function &optional filter handback)
  "Make an instance of slot object.
Arguments are corresponding to callback function, filter function, and
a handback object, respectively.
This function is for internal use only."
  (vector source listener function filter handback))

(defun elmo-slot-source (slot)
  "Return the source of SLOT.
This function is for internal use only."
  (aref slot 0))

(defun elmo-slot-listener (slot)
  "Return the listener of SLOT.
This function is for internal use only."
  (aref slot 1))

(defun elmo-slot-function (slot)
  "Return the callback function of SLOT.
This function is for internal use only."
  (aref slot 2))

(defun elmo-slot-filter (slot)
  "Return the filter function of SLOT.
This function is for internal use only."
  (aref slot 3))

(defun elmo-slot-handback (slot)
  "Return the handback object of SLOT.
This function is for internal use only."
  (aref slot 4))

(put 'elmo-define-signal 'lisp-indent-function 'defun)
(defmacro elmo-define-signal (name args &optional doc)
  `(setplist ',name (list 'elmo-signal-args ',args
			  'elmo-signal-docstring ,doc)))

(defun elmo-signal-name (signal)
  "Return the name of SIGNAL."
  signal)

(defun elmo-signal-args (signal)
  "Return the argument list of SIGNAL."
  (get signal 'elmo-signal-args))

(defun elmo-signal-docstring (signal)
  "Return the docment string of SIGNAL."
  (get signal 'elmo-signal-docstring))

(defun elmo-signal-bindings (source listener args handback arg-list)
  (let ((i 0)
	bindings)
    (when (car arg-list)
      (setq bindings (cons (list (car arg-list) listener) bindings)))
    (when (setq arg-list (cdr arg-list))
      (setq bindings (cons (list (car arg-list) source) bindings)))
    (while (and (setq arg-list (cdr arg-list))
		(not (eq (car arg-list) '&optional)))
      (setq bindings (cons (list (car arg-list) (list 'nth i args)) bindings)
	    i (1+ i)))
    (when (and handback
	       (setq arg-list (cdr arg-list)))
      (setq bindings (cons (list (car arg-list) handback) bindings)))
    bindings))

(defmacro elmo-define-signal-handler (args &rest body)
  "Define a signal handler.
ARGS is a symbol list as (LISTENER SOURCE ARG... &optional HANDBACK)."
  (let ((source   (make-symbol "--source--"))
	(listener (make-symbol "--listener--"))
	(argument (make-symbol "--argument--"))
	(handback (make-symbol "--handback--")))
    `(lambda (,listener ,source ,argument ,handback)
       (let ,(elmo-signal-bindings source listener argument handback args)
	 ,@body))))

(put 'elmo-define-signal-handler 'lisp-indent-function 'defun)
(def-edebug-spec elmo-define-signal-handler
  (&define (arg [&rest arg] [&optional ["&optional" arg &rest arg]])
	   def-body))

(defmacro elmo-define-signal-filter (args &rest body)
  "Define a signal filter.
ARGS is a symbol list as (LISTENER SOURCE ARG...)."
  (let ((source   (make-symbol "--source--"))
	(listener (make-symbol "--listener--"))
	(argument (make-symbol "--argument--")))
    `(lambda (,listener ,source ,argument)
       (let ,(elmo-signal-bindings source listener argument nil args)
	 ,@body))))

(put 'elmo-define-signal-filter 'lisp-indent-function 'defun)
(def-edebug-spec elmo-define-signal-filter
  (&define (arg [&rest arg])
	   def-body))

(defun elmo-connect-signal (source signal-name listener handler
				   &optional filter handback)
  "Add HANDLER as a callback function for signal identified by SIGNAL-NAME.
If SOURCE has non-nil value, HANDLER will be invoked only if SOURCE is same as
source argument of `elmo-emit-signal'. Comparison is done with `eq'. If SOURCE
is nil, react on signals from any sources.
You can specify further filter function by FILTER."
  (let ((symbol (intern (symbol-name signal-name) elmo-signal-slot-obarray)))
    (set symbol (cons (elmo-make-slot source listener handler filter handback)
		      (if (boundp symbol)
			  (symbol-value symbol))))))

(defun elmo-disconnect-signal (signal-name listener &optional function)
  "Remove FUNCTION from the listener of the signal identified by SIGNAL-NAME."
  (let* ((symbol (intern-soft (symbol-name signal-name)
			     elmo-signal-slot-obarray))
	 (slots (symbol-value symbol)))
    (while slots
      (when (and (eq (elmo-slot-listener (car slots)) listener)
		 (or (null function)
		     (eq (elmo-slot-function (car slots)) function)))
	(set symbol (delq (car slots) (symbol-value symbol))))
      (setq slots (cdr slots)))))

(defun elmo-clear-signal-slots ()
  "Remove all functions from listeners list."
  (fillarray elmo-signal-slot-obarray 0))

(defun elmo-emit-signal (signal-name source &rest args)
  "Emit signal with SIGNAL-NAME."
  (let ((symbol (intern-soft (symbol-name signal-name)
			     elmo-signal-slot-obarray))
	signal)
    (when symbol
      (dolist (slot (symbol-value symbol))
	(ignore-errors
	  (when (and (or (null (elmo-slot-source slot))
			 (eq (elmo-slot-source slot) source))
		     (or (null (elmo-slot-filter slot))
			 (ignore-errors
			  (funcall (elmo-slot-filter slot)
				   (elmo-slot-listener slot)
				   source
				   args))))
	    (funcall (elmo-slot-function slot)
		     (elmo-slot-listener slot)
		     source
		     args
		     (elmo-slot-handback slot))))))))

(require 'product)
(product-provide (provide 'elmo-signal) (require 'elmo-version))

;;; elmo-signal.el ends here
