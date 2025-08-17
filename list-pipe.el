;;; list-pipe.el --- List-Pipes for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2018 Isaac Lewis

;; Author: Isaac Lewis <isaac.b.lewis@gmail.com>
;; Version: 1.0.0
;; Keywords: comm

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;; TODO

;;{{{
;; Code:

;;; Required Libraries
;; For 'pipe-default-newline-delim'
(require 'pipe)

;;{{{
;;; Customization Variables

(defvar list-pipe-debug nil "Whether or not to print debugging
messages.")

(defvar list-pipe-default-underflow-handler (lambda () (error "Underflow")))

;;}}}

;;{{{
;;; List Pipe Functions & Macros

(defun list-pipe-make-list-pipe ()
  "Make a list-based pipe."
  nil)

;;{{{
;;; Peeking Functions

(defun list-pipe-peek (list-pipe)
  "Return the next character to be read from pipe, but don't
modify the pipe."
  (car list-pipe))

(defun list-pipe-ln (list-pipe)
  "Return the next line to be read from pipe, but don't modify the
  pipe."
  (let ((line (list-pipe-read-ln! list-pipe)))
    (dolist (char (reverse line))
      ;; unread the character
      (list-pipe-read! list-pipe char))))

(defun list-pipe-sexp (list-pipe)
  "Return the next sexp to be read from pipe, but don't modify
  the pipe."
  (let ((sexp (list-pipe-read-sexp! list-pipe)))
    (dolist (char (reverse sexp))
      ;; unread the character
      (list-pipe-read! list-pipe char))))

(defun list-pipe-peek-all (list-pipe)
  "Return a string containing all of `list-pipe's currently
  available input, but don't modify `list-pipe'."
  (concat list-pipe))

;;}}}

;;{{{
;;; Reading Macros

(defmacro list-pipe-read! (list-pipe &optional unread underflow-handler)
  "Read a character from `list-pipe'."
  `(progn
     (unless (listp ,list-pipe)
       (error "list-pipe must be a list-pipe"))
     (let ((_unread2
	    (cond ((and (stringp ,unread)
			(equal (length ,unread) 1))
		   (string-to-char ,unread))
		  ((or (not ,unread)
		       (characterp ,unread))
		   ,unread)
		  (t
		   (error "unread must be a character or a string
	   containing a single character."))))  )
       (if _unread2
	   (push _unread2 ,list-pipe)
	 (or (pop ,list-pipe)
	     (funcall (or ,underflow-handler
			  list-pipe-default-underflow-handler)))))))

(defmacro list-pipe-read-ln! (list-pipe)
  "Read a line from `list-pipe'."
  `(let ((chars '()))
     (while (not
	     (funcall (lambda (chars)
			(string-suffix-p pipe-default-newline-delim
					 (concat chars)))
		      (setf chars (append chars
					  (list (list-pipe-read! ,list-pipe)))))))
     (concat chars)))

(defmacro list-pipe-read-sexp! (list-pipe)
  "Read an sexp from `list-pipe'."
  `(read (lambda (&optional unread)
	   (list-pipe-read! ,list-pipe unread))))


(defmacro list-pipe-read-all! (list-pipe &optional unread)
  "Read all available characters from `list-pipe'."
  `(prog1
       (concat ,list-pipe)
     (setf ,list-pipe nil)))

;;}}}

;;{{{
;;; Writing Macros

(defmacro list-pipe-write! (list-pipe str-or-char)
  "Write a character to `list-pipe'."
  `(progn
     (unless (listp ,list-pipe)
       (error "list-pipe must be a list-pipe"))
     (cond ((characterp ,str-or-char)
	    (setf ,list-pipe (append ,list-pipe ,str-or-char)))
	   ((stringp ,str-or-char)
	    (setf ,list-pipe (append ,list-pipe (string-to-list ,str-or-char))))
	   (t
	    (error "str-or-char must be a string or character")))))

(defmacro list-pipe-write-ln! (list-pipe &optional str-or-char)
  "Write `string' followed by a new line delimiter to `pipe'."
  `(list-pipe-write! ,list-pipe (concat ,(or str-or-char "")
					pipe-default-newline-delim)))

(defmacro list-pipe-write-sexp! (list-pipe sexp)
  "Write `string' followed by a new line delimiter to `pipe'."
  `(progn (list-pipe-write! ,list-pipe (prin1-to-string ,sexp))
	  (list-pipe-write! ,list-pipe " ")))

;;}}}

;;}}}

;;}}}

(provide 'list-pipe)
;;; list-pipe.el ends here
