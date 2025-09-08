;;; codeawareness-process-sockets.el --- Process sockets for Code Awareness  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Isaac Lewis

;; Author: Isaac Lewis <isaac.b.lewis@gmail.com>
;; Maintainer: Mark Vasile <mark@codeawareness.com>
;; Version: 1.0.0
;; Keywords: processes, comm

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

;; Process sockets for Code Awareness
;;
;; See the documentation at https://github.com/IkeLewis/process-sockets
;;

;;; Code:

;;; Requirements

;; For 'defun*'
(require 'cl)
;; For 'codeawareness-pipe-make-pipe'
(require 'codeawareness-pipe)

;;; API Functions

;;; Constructor(s)

(defun* codeawareness-ps-make-process-socket (process &optional (pipe-buf-size codeawareness-pipe-default-buf-size))
  "Creates a socket for communicating with `process'."
  (unless (>= emacs-major-version 27)
    (error "Emacs 27+ is required"))
  (let* ((process process)
	 (sock-mutex (make-mutex))
	 (sock-output-ready nil)
	 (sock-cv-output-ready
	  (make-condition-variable sock-mutex "sock-cv-output-ready"))
	 (input-pipe (codeawareness-pipe-make-pipe
		      pipe-buf-size
		      (lambda ()
			(if (equal (current-thread)
				   (process-thread process))
			    (catch 'done
			      (while t
				(with-mutex sock-mutex
				  (when sock-output-ready
				    (setq sock-output-ready nil)
				    (throw 'done t)))
				(sleep-for 0 1)))
			  (with-mutex sock-mutex
			    (while (not sock-output-ready)
			      (condition-wait sock-cv-output-ready))
			    (setq sock-output-ready nil))))))
	 (output-pipe (codeawareness-pipe-make-pipe pipe-buf-size))
	 (auto-flush nil))
    ;; Write output from the process to the socket's input stream.
    (set-process-filter process (lambda (process string)
				  (with-mutex sock-mutex
				    (codeawareness-pipe-write! input-pipe string)
				    (setq sock-output-ready t)
				    (condition-notify sock-cv-output-ready t))))
    (lambda (fn-or-var &rest args)
      (case fn-or-var
	;; Accessors
	((process)
	 process)
	((input-pipe)
	 input-pipe)
	((output-pipe)
	 output-pipe)
	((auto-flush)
	 auto-flush)
	;; Mutators
	((set-auto-flush!)
	 (setf auto-flush (car args)))
	(t
	 (error "Invalid arguments"))))))

;;; Accessors

(defun codeawareness-ps-input-stream (ps)
  (codeawareness-pipe-input-stream (funcall ps 'input-pipe)))

(defun codeawareness-ps-output-stream (ps)
  (codeawareness-pipe-output-stream (funcall ps 'output-pipe)))

(defun codeawareness-ps-auto-flush (ps)
  (funcall ps 'auto-flush))

;;; Mutators

(defun codeawareness-ps-set-auto-flush! (ps val)
  (funcall ps 'set-auto-flush! val))

;;; Reading Functions

(defun codeawareness-ps-read! (ps)
  (codeawareness-pipe-read! (funcall ps 'input-pipe)))

(defun codeawareness-ps-read-ln! (ps)
  (codeawareness-pipe-read-ln! (funcall ps 'input-pipe)))

(defun codeawareness-ps-read-all! (ps)
  (codeawareness-pipe-read-all! (funcall ps 'input-pipe)))

(defun codeawareness-ps-read-sexp! (ps)
  (codeawareness-pipe-read-sexp! (funcall ps 'input-pipe)))

;;; Writing Functions

(defun codeawareness-ps-write! (ps char-or-str)
  (codeawareness-pipe-write! (funcall ps 'output-pipe) char-or-str)
  (when (codeawareness-ps-auto-flush ps)
    (codeawareness-ps-flush! ps)))

(defun codeawareness-ps-write-ln! (ps &optional char-or-str)
  (codeawareness-pipe-write-ln! (funcall ps 'output-pipe) char-or-str)
  (when (codeawareness-ps-auto-flush ps)
    (codeawareness-ps-flush! ps)))

(defun codeawareness-ps-write-sexp! (ps sexp)
  (codeawareness-pipe-write-sexp! (funcall ps 'output-pipe) sexp)
  (when (codeawareness-ps-auto-flush ps)
    (codeawareness-ps-flush! ps)))

(defun codeawareness-ps-flush! (ps)
  (process-send-string (funcall ps 'process)
		       (codeawareness-pipe-read-all! (funcall ps 'output-pipe))))

;;; Misc Functions

(defun codeawareness-ps-close! (ps)
  (codeawareness-ps-flush! ps)
  (delete-process (funcall ps 'process)))

(provide 'codeawareness-process-sockets)
;;; codeawareness-process-sockets.el ends here
