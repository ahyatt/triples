;;; triples-test-utils.el --- Test utilities for triples.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Thyis file contiains utilities for testing triples.el, all with the
;; `triples-test' prefix.

;;; Code:

(defvar triples-test-db-file nil
  "The database file used in a test. This is defined so we can
easily debug into it.")

(defmacro triples-test-with-temp-db (&rest body)
  (declare (indent 0) (debug t))
  `(let ((db-file (make-temp-file "triples-test")))
     (unwind-protect
         (progn
           (let ((db (triples-connect db-file)))
             (setq triples-test-db-file db-file)
             ,@body
             (triples-close db)))
       (delete-file db-file))))

(defun triples-test-open-db ()
  (interactive)
  (sqlite-mode-open-file triples-test-db-file))

(defmacro triples-deftest (name _ &rest body)
  "Create a test exercising variants of `triples-sqlite-interface'."
  (declare (debug t) (indent 2))
  (let ((builtin-name (intern (format "%s-builtin" name)))
        (emacsql-name (intern (format "%s-emacsql" name))))
    `(progn
       (ert-deftest ,builtin-name ()
         (let ((triples-sqlite-interface 'builtin))
           (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
           ,@body))
       (ert-deftest ,emacsql-name ()
         (let ((triples-sqlite-interface 'emacsql))
           (skip-unless (featurep 'emacsql))
           ,@body)))))


(provide 'triples-test-utils)

;;; triples-test-utils.el ends here
