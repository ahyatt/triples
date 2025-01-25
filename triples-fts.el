;;; triples-fts.el --- Sqlite full text search for triples. -*- lexical-binding: t; -*-

;; Copyright (c) 2023  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/triples
;;
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
;; This package provides full text search for triples. It uses sqlite's FTS
;; capabilities.  It indexes all text objects.

;; This only will work with the built-in sqlite support in Emacs 29.1 or later.

;;; Code:

(require 'triples)
(require 'sqlite)
(require 'seq)

(defun triples-fts-decolon-to-str (pred)
  "Convert a predicate symbol PRED to a string, removing the colon."
  (replace-regexp-in-string (rx string-start ?:) "" (symbol-name pred)))

(defun triples-fts-set-predicate-abbrevs (db abbrevs)
  "Set the predicate abbreviations to ABBREVS
ABBREVS is an alist of abbreviations to type/predicates, such as
((\"tag\" . 'tagged/tag)).

This stores the abbreviation in the database DB."
  (triples-with-transaction
    db
    (triples-add-schema db 'fts '(abbrev :base/type string :base/unique t))
    (triples-add-schema db 'fts-abbrev '(predicate :base/virtual-reversed fts/abbrev))
    (dolist (abbrev abbrevs)
      (let* ((props (triples-properties-for-predicate db (cdr abbrev))))
        (unless props
          (error "Predicate %s does not exist" (cdr abbrev)))
        (unless (eq (plist-get props :base/type) 'string)
          (error "Predicate %s is not a string" (cdr abbrev)))
        (triples-set-type db (symbol-name (cdr abbrev)) 'fts :abbrev (car abbrev))))))

(defun triples-fts-get-predicate-for-abbrev (db abbrev)
  "Get the predicate for ABBREV from the database DB.
Will return the predicate as a string, or nil if not found."
  (car (plist-get (triples-get-type db abbrev 'fts-abbrev) :predicate)))

(defun triples-fts-rebuild (db)
  "Rebuild the FTS table for DB."
  (sqlite-execute db "INSERT INTO triples_fts (triples_fts) VALUES ('rebuild')"))

(defun triples-fts--transform-query (query)
  "Rewrite abbreviations in QUERY based on `triples-fts-predicate-abbrevs`.

E.g. if `triples-fts-predicate-abbrevs` is '((\"tag\" . \"tagged/tag\")),
then:
   \"tag:foo urgent\" ==> \"predicate:\"tagged/tag\" object:\"foo\" urgent\"."
  ;; Very naive approach: split by whitespace, look for "prefix:rest".
  (let ((words (split-string query)))
    (mapconcat
     (lambda (w)
       (if (string-match "^\\([^:]+\\):\\(.*\\)$" w)
           (let* ((prefix (match-string 1 w))
                  (rest   (match-string 2 w))
                  (full   (cdr (assoc prefix triples-fts-predicate-abbrevs))))
             (if full
                 ;; Example: "tag" => "tagged/tag", rest => "foo"
                 (format "predicate:\"%s\" object:\"%s\"" full rest)
               w))  ; No known abbreviation; just leave as-is.
         w))
     words
     " ")))

(defun triples-fts-query-subject (db query &optional predicate)
  "Query DB with QUERY, returning only subjects.

If PREDICATE is provided, we restrict to that single predicate in
the FTS.  Otherwise we do a full table match.

If `triples-fts-predicate-abbrevs` is set, then we also expand
user abbreviations like `tag:xyz` => `predicate:\"tagged/tag\" object:\"xyz\"`."
  (seq-uniq
   (mapcar
    #'triples-standardize-result
    (mapcar
     #'car
     (sqlite-select
      db
      (if predicate
          ;; If we *also* want the user to be able to pass `predicate` explicitly:
          "SELECT subject FROM triples_fts
           WHERE predicate = ? AND triples_fts MATCH ?
           ORDER BY rank"
        ;; Otherwise, do no predicate filter:
        "SELECT subject FROM triples_fts
         WHERE triples_fts MATCH ?
         ORDER BY rank")
      (if predicate
          (list (replace-regexp-in-string
                 (rx string-start ?:)
                 ""
                 (symbol-name predicate))
                (triples-fts--transform-query query))
        (list (triples-fts--transform-query query))))))))

(provide 'triples-fts)

;;; triples-fts.el ends here
