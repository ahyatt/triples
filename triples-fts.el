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
;; This package provides full text search for triples.  It uses sqlite's FTS
;; capabilities.  It indexes all text objects.

;; This only will work with the built-in sqlite support in Emacs 29.1 or later.

;;; Code:

(require 'triples)
(require 'sqlite)
(require 'seq)

(defun triples-fts-decolon-to-str (pred)
  "Convert a predicate symbol PRED to a string, removing the colon."
  (replace-regexp-in-string (rx string-start ?:) "" (symbol-name pred)))

(defun triples-fts-add-predicate-abbrevs (db abbrevs)
  "Add the predicate abbreviations ABBREVS.
ABBREVS is an alist of abbreviations to type/predicates, such as
`((\"tag\" . 'tagged/tag))'.

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

(defun triples-fts-setup (db &optional force)
  "Ensure DB has a FTS table.
As long as the FTS table exists, this will not try to recreate
it.  If FORCE is non-nil, then the FTS and all triggers will be
recreated and repopulated."
  (let ((fts-existed (sqlite-select db "SELECT name FROM sqlite_master WHERE type='table' AND name='triples_fts'")))
    (when force (sqlite-execute db "DROP TABLE triples_fts"))
    (sqlite-execute db "CREATE VIRTUAL TABLE IF NOT EXISTS triples_fts USING fts5 (subject, predicate, object, content=triples, content_rowid=rowid)")
    ;; Triggers that will update triples_fts, but only for text objects.
    ;; New rows:
    (when force (sqlite-execute db "DROP TRIGGER IF EXISTS triples_fts_insert"))
    (sqlite-execute db "CREATE TRIGGER IF NOT EXISTS triples_fts_insert AFTER INSERT ON triples
      WHEN new.object IS NOT NULL and typeof(new.object) = 'text'
      BEGIN
        INSERT INTO triples_fts (rowid, subject, predicate, object) VALUES (new.rowid, new.subject, new.predicate, new.object);
      END")
    ;; Updated rows:
    (when force (sqlite-execute db "DROP TRIGGER IF EXISTS triples_fts_update"))
    (sqlite-execute db "CREATE TRIGGER IF NOT EXISTS triples_fts_update AFTER UPDATE ON triples
        WHEN new.object IS NOT NULL AND typeof(new.object) = 'text'
        BEGIN
          INSERT INTO triples_fts (triples_fts, rowid, subject, predicate, object) VALUES ('delete', old.rowid, old.subject, old.predicate, old.object);
          INSERT INTO triples_fts (rowid, subject, predicate, object) VALUES (new.rowid, new.subject, new.predicate, new.object);
        END")
    ;; Deleted rows:
    (when force (sqlite-execute db "DROP TRIGGER IF EXISTS triples_fts_delete"))
    (sqlite-execute db "CREATE TRIGGER IF NOT EXISTS triples_fts_delete AFTER DELETE ON triples
      WHEN old.object IS NOT NULL AND typeof(old.object) = 'text'
      BEGIN
        INSERT INTO triples_fts (triples_fts, subject, predicate, object) VALUES ('delete', old.subject, old.predicate, old.object);
      END")
    (if (or force (not fts-existed)) (triples-fts-rebuild db))))

(defun triples-fts-rebuild (db)
  "Rebuild the FTS table for DB."
  (sqlite-execute db "INSERT INTO triples_fts (triples_fts) VALUES ('rebuild')"))

(defun triples-fts--split-query (query)
  "Return the QUERY split by whitespace, except for quoted strings."
  ;; First, we remove all quoted strings via regexes.
  (let ((quoted-strings '())
        (quoted-strings-re (rx (seq "\"" (group (zero-or-more (not (any "\"")))) "\"")))
        (query-copy query))
    (while (string-match quoted-strings-re query-copy)
      (push (match-string 1 query-copy) quoted-strings)
      (setq query-copy (replace-match "" t t query-copy)))
    ;; Now we split by whitespace, except for quoted strings.
    (append (split-string query-copy) quoted-strings)))

(defun triples-fts--transform-query (db query)
  "Rewrite abbreviations in QUERY based on `triples-fts-predicate-abbrevs`.

This returns a list of new queries.  Because each triple is a row, we
have each part of the query matching separately, and then we do an
intersection on the results.

Because predicates that we need to match against are

E.g. if `tag' is an abbreviation for `tagged/tag'
then:
   \"tag:foo urgent\" ==> \"predicate:\"tagged/tag\" object:\"foo\" urgent\"."
  ;; Split by whitespace, except for quoted strings.
  (let ((segments (triples-fts--split-query query)))
    (mapcar
     (lambda (w)
       (if (string-match "^\\([^:]+\\):\\(.*\\)$" w)
           (let* ((prefix (match-string 1 w))
                  (rest   (match-string 2 w))
                  (full   (triples-fts-get-predicate-for-abbrev db prefix)))
             (if full
                 ;; Example: "tag" => "tagged/tag", rest => "foo"
                 (format "predicate:\"%s\" object:\"%s\"" full rest)
               w))  ; No known abbreviation; just leave as-is.
         w))
     segments)))

(defun triples-fts-query-subject (db query)
  "Query DB with QUERY, returning only subjects.

QUERY should not have operators such as AND or OR, everything is assumed
to be ANDed together.  Phrases can be in quotes.

If there are existing abbreviations, then we also expand user
abbreviations like `tag:xyz` => `predicate:\"tagged/tag\"
object:\"xyz\"`."
  (seq-uniq
   (mapcar
    #'triples-standardize-result
    (cl-reduce #'seq-intersection
               (mapcar
                (lambda (subquery)
                  (mapcar #'car
                          (sqlite-select
                           db
                           "SELECT subject FROM triples_fts
         WHERE triples_fts MATCH ?
         ORDER BY rank"
                           (list subquery))))
                (triples-fts--transform-query db query))))))

(provide 'triples-fts)

;;; triples-fts.el ends here
