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

(defun triples-fts-setup (db &optional force)
  "Ensure DB has a FTS table.
As long as the FTS table exists, this will not try to recreate
it.  If FORCE is non-nil, then the FTS and all triggers will be
recreated and repopulated."
  (unless (eq triples-sqlite-interface 'builtin)
    (error "triples-fts requires the built-in sqlite support in Emacs 29.1 or later"))
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
        (query-copy (replace-regexp-in-string (rx (seq ?: (zero-or-more space))) ":" query)))
    (while (string-match quoted-strings-re query-copy)
      (push (match-string 1 query-copy) quoted-strings)
      (setq query-copy (replace-match "" t t query-copy)))
    ;; Now we split by whitespace, except for quoted strings.
    (append (split-string query-copy) quoted-strings)))

(defun triples-fts--transform-query (query abbrevs)
  "Rewrite abbreviations in QUERY based on `triples-fts-predicate-abbrevs`.

This returns a list of new queries.  Because each triple is a row, we
have each part of the query matching separately, and then we do an
intersection on the results.

Because predicates that we need to match against are

E.g. if `tag' is an abbreviation for `tagged/tag', from the alist
ABBREVS, then: \"tag:foo urgent\" ==> \"predicate:\"tagged/tag\"
object:\"foo\" urgent\"."
  ;; Split by whitespace, except for quoted strings.
  (let ((segments (triples-fts--split-query query)))
    (mapcar
     (lambda (w)
       (if (string-match "^\\([^:]+\\):\\(.*\\)$" w)
           (let* ((prefix (match-string 1 w))
                  (rest   (match-string 2 w))
                  (full   (assoc-default prefix abbrevs)))
             (if (or full (string-match-p "/" prefix))
                 ;; Example: "tag" => "tagged/tag", rest => "foo"
                 (format "predicate:\"%s\" object:\"%s\"" (or full prefix) rest)
               w))  ; No known abbreviation; just leave as-is.
         w))
     segments)))

(defun triples-fts-query-subject (db query &optional abbrevs)
  "Query DB with QUERY, returning only subjects.

QUERY should not have operators such as AND or OR, everything is assumed
to be ANDed together.  Phrases can be in quotes.

Predicates can appear before colons to restrict a query term.  For
example, `person/name:Billy'.  Anything with a slash in it, or matching
an entry in ABBREV will be used to filter by a predicate, otherwise it
is passed to FTS5 as-is.

ABBREVS is an alist of abbreviations to predicate (both strings).  If
this is populated then we also expand user abbreviations like `tag:xyz`
=> `predicate:\"tagged/tag\" object:\"xyz\"`."
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
                (triples-fts--transform-query query abbrevs))))))

(provide 'triples-fts)

;;; triples-fts.el ends here
