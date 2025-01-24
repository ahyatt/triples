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

(defun triples-fts-setup (db &optional force)
  "Ensure DB has a FTS table.
As long as the FTS table exists, this will not try to recreate
it. If FORCE is non-nil, then the FTS and all triggers will be
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

(defun triples-fts-query-subject (db query &optional predicate)
  "Query DB with QUERY, returning only subjects.

Returns a list of subjects whose objects match the query, sorted by most
relevant to least.  PREDICATE can be passed to filter the results to
just objects with a certain predicate."
  (seq-uniq
   (mapcar
    #'triples-standardize-result
    (mapcar #'car
            (sqlite-select
             db
             (if predicate
                 "SELECT subject FROM triples_fts WHERE predicate = ? AND triples_fts MATCH ? ORDER BY rank"
               "SELECT subject FROM triples_fts WHERE triples_fts MATCH ? ORDER BY rank")
             (if predicate
                 (list (replace-regexp-in-string
                        (rx (seq string-start ?:))
                        "" (symbol-name predicate)) query)
               (list query)))))))

(provide 'triples-fts)

;;; triples-fts.el ends here
