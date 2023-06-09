;;; triples-upgrade --- Functions to upgrade data from previous triple db version  -*- lexical-binding: t; -*-

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
;; Occasionally, changes in the triples library are not backwards-compatible,
;; and require upgrading the database. This file contains functions to do those
;; ugprades, along with instructions an how and when to use them.

;;; Code:

(require 'triples)
(require 'rx)
(require 'sqlite)

(defun triples-upgrade-to-0.3 (db)
  "Upgrade the DB to version 0.3.
This will convert all stringified integers stored with sqlite to
actual integers. On emacs version before 29, it will not do
anything, since only the built-in sqlite data needs upgrading.
Callers should force a backup to happen before calling this,
with `(triples-backup db file most-positive-fixnum)'.

This function only handles the case where users transition from
emacsql to sqlite, it is assumed that users don't transition from
sqlite to emacsql after first creating their database.

After triples version 0.3, everything should be created
correctly, so databases created at that version or later should
be correct by default."
  (if (or (version< emacs-version "29")
          (not (eq (type-of db) 'sqlite)))
      (message "Upgrade is only needed for the built-in sqlite databases used by emacs 29+")
    (message "triples: Upgrading triples schema to 0.3")
    (triples-with-transaction
      db
      (sqlite-execute db "ALTER TABLE triples RENAME TO triples_old")
      (triples-setup-table-for-builtin db)
      (sqlite-execute db "INSERT INTO triples (subject, predicate, object, properties) SELECT subject, predicate, object, properties FROM triples_old")
      (sqlite-execute db "DROP TABLE triples_old"))
    (let ((replace-approved))
        (mapc (lambda (column)
                ;; This would all be easier if sqlite supported REGEXP, but
                ;; instead we have to programmatically examine each string to see if it
                ;; is an integer.
                (mapc (lambda (row)
                        (let ((string-val (car row)))
                          (when (string-match (rx (seq string-start (opt ?\") (group-n 1 (1+ digit))) (opt ?\") string-end)
                                              string-val)
                            (message "triples: Upgrading %s with integer string value %s to a real integer" column string-val)
                            ;; Subject transformations have to be treated
                            ;; carefully, since they could end up duplicating
                            ;; predicates.
                            (let ((int-val (string-to-number (match-string 1 string-val))))
                              (when (equal column "subject")
                                (when (and (> (caar (sqlite-execute db "SELECT count(*) FROM triples WHERE subject = ? AND typeof(subject) = 'integer'"
                                                                              (list int-val))) 0)
                                               (or replace-approved
                                                   (y-or-n-p (format "triples: For subject %d, existing real integer subject found.  Replace for this and others? "
                                                                     int-val))))
                                      (setq replace-approved t)
                                        (sqlite-execute db "DELETE FROM triples WHERE subject = ? AND typeof(subject) = 'integer"
                                                        (list int-val))))
                              (sqlite-execute db (format "UPDATE OR REPLACE triples SET %s = cast(REPLACE(%s, '\"', '') as integer) WHERE %s = ?"
                                                         column column column)
                                              (list string-val))))))
                      (sqlite-select
                       db
                       (format "SELECT %s from triples WHERE cast(REPLACE(%s, '\"', '') as integer) > 0 AND typeof(%s) = 'text' GROUP BY %s"
                               column column column column))))
              '("subject" "object"))
        (message "Upgraded all stringified integers in triple database to actual integers"))))

(provide 'triples-upgrade)
;; triples-upgrade ends here
