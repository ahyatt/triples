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

(defun triples-upgrade-to-0.3 (db)
  "Upgrade the DB to version 0.3.
This will convert all stringified integers stored with sqlite to
actual integers. On emacs version before 29, it will not do
anything, since only the built-in sqlite data needs upgrading.
Callers should force a backup to happen before calling this,
with `(triples-backup db file most-positive-fixnum)'."
  (if (or (version< emacs-version "29")
          (not (eq (type-of db) 'sqlite)))
      (message "Upgrade is only needed for the built-in sqlite databases used by emacs 29+")
    (triples-with-transaction
      db
      (mapc (lambda (column)
              (sqlite-execute
               db
               (format "UPDATE OR IGNORE triples SET %s = cast(REPLACE(%s, '\"', '') as integer) WHERE cast(REPLACE(%s, '\"', '') as integer) > 0"
                       column column column)))
            '("subject" "object"))
      (message "Upgraded all stringified integers in triple database to actual integers"))))

(provide 'triples-upgrade)
;; triples-upgrade ends here
