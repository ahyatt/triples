;;; triples-backups --- Functions to add backup functionality to triple databases.  -*- lexical-binding: t; -*-

;; Copyright (c) 2022  Free Software Foundation, Inc.

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
;; This provides backup functionality. The information about how and when to do
;; backups lives in the database itself, on a special entity `database'.

(require 'triples)

(defun triples-backups-setup (db num-to-keep strategy)
  "Set DB's backup strategy.
NUM-TO-KEEP is the number of backup files to keep. Older ones are
removed. STRATEGY is a symbol that corresponds to a function
`triples-backups-strategy-STRATEGY'. This function must always be
loaded before any client of this db calls
`triples-backups-maybe-backup', so adding your own may not always
be appropriate."
  (triples-with-transaction db
                            (triples-add-schema db 'backup '(num-to-keep :base/unique t :base/type integer)
                                                '(strategy :base/unique t :base/type symbol)
                                                '(last-update-time :base/unique t :base/type integer))
                            (triples-set-type db 'database 'backup :num-to-keep num-to-keep
                                              :strategy strategy :last-update-time (time-convert (current-time) 'integer))))

(defun triples-backups-configuration (db)
  "Returns the backup configuration set by `triples-backups-setup'.
If no one has ever run that on this database, `nil' is returned."
  (triples-get-type db 'database 'backup))

(defun triples-backups-last-update-time (db)
  "Get the last time DB has been updated."
  (plist-get (triples-get-type db 'database 'backup) :last-update-time))

(defun triples-backups-maybe-backup (db &optional filename)
  "If it is time for DB to be backed up, then back it up.
FILENAME is optional, as in `triples-connect', if not given will
default to the standard triple database given in
`triples-default-database-filename'."
  (let* ((backup-info (triples-backups-configuration db))
         (strategy-func (intern (format "triples-backups-strategy-%s"
                                        (plist-get backup-info :strategy)))))
    (unless backup-info
      (error "`triples-backups-setup' needs to be called on this database before trying to back up."))
    (unless (fboundp strategy-func)
      (display-warning
       'triples
       (format "Triples backup strategy %s not found, defaulting to `triples-backups-strategy-daily'"
               strategy-func)
       :error))
    (when (funcall (or (symbol-function strategy-func) #'triples-backups-strategy-daily)
                   (plist-get backup-info :last-update-time))
      (triples-backup db filename (plist-get backup-info :num-to-keep))
      (apply #'triples-set-type db 'database 'backup (plist-put backup-info :last-update-time (time-convert (current-time) 'integer))))))

(defun triples-backups-strategy-every-change (_)
  "Backup strategy to do a backup on each change."
  t)

(defun triples-backups-strategy-never (_)
  "Backup strategy to never do a backup."
  nil)

(defun triples-backups-strategy-daily (last-update)
  "Backup strategy to create a change daily at most.
LAST-UPDATE is the time of the last update."
  (>= (/ (- (float-time (current-time)) (float-time last-update)) 86400)
      1))

(defun triples-backups-strategy-weekly (last-update)
  "Backup strategy to create a change daily at most.
LAST-UPDATE is the time of the last update."
  (>= (/ (- (float-time (current-time)) (float-time last-update)) 86400)
      7))

(provide 'triples-backups)
;;; triples-backups.el ends here
