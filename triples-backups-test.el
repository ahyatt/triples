;;; triples-backups-test.el --- Tests for the triples-backup module.  -*- lexical-binding: t; -*-

;; Copyright (c) 2022  Free Software Foundation, Inc.

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

;; Note: It's important to test this on emacs 29, with emacsql installed, so we
;; can make both types of sqlite backend work.

;;; Code:
(require 'ert)
(require 'triples-backups)

(ert-deftest triples-backups-strategy-daily ()
  (cl-letf (((symbol-function 'current-time)
             (lambda ()
               (encode-time (iso8601-parse "2023-01-15T12:00Z")))))
    (should (triples-backups-strategy-daily (encode-time (iso8601-parse "2023-01-14T12:00Z"))))
    (should (triples-backups-strategy-daily (encode-time (iso8601-parse "2022-01-01T12:00Z"))))
    (should-not (triples-backups-strategy-daily (encode-time (iso8601-parse "2023-01-15T12:00Z"))))
    (should-not (triples-backups-strategy-daily (encode-time (iso8601-parse "2023-02-01T12:00Z"))))))

(ert-deftest triples-backups-strategy-weekly ()
  (cl-letf (((symbol-function 'current-time)
             (lambda ()
               (encode-time (iso8601-parse "2023-01-15T12:00Z")))))
    (should (triples-backups-strategy-daily (encode-time (iso8601-parse "2023-01-01T12:00Z"))))
    (should (triples-backups-strategy-daily (encode-time (iso8601-parse "2022-01-01T12:00Z"))))
    (should-not (triples-backups-strategy-daily (encode-time (iso8601-parse "2023-01-15T12:00Z"))))
    (should-not (triples-backups-strategy-daily (encode-time (iso8601-parse "2023-02-01T12:00Z"))))))

(ert-deftest triples-backups-maybe-backup ()
  (let* ((filename (make-temp-file "triples-test"))
         (db (triples-connect filename))
         (backup-called))
    (cl-letf (((symbol-function 'triples-backup)
               (lambda (_ _ num-to-keep)
                 (should (= num-to-keep 3))
                 (setq backup-called t)))
              ((symbol-function 'triples-backups-strategy-always)
               (lambda (_) t))
              ((symbol-function 'triples-backups-strategy-never)
               (lambda (_) nil)))
      (should-error (triples-backups-maybe-backup db filename))
      (triples-backups-setup db 3 'never)
      (triples-backups-maybe-backup db filename)
      (should-not backup-called)
      (triples-backups-setup db 3 'always)
      (triples-backups-maybe-backup db filename)
      (should backup-called))))

(provide 'triples-backups-test)
