;;; triples-fts-test.el --- Tests for triples FTS module.  -*- lexical-binding: t; -*-

;; Copyright (c) 2025  Free Software Foundation, Inc.

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

;; This file contains tests for the triples FTS module.

;;; Code:

(require 'ert)
(require 'triples-test-utils)
(require 'triples-fts)

(ert-deftest triples-fts-set-predicate-abbrevs-normal ()
  (triples-test-with-temp-db
    (triples-add-schema db 'text '(text :base/type string :base/unique t))
    (triples-fts-set-predicate-abbrevs db '(("txt" . text/text)))
    (should (equal "text/text" (triples-fts-get-predicate-for-abbrev db "txt")))))
