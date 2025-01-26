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

(ert-deftest triples-fts-query-subject-after-setup ()
  (triples-test-with-temp-db
    (triples-fts-setup db)
    (triples-add-schema db 'text '(text :base/type string :base/unique t)
                        '(moretext :base/type string :base/unique t))
    (triples-set-subject db 'a '(text :text "Hello, world!" :moretext "World is bond"))
    (triples-set-subject db 'b '(text :text "Goodbye, world!"))
    (should (equal '(a b)
                   (triples-fts-query-subject db "world")))
    (should (equal '(a)
                   (triples-fts-query-subject db "bond")))))

(ert-deftest triples-fts-query-subject-added-before-setup ()
  (triples-test-with-temp-db
    (triples-add-schema db 'text '(text :base/type string :base/unique t)
                        '(moretext :base/type string :base/unique t))
    (triples-set-subject db 'a '(text :text "Hello, world!" :moretext "World is bond"))
    (triples-set-subject db 'b '(text :text "Goodbye, world!"))
    (triples-fts-setup db)
    (should (equal '(a b)
                   (triples-fts-query-subject db "world")))
    (should (equal '(a)
                   (triples-fts-query-subject db "bond")))))

(ert-deftest triples-fts-query-subject-with-abbrev ()
  (triples-test-with-temp-db
    (let ((abbrevs '(("tag" . "text/tag"))))
      (triples-fts-setup db)
      (triples-add-schema db 'text '(text :base/type string :base/unique t)
                          '(tag :base/type string))
      (triples-set-subject db 'a '(text :text "Hello, world!" :tag ("foo" "bar")))
      (should (equal '(a) (triples-fts-query-subject db "Hello" abbrevs)))
      (should (equal '(a) (triples-fts-query-subject db "tag:foo world" abbrevs)))
      (should (equal '(a) (triples-fts-query-subject db "tag: foo world" abbrevs)))
      (should (equal nil (triples-fts-query-subject db "tag:baz world" abbrevs)))
      (should (equal '(a) (triples-fts-query-subject db "text/tag:foo world" abbrevs))))))

(provide 'triples-fts-test)

;;; triples-fts-test.el ends here
