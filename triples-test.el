;;; triples-test.el --- Tests for triples module.  -*- lexical-binding: t; -*-

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
;;
;; The tests also require the `kv' package, which can be found at
;; https://github.com/jjeffery/kv.

;;; Code:

(require 'triples)
(require 'seq)
(require 'kv nil t)                     ;; May be absent.
(require 'emacsql nil t)                ;; May be absent.

;;; Code:

(defvar triples-test-db-file nil
  "The database file used in a test. This is defined so we can
easily debug into it.")

(defmacro triples-test-with-temp-db (&rest body)
  (declare (indent 0) (debug t))
  `(let ((db-file (make-temp-file "triples-test")))
     (unwind-protect
         (progn
           (let ((db (triples-connect db-file)))
             (setq triples-test-db-file db-file)
             ,@body
             (triples-close db)))
       (delete-file db-file))))

(defun triples-test-connect-db ()
  (interactive)
  (defvar sql-database)
  (let ((sql-database triples-test-db-file))
    (sql-sqlite (format "*schema test db SQL %s*" triples-test-db-file))))

(defmacro triples-deftest (name _ &rest body)
  "Create a test exercising variants of `triples-sqlite-interface'."
  (declare (debug t) (indent 2))
  (let ((builtin-name (intern (format "%s-builtin" name)))
        (emacsql-name (intern (format "%s-emacsql" name))))
    `(progn
       (ert-deftest ,builtin-name ()
         (let ((triples-sqlite-interface 'builtin))
           (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
           ,@body))
       (ert-deftest ,emacsql-name ()
         (let ((triples-sqlite-interface 'emacsql))
           (skip-unless (featurep 'emacsql))
           ,@body)))))

(triples-deftest triples-connect-default ()
  (let* ((triples-default-database-filename (make-temp-file "triples-default"))
         (db (triples-connect)))
    (triples-db-insert db 1 'pred 2)
    (triples-close db)
    (should (file-exists-p triples-default-database-filename))))

(triples-deftest triples-test-insert ()
  (triples-test-with-temp-db
    (triples-db-insert db "sub" 'pred "obj")
    (should (equal (mapcar (lambda (row) (seq-take row 3)) (triples-db-select db))
                   '(("sub" pred "obj"))))
    ;; Test that we actually are storing with builtin something compatible
    ;; with emacsql.
    (when (eq triples-sqlite-interface 'builtin)
      (should (equal (sqlite-select db "SELECT * FROM triples")
                     '(("\"sub\"" "pred" "\"obj\"" "()")))))
    ;; Test that it replaces - this shouldn't result in two rows.
    (triples-db-insert db "sub" 'pred "obj")
    (should (= (length (triples-db-select db)) 1))
    ;; Test that colons in the predicate are stripped away when stored.
    (triples-db-insert db "sub" :test/pred "obj")
    (should (= (length (triples-db-select db nil 'test/pred)) 1))
    ;; Test we correctly test for bad inputs.
    (should-error (triples-db-insert db "sub" "pred" "obj"))
    (should-error (triples-db-insert db "sub" 'pred "obj" '(ordinary-list)))
    (should-error (triples-db-insert db "sub" 'pred "obj" "string"))
    ;; Test that we can have symbol subject and objects.
    (triples-db-insert db 'sub 'pred 'obj)
    (should (equal
             (mapcar (lambda (row) (seq-take row 3)) (triples-db-select db 'sub))
             '((sub pred obj))))
    ;; Test that properties aren't strings. They happen to be stored
    ;; differently for each system due to differences in how the inserting
    ;; interface works.
    (should (plistp (nth 3 (car (triples-db-select db 'sub)))))))

(triples-deftest triples-test-delete ()
  (triples-test-with-temp-db
    (triples-db-insert db 1 'pred 2)
    (triples-db-insert db 2 'pred 1)
    (triples-db-delete db 1)
    (should (= 1 (length (triples-db-select db))))
    (should (= 0 (length (triples-db-select db 1))))
    (triples-db-insert db 1 'pred 2)
    (triples-db-delete db nil nil 2)
    (should (= 0 (length (triples-db-select db nil nil 2))))
    (triples-db-insert db 1 'pred 2)
    (triples-db-delete db nil 'pred nil)
    (should (= 0 (length (triples-db-select db))))))

(triples-deftest triples-test-delete-subject-predicate-prefix ()
  (triples-test-with-temp-db
    (triples-db-insert db 1 'test/foo 2)
    (triples-db-insert db 1 'bar/bar 1)
    (triples-db-delete-subject-predicate-prefix db 1 'test)
    (should (= 1 (length (triples-db-select db))))
    ;; Make sure colons are stripped.
    (triples-db-delete-subject-predicate-prefix db 1 :bar)
    (should (= 0 (length (triples-db-select db))))))

(triples-deftest triples-test-select ()
  (triples-test-with-temp-db
    (triples-db-insert db 1 'pred 2 '(:a 1))
    (let ((expected '((1 pred 2 (:a 1)))))
      (should (equal (triples-db-select db 1) expected))
      (should (equal (triples-db-select db nil 'pred) expected))
      (should (equal (triples-db-select db nil nil 2) expected))
      (should (equal (triples-db-select db 1 nil 2) expected))
      (should (equal (triples-db-select db 1 'pred 2) expected))
      (should (equal '((1)) (triples-db-select db 1 nil nil nil '(subject))))
      (should (equal '((1 pred)) (triples-db-select db 1 nil nil nil '(subject predicate)))))))

(triples-deftest triples-test-select-with-pred-prefix ()
  (triples-test-with-temp-db
    (triples-db-insert db 'sub1 'pred/foo 'obj)
    (triples-db-insert db 'sub1 'pred/bar 'obj)
    (triples-db-insert db 'sub2 'pred/foo 'obj)
    (should (equal (triples-test-list-sort (triples-db-select-pred-prefix db 'sub1 'pred))
                   (triples-test-list-sort `((sub1 pred/foo obj ,(pcase triples-sqlite-interface
                                                                   ('builtin nil)
                                                                   ('emacsql '(:t t))))
                                             (sub1 pred/bar obj ,(pcase triples-sqlite-interface
                                                                   ('builtin nil)
                                                                   ('emacsql '(:t t))))))))))

(triples-deftest triples-test-select-predicate-object-fragment ()
  (triples-test-with-temp-db
    (triples-db-insert db 'sub1 'pred/foo "a whole phrase")
    (should (equal
             (mapcar (lambda (row) (seq-take row 3))
                     (triples-db-select-predicate-object-fragment db 'pred/foo "whole"))
             '((sub1 pred/foo "a whole phrase"))))))

(triples-deftest triples-test-subjects-with-predicate-object ()
  (triples-test-with-temp-db
    (triples-db-insert db 'sub1 'pred/foo "bar")
    (should (equal (triples-subjects-with-predicate-object db 'pred/foo "bar")
                   '(sub1)))))

;; After this we don't bother testing both with emacsql and the builtin sqlite,
;; since if the functions tested above work, it should also work for both.

(defun triples-test-op-equals (result target)
  (and (equal (car result) (car target))
       (seq-set-equal-p (cdr result) (cdr target) #'equal)))

(ert-deftest triples-add-schema-op ()
  (should (triples-test-op-equals
           (triples--add-schema-op
            'named
            '(name :base/unique t)
            '(locale :base/unique t)
            'alternate-names
            '(nicknames :base/unique nil))
           '(replace-subject
             .
             ((named base/type schema)
              (named schema/property name)
              (named/name base/unique t)
              (named schema/property locale)
              (named/locale base/unique t)
              (named schema/property alternate-names)
              (named schema/property nicknames))))))

(defun triples-test-list-sort (list)
  "Standard sort for LIST for test stability."
  (sort list (lambda (a b) (string< (format "%S" a) (format "%S" b)))))

(ert-deftest triples-schema-crud ()
  (triples-test-with-temp-db
    (triples-add-schema db 'named
                        '(name :base/unique t) 'alternate-names)
    (should (equal '(:base/unique t)
           (triples-properties-for-predicate db 'named/name)))
    (should (equal
             (triples-test-list-sort '(name alternate-names))
             (triples-test-list-sort (triples-predicates-for-type db 'named))))))

(ert-deftest triples-properties-for-predicate ()
  (triples-test-with-temp-db
    (triples-add-schema db 'named
                        '(name :base/unique t)
                        'alternate-names)
    (should (equal '(:base/unique t)
                    (triples-properties-for-predicate db 'named/name)))
    (should-not (triples-properties-for-predicate db 'foo/bar))))

(ert-deftest triples-set-type ()
  (should (triples-test-op-equals
           (triples--set-type-op "Bert" 'named
                                 '(:name "Bertholomew The Second"
                                         :alias ("Bert" "Berty")))
           '(replace-subject-type
             .
             (("Bert" base/type named)
              ("Bert" named/name "Bertholomew The Second")
              ("Bert" named/alias "Bert" (:index 0))
              ("Bert" named/alias "Berty" (:index 1)))))))

(ert-deftest triples-schema-compliant ()
  (triples-test-with-temp-db
    (triples-add-schema db 'named
                        '(name :base/unique t :base/type string)
                        'alternate-names)
    (triples-set-type db "foo" 'named :name "name")
    (should (triples-verify-schema-compliant db '(("foo" named/name "bar"))))
    (should-error (triples-verify-schema-compliant db '(("foo" named/name 5))))
    (should-error (triples-verify-schema-compliant db '(("foo" named/name "bar" (:index 0)))))
    (should (triples-verify-schema-compliant db '(("foo" named/alternate-names "bar" (:index 0)))))))

(defun triples-test-plist-sort (plist)
  "Sort PLIST in a standard way, for comparison."
  (kvalist->plist
   (kvalist-sort (kvplist->alist plist)
                 (lambda (a b) (string< (format "%s" a) (format "%s" b))))))

(ert-deftest triples-crud ()
  (triples-test-with-temp-db
   (triples-add-schema db 'named
                       '(name :base/unique t)
                       'alias)
   (triples-add-schema db 'callable
                       '(phone-number :base/unique t))
   (triples-set-type db "foo" 'named :name "Name" :alias '("alias1" "alias2"))
   (triples-set-type db "foo" 'callable :phone-number "867-5309")
   (should (equal (triples-test-plist-sort '(:name "Name" :alias ("alias1" "alias2")))
                  (triples-test-plist-sort (triples-get-type db "foo" 'named))))
   (should (equal (triples-test-list-sort (triples-get-types db "foo"))
                  (triples-test-list-sort '(callable named))))
   (should-not (triples-get-type db "bar" 'named))
   (should-not (triples-get-types db "bar"))
   (triples-remove-type db "foo" 'named)
   (should-not (triples-get-type db "foo" 'named))
   (should (triples-get-type db "foo" 'callable))))

(ert-deftest triples-crud-all ()
  (triples-test-with-temp-db
    (triples-add-schema db 'named
                        '(name :base/unique t))
    (triples-add-schema db 'positioned '(position :/base/unique t))
    (should-not (triples-get-subject db "foo"))
    (triples-set-subject db "foo"
                         '(named :name "bar")
                         '(positioned :position "right behind you"))
    (should (equal '(:named/name "bar" :positioned/position "right behind you")
                   (triples-get-subject db "foo")))
    (triples-delete-subject db "foo")
    (should-not (triples-get-subject db "foo"))))

(ert-deftest triples-set-types ()
  (triples-test-with-temp-db
    (triples-add-schema db 'named
                        '(name :/base/unique t)
                       'alias)
    (triples-add-schema db 'reachable 'phone)
    (triples-set-type db "foo" 'named :name "Name" :alias '("alias1" "alias2"))
    (triples-set-types db "foo" :named/name "New Name" :reachable/phone '("867-5309"))
    (should (equal (triples-test-plist-sort '(:named/name "New Name" :reachable/phone ("867-5309")))
                   (triples-test-plist-sort (triples-get-subject db "foo"))))))

(ert-deftest triples-single-element ()
  (triples-test-with-temp-db
   (triples-add-schema db 'named 'name)
   (triples-set-type db "foo" 'named :name '("Name"))
   (should (equal '(:name ("Name"))
                  (triples-get-type db "foo" 'named)))))

(ert-deftest triples-store-and-retrieve ()
  (triples-test-with-temp-db
    (triples-add-schema db 'text '(text :base/unique t))
    (let ((text "Foo\nBar\tBaz \"Quoted\" "))
      (triples-set-type db "foo" 'text :text text)
      (let ((retrieved (triples-get-type db "foo" 'text)))
        (should (equal `(:text ,text) retrieved))
        (triples-set-type db "foo" 'text retrieved)
        (should (equal `(:text ,text) (triples-get-type db "foo" 'text)))))))

(ert-deftest triples-vector ()
  (triples-test-with-temp-db
   (triples-add-schema db 'named 'name)
   (triples-add-schema db 'embedding '(embedding :base/unique t :base/type vector))
   (triples-set-type db "foo" 'named :name '("Name"))
   (triples-set-type db "foo" 'embedding :embedding [1 2 3 4 5])
   (should (equal '(:embedding [1 2 3 4 5])
                  (triples-get-type db "foo" 'embedding)))
   (should-error (triples-set-type db "foo" 'embedding :embedding '(1 2 3)))))

(ert-deftest triples-reversed ()
  (triples-test-with-temp-db
   (triples-add-schema db 'named
                       '(name :base/unique t)
                       '(locale :base/unique t))
   (triples-add-schema db 'locale
                       '(used-in-name :base/virtual-reversed named/locale))
   (triples-set-type db "en/US" 'locale nil)
   (should-not (triples-get-type db "en/US" 'locale))
   (triples-set-type db "foo" 'named :name "foo" :locale "en/US")
   (should (equal '(:used-in-name ("foo"))
                  (triples-get-type db "en/US" 'locale)))
   (should-error (triples-set-type db "en/US" 'locale :used-in-name '("bar")))))

(ert-deftest triples-with-predicate ()
  (triples-test-with-temp-db
   (triples-add-schema db 'named '(name))
   (should-not (triples-with-predicate db 'named/name))
   (triples-set-type db "foo" 'named :name "My Name Is Fred Foo")
   (triples-set-type db "bar" 'named :name "My Name Is Betty Bar")
   (should (equal
            (triples-test-list-sort
             '(("bar" named/name "My Name Is Betty Bar" nil)
               ("foo" named/name "My Name Is Fred Foo" nil)))
            (triples-test-list-sort
             (triples-with-predicate db 'named/name))))))

(ert-deftest triples-subjects-of-type ()
  (triples-test-with-temp-db
    (triples-add-schema db 'named '(name))
    (should-not (triples-subjects-of-type db 'named))
    (triples-set-type db "foo" 'named :name "My Name Is Fred Foo")
    (triples-set-type db "bar" 'named :name "My Name Is Betty Bar")
    (should (seq-set-equal-p '("foo" "bar")
                             (triples-subjects-of-type db 'named)))))

(ert-deftest triples-no-dups ()
  (triples-test-with-temp-db
    ;; Just add a marker schema, no attributes
    (triples-add-schema db 'marker)
    (triples-set-type db "foo" 'marker)
    (should (equal '((1))
                   (sqlite-select db "SELECT COUNT(*) FROM triples WHERE subject = ? AND predicate = 'base/type' AND object = 'marker'"
                                  (list (triples-standardize-val "foo")))))
    (triples-set-type db "foo" 'marker)
    (should (equal '((1))
                   (sqlite-select db "SELECT COUNT(*) FROM triples WHERE subject = ? AND predicate = 'base/type' AND object = 'marker'"
                                  (list (triples-standardize-val "foo")))))))

(ert-deftest triples-move-subject ()
  (triples-test-with-temp-db
   (triples-add-schema db 'named '(name))
   (triples-add-schema db 'friend '(id))
   (triples-set-subject db 123 '(named :name "Ada Lovelace"))
   (triples-set-subject db 456 '(named :name "Michael Faraday")
                        '(friend :id 123))
   (triples-set-subject db 987 '(named :name "To Be Deleted"))
   (should-error (triples-move-subject db 123 987))
   (triples-delete-subject db 987)
   (triples-move-subject db 123 987)
   (should-not (triples-get-subject db 123))
   (should (equal "Ada Lovelace" (plist-get (triples-get-subject db 987) :named/name)))
   (should (equal 987 (plist-get (triples-get-subject db 456) :friend/id)))))

(ert-deftest triples-test-subjects-with-predicate-object-unique-subject ()
  (triples-test-with-temp-db
    (triples-add-schema db 'named '(name))
    (triples-set-subject db 123 '(named :name ("Foo" "Foo")))
    (should (= 1 (length (triples-subjects-with-predicate-object db 'named/name "Foo"))))))

(ert-deftest triples-readme ()
  (triples-test-with-temp-db
   (triples-add-schema db 'person
       '(name :base/unique t :base/type string)
       '(age :base/unique t :base/type integer))
   (triples-add-schema db 'employee
       '(id :base/unique t :base/type integer)
       '(manager :base/unique t)
       '(reportees :base/virtual-reversed employee/manager))
   ;; Set up catherine and dennis
  (triples-set-type db "catherine" 'employee :manager "alice")
  (triples-set-type db "dennis" 'employee :manager "alice")
  (triples-delete-subject db "alice")
  (triples-set-type db "alice" 'person :name "Alice Aardvark" :age 41)
  (triples-set-type db "alice" 'employee :id 1901 :manager "bob")
  (should (equal (triples-test-plist-sort (triples-get-subject db "alice"))
                 (triples-test-plist-sort '(:person/name "Alice Aardvark" :person/age 41
                                                         :employee/id 1901
                                                         :employee/manager "bob"
                                                         :employee/reportees ("catherine" "dennis")))))
  (triples-set-subject db "alice" '(person :name "Alice Aardvark" :age 41)
                       '(employee :id 1901 :manager "bob"))
  (should (equal (triples-test-plist-sort (triples-get-subject db "alice"))
                 (triples-test-plist-sort '(:person/name "Alice Aardvark" :person/age 41
                                                         :employee/id 1901
                                                         :employee/manager "bob"
                                                         :employee/reportees ("catherine" "dennis")))))))


(provide 'triples-test)

;;; triples-test.el ends here
