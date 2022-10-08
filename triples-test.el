;;; triples-test.el --- Tests for triples module.
(require 'triples)
(require 'seq)
(require 'kv)

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
             ,@body))
       (delete-file db-file))))

(defun triples-test-connect-db ()
  (interactive)
  (defvar sql-database)
  (let ((sql-database triples-test-db-file))
    (sql-sqlite (format "*schema test db SQL %s*" triples-test-db-file))))

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
  (sort list (lambda (a b) (string< (format "%s" a) (format "%s" b))) ))

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
                       '(name :unique t)
                       'alias)
   (triples-set-type db "foo" 'named :name "Name" :alias '("alias1" "alias2"))
   (should (equal (triples-test-plist-sort '(:name "Name" :alias ("alias1" "alias2")))
                  (triples-test-plist-sort (triples-get-type db "foo" 'named))))
   (should (equal (triples-get-types db "foo") '(named)))
   (should-not (triples-get-type db "bar" 'named))
   (should-not (triples-get-types db "bar"))
   (should (equal '(named) (triples-get-types db "foo")))
   (triples-remove-type db "foo" 'named)
   (should-not (triples-get-types db "foo"))))

(ert-deftest triples-crud-all ()
  (triples-test-with-temp-db
    (triples-add-schema db 'named
                        '(name :unique t))
    (triples-add-schema db 'positioned '(position :unique t))
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
                       '(name :unique t)
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

(ert-deftest triples-reversed ()
  (triples-test-with-temp-db
   (triples-add-schema db 'named
                       '(name :unique t)
                       '(locale :unique t))
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
   (should-not (triples-with-predicate db :named/name))
   (triples-set-type db "foo" 'named :name "My Name Is Fred Foo")
   (triples-set-type db "bar" 'named :name "My Name Is Betty Bar")
   (should (equal
            '(("bar" named/name "My Name Is Betty Bar" (:empty t))
              ("foo" named/name "My Name Is Fred Foo" (:empty t)))
            (sort (triples-with-predicate db :named/name)
                  (lambda (a b)
                    (string< (car a) (car b))))))))

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
                (emacsql db [:select (funcall count) :from triples :where (= subject $s1)
                             :and (= predicate 'base/type) :and (= object 'marker)]
                         "foo")))
    (triples-set-type db "foo" 'marker)
    (should (equal '((1))
                (emacsql db [:select (funcall count) :from triples :where (= subject $s1)
                             :and (= predicate 'base/type) :and (= object 'marker)]
                         "foo")))))

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
