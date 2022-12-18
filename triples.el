;;; triples.el --- A flexible triple-based database for us in apps.  -*- lexical-binding: t; -*-

;; Copyright (c) 2022  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/triples
;; Package-Requires: ((seq "2.0") (emacs "25"))
;; Keywords: triples, kg, data, sqlite
;; Version: 0.2
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
;; Triples is a library implementing a data storage based on the idea of
;; triples: subject, predicate, objects, plus some extra metadata. This data
;; structure provides a way to store data according to an extensible schema, and
;; provide an API offering two-way links between all information stored.
;;
;; This package requires either emacs 29 or the emacsql package to be installed.


(require 'cl-lib)
(require 'package)
(require 'seq)
(require 'subr-x)

;;; Code:

(defvar triples-sqlite-interface
  (if (and (fboundp 'sqlite-available-p) (sqlite-available-p))
      'builtin
    'emacsql)
  "The interface to sqlite to use.
Either `builtin' or `emacsql'. Defaults to builtin when
available. Builtin is available when the version is Emacs 29 or
greater, and emacsql is usable when the `emacsql' package is
installed.")

(defconst triples-sqlite-executable "sqlite3"
  "If using emacs 29 builtin sqlite, this specifices the executable.
It is invoked to make backups.")

(defconst triples-default-database-filename (locate-user-emacs-file "triples.db")
  "The default filename triples database. If no database is
specified, this file is used.")

(defun triples-connect (&optional file)
  "Connect to the database FILE and make sure it is populated.
If FILE is nil, use `triples-default-database-filename'."
  (unless (pcase-exhaustive triples-sqlite-interface
              ('builtin
               (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
              ('emacsql (require 'emacsql nil t)))
    (error "The triples package requires either Emacs 29 or the emacsql package to be installed."))
  (let ((file (or file triples-default-database-filename)))
    (pcase triples-sqlite-interface
      ('builtin (let* ((db (sqlite-open file)))
                  (sqlite-execute db "CREATE TABLE IF NOT EXISTS triples(subject TEXT NOT NULL, predicate TEXT NOT NULL, object NOT NULL, properties TEXT NOT NULL)")
                  (sqlite-execute db "CREATE INDEX IF NOT EXISTS subject_idx ON triples (subject)")
                  (sqlite-execute db "CREATE INDEX IF NOT EXISTS subject_predicate_idx ON triples (subject, predicate)")
                  (sqlite-execute db "CREATE INDEX IF NOT EXISTS predicate_object_idx ON triples (predicate, object)")
                  (sqlite-execute db "CREATE UNIQUE INDEX IF NOT EXISTS subject_predicate_object_properties_idx ON triples (subject, predicate, object, properties)")
                  db))
      ('emacsql
       (require 'emacsql)
       (let* ((db (emacsql-sqlite file))
              (triple-table-exists
               (emacsql db [:select name
                                    :from sqlite_master
                                    :where (= type table) :and (= name 'triples)])))
         (unless triple-table-exists
           (emacsql db [:create-table triples ([(subject :not-null)
                                                (predicate text :not-null)
                                                (object :not-null)
                                                (properties text :not-null)])])
           (emacsql db [:create-index subject_idx :on triples [subject]])
           (emacsql db [:create-index subject_predicate_idx :on triples [subject predicate]])
           (emacsql db [:create-index predicate_object_idx :on triples [predicate object]])
           (emacsql db [:create-unique-index subject_predicate_object_properties_idx :on triples [subject predicate object properties]]))
         db)))))

(defun triples-close (db)
  "Close sqlite database DB."
  (pcase triples-sqlite-interface
    ('builtin (sqlite-close db))
    ('emacsql (emacsql-close db))))

(defun triples-backup (db filename num-to-keep)
  "Perform a backup of DB, located at path FILENAME.
This uses the same backup location and names as configured in
variables such as `backup-directory-alist'. Due to the fact that
the database is never opened as a buffer, normal backups will not
work, therefore this function must be called instead.

Th DB argument is currently unused, but may be used in the future
if emacs's native sqlite gains a backup feature.

This also will clear excess backup files, according to
NUM-TO-KEEP, which specifies how many backup files at max should
exist at any time. Older backups are the ones that are deleted."
  (call-process (pcase triples-sqlite-interface
                  ('builtin triples-sqlite-executable)
                  ('emacsql emacsql-sqlite-executable))
                nil nil nil (expand-file-name filename)
                (format ".backup '%s'" (expand-file-name
                                        (car (find-backup-file-name
                                              (expand-file-name filename))))))
  (let ((backup-files (file-backup-file-names (expand-file-name filename))))
    (cl-loop for backup-file in (cl-subseq
                                 backup-files
                                 (min num-to-keep (length backup-files)))
             do (delete-file backup-file))))

(defun triples--decolon (sym)
  "Remove colon from SYM."
  (intern (string-replace ":" "" (format "%s" sym))))

(defun triples--encolon (sym)
  "Add a colon to SYM."
  (intern (format ":%s" sym)))

(defun triples-standardize-val (val)
  "If VAL is a string, return it as enclosed in quotes
This is done to have compatibility with the way emacsql stores
values. Turn a symbol into a string as well, but not a quoted
one, because sqlite cannot handle symbols."
  (let ((print-escape-control-characters t))
    (if val
        (format "%S" val)
      ;; Just to save a bit of space, let's use "()" instead of "null", which is
      ;; what it would be turned into by the pcase above.
      "()")))

(defun triples-standardize-result (result)
  "Return RESULT in standardized form.
This imitates the way emacsql returns items, with strings
becoming either symbols, lists, or strings depending on whether
the string itself is wrapped in quotes."
  (if (and (stringp result)
           (string-prefix-p "\"" result)
           (string-suffix-p "\"" result))
      (string-remove-suffix "\"" (string-remove-prefix "\"" result))
    (if (numberp result)
        result
      (read result))))

(defun triples-db-insert (db subject predicate object &optional properties)
  "Insert triple to DB: SUBJECT, PREDICATE, OBJECT with PROPERTIES.
This is a SQL replace operation, because we don't want any
duplicates; if the triple is the same, it has to differ at least
with PROPERTIES. This is a low-level function that bypasses our
normal schema checks, so should not be called from client programs."
  (unless (symbolp predicate)
    (error "Predicates in triples must always be symbols"))
  (unless (plistp properties)
    (error "Properties stored must always be plists"))
  (pcase triples-sqlite-interface
    ('builtin 
     (sqlite-execute db "REPLACE INTO triples VALUES (?, ?, ?, ?)"
                     (list (triples-standardize-val subject)
                           (triples-standardize-val (triples--decolon predicate))
                           (triples-standardize-val object)
                           ;; Properties cannot be null, since in sqlite each null value
                           ;; is distinct from each other, so replace would not replace
                           ;; duplicate triples each with null properties.
                           (triples-standardize-val properties))))
    ('emacsql
     ;; We use a simple small plist '(:t t). Unlike sqlite, we can't insert this
     ;; as a string, or else it will store as something that would come out as a
     ;; string.  And if we use nil, it will actually store a NULL in the cell.
     (emacsql db [:replace :into triples :values $v1]
              (vector subject (triples--decolon predicate) object (or properties '(:t t)))))))

(defun triples--emacsql-andify (wc)
  "In emacsql where clause WC, insert `:and' between query elements.
Returns the new list with the added `:and.'s. The first element
MUST be there `:where' clause. This does reverse the clause
elements, but it shouldn't matter."
  (cons (car wc) ;; the :where clause
        (let ((clauses (cdr wc))
              (result))
          (while clauses
            (push (car clauses) result)
            (if (cdr clauses) (push :and result))
            (setq clauses (cdr clauses)))
          result)))

(defun triples-db-delete (db &optional subject predicate object properties)
  "Delete triples matching SUBJECT, PREDICATE, OBJECT, PROPERTIES.
If any of these are nil, they will not selected for. If you set
all to nil, everything will be deleted, so be careful!"
  (pcase triples-sqlite-interface
    ('builtin (sqlite-execute
               db
               (concat "DELETE FROM triples"
                       (when (or subject predicate object properties)
                         (concat " WHERE "
                                 (string-join
                                  (seq-filter #'identity
                                              (list (when subject "SUBJECT = ?")
                                                    (when predicate "PREDICATE = ?")
                                                    (when object "OBJECT = ?")
                                                    (when properties "PROPERTIES = ?")))
                                  " AND "))))
               (mapcar #'triples-standardize-val (seq-filter #'identity (list subject predicate object properties)))))
    ('emacsql
     (let ((n 0))
       (apply #'emacsql
              db
              (apply #'vector
                     (append '(:delete :from triples)
                             (when (or subject predicate object properties)
                               (triples--emacsql-andify 
                                (append
                                 '(:where)
                                 (when subject `((= subject ,(intern (format "$s%d" (cl-incf n))))))
                                 (when predicate `((= predicate ,(intern (format "$s%d" (cl-incf n))))))
                                 (when object `((= object ,(intern (format "$s%d" (cl-incf n))))))
                                 (when properties `((= properties ,(intern (format "$s%d" (cl-incf n)))))))))))
              (seq-filter #'identity (list subject predicate object properties)))))))

(defun triples-db-delete-subject-predicate-prefix (db subject pred-prefix)
  "Delete triples matching SUBJECT and predicates with PRED-PREFIX."
  (unless (symbolp pred-prefix)
    (error "Predicates in triples must always be symbols"))
  (pcase triples-sqlite-interface
    ('builtin (sqlite-execute db "DELETE FROM triples WHERE subject = ? AND predicate LIKE ?"
                  (list (triples-standardize-val subject)
                        (format "%s/%%" (triples--decolon pred-prefix)))))
    ('emacsql (emacsql db [:delete :from triples :where (= subject $s1) :and (like predicate $r2)]
                       subject (format "%s/%%" (triples--decolon pred-prefix))))))

(defun triples-db-select-pred-prefix (db subject pred-prefix)
  "Return rows matching SUBJECT and PRED-PREFIX."
  (pcase triples-sqlite-interface
    ('builtin (mapcar (lambda (row) (mapcar #'triples-standardize-result row))
          (sqlite-select db "SELECT * FROM triples WHERE subject = ? AND predicate LIKE ?"
                         (list (triples-standardize-val subject)
                               (format "%s/%%" pred-prefix)))))
    ('emacsql (emacsql db [:select * :from triples :where (= subject $s1) :and (like predicate $r2)]
                       subject (format "%s/%%" pred-prefix)))))

(defun triples-db-select-predicate-object-fragment (db predicate object-fragment)
  "Return rows with PREDICATE and with OBJECT-FRAGMENT in object."
  (pcase triples-sqlite-interface
    ('builtin (mapcar (lambda (row) (mapcar #'triples-standardize-result row))
                      (sqlite-select db "SELECT * from triples WHERE predicate = ? AND object LIKE ?"
                                     (list (triples-standardize-val predicate)
                                           (format "%%%s%%" object-fragment)))))
    ('emacsql (emacsql db [:select * :from triples :where (= predicate $s1) :and (like object $s2)]
                       predicate (format "%%%s%%" object-fragment)))))

(defun triples-db-select (db &optional subject predicate object properties selector)
  "Return rows matching SUBJECT, PREDICATE, OBJECT, PROPERTIES.
If any of these are nil, they are not included in the select
statement. The SELECTOR is list of symbols subject, precicate,
object, properties to retrieve or nil for *."
  (pcase triples-sqlite-interface
    ('builtin (mapcar (lambda (row) (mapcar #'triples-standardize-result row))
                      (sqlite-select db
                                     (concat "SELECT "
                                             (if selector
                                                 (mapconcat (lambda (e) (format "%s" e)) selector ", ")
                                               "*") " FROM triples"
                                             (when (or subject predicate object properties)
                                               (concat " WHERE "
                                                       (string-join
                                                        (seq-filter #'identity
                                                                    (list (when subject "SUBJECT = ?")
                                                                          (when predicate "PREDICATE = ?")
                                                                          (when object "OBJECT = ?")
                                                                          (when properties "PROPERTIES = ?")))
                                                        " AND "))))
                                     (mapcar #'triples-standardize-val (seq-filter #'identity (list subject predicate object properties))))))
    ('emacsql
     (let ((n 0))
       (apply #'emacsql
              db
              (apply #'vector
                     (append `(:select
                               ,(if selector (apply #'vector selector) '*)
                               :from triples)
                             (when (or subject predicate object properties)
                               (triples--emacsql-andify 
                                (append
                                 '(:where)
                                 (when subject `((= subject ,(intern (format "$s%d" (cl-incf n))))))
                                 (when predicate `((= predicate ,(intern (format "$s%d" (cl-incf n))))))
                                 (when object `((= object ,(intern (format "$s%d" (cl-incf n))))))
                                 (when properties `((= properties ,(intern (format "$s%d" (cl-incf n)))))))))))
              (seq-filter #'identity (list subject predicate object properties)))))))

(defun triples-move-subject (db old-subject new-subject)
  "Replace all instance in DB of OLD-SUBJECT to NEW-SUBJECT.
Any references to OLD-SUBJECT as an object are also replaced.
This will throw an error if there is an existing subject
NEW-SUBJECT with at least one equal property (such as type
markers). But if there are no commonalities, the OLD-SUBJECT is
merged into NEW-SUBJECT."
  (pcase triples-sqlite-interface
    ('builtin
     (condition-case err
         (progn
           (sqlite-transaction db)
           (sqlite-execute db "UPDATE triples SET subject = ? WHERE subject = ?"
                           (list (triples-standardize-val new-subject) (triples-standardize-val old-subject)))
           (sqlite-execute db "UPDATE triples SET object = ? WHERE object = ?"
                           (list (triples-standardize-val new-subject) (triples-standardize-val old-subject)))
           (sqlite-commit db))
       (error (sqlite-rollback db)
              (signal 'error err))))
    ('emacsql
     (emacsql-with-transaction db
         (emacsql db [:update triples :set (= subject $s1) :where (= subject $s2)]
                  new-subject old-subject)
         (emacsql db [:update triples :set (= object $s1) :where (= object $s2)]
                  new-subject old-subject)))))

;; Code after this point should not call sqlite or emacsql directly. If any more
;; calls are needed, put them in a defun, make it work for sqlite and emacsql,
;; and put them above.

(defun triples--subjects (triples)
  "Return all unique subjects in TRIPLES."
  (seq-uniq (mapcar #'car triples)))

(defun triples--group-by-subjects (triples)
  "Return an alist of subject to TRIPLES with that subject."
  (let ((subj-to-triples (make-hash-table :test #'equal)))
    (dolist (triple triples)
      (puthash (car triple)
               (cons triple (gethash (car triple) subj-to-triples))
               subj-to-triples))
    (cl-loop for k being the hash-keys of subj-to-triples using (hash-values v)
             collect (cons k v))))

(defun triples--add (db op)
  "Perform OP on DB."
  (pcase (car op)
      ('replace-subject
       (mapc
        (lambda (sub)
          (triples-db-delete db sub))
        (triples--subjects (cdr op))))
      ('replace-subject-type
       (mapc (lambda (sub-triples)
               (mapc (lambda (type)
                       ;; We have to ignore base, which keeps type information in general.
                       (unless (eq type 'base)
                         (triples-db-delete-subject-predicate-prefix db (car sub-triples) type)))
                     (seq-uniq
                      (mapcar #'car (mapcar #'triples-combined-to-type-and-prop
                                                     (mapcar #'cl-second (cdr sub-triples)))))))
             (triples--group-by-subjects (cdr op)))))
  (mapc (lambda (triple)
          (apply #'triples-db-insert db triple))
          (cdr op)))

(defun triples-properties-for-predicate (db cpred)
  "Return the properties in DB for combined predicate CPRED as a plist."
  (mapcan (lambda (row)
            (list (intern (format ":%s" (nth 1 row))) (nth 2 row)))
          (triples-db-select db cpred)))

(defun triples-predicates-for-type (db type)
  "Return all predicates defined for TYPE in DB."
  (mapcar #'car
          (triples-db-select db type 'schema/property nil nil '(object))))

(defun triples-verify-schema-compliant (db triples)
  "Error if TRIPLES is not compliant with schema in DB."
  (mapc (lambda (triple)
          (pcase-let ((`(,type . ,prop) (triples-combined-to-type-and-prop (nth 1 triple))))
            (unless (or (eq type 'base)
                        (triples-db-select db type 'schema/property prop nil))
              (error "Property %s not found in schema" (nth 1 triple)))))
        triples)
  (mapc (lambda (triple)
          (triples--plist-mapc (lambda (pred-prop val)
                                 (let ((f (intern (format "triples-verify-%s-compliant"
                                                          (triples--decolon pred-prop)))))
                                 (if (fboundp f)
                                     (funcall f val triple))))
                             (triples-properties-for-predicate db (nth 1 triple)))) triples))

(defun triples-add-schema (db type &rest props)
  "Add schema for TYPE and its PROPS to DB."
  (triples--add db (apply #'triples--add-schema-op type props)))

(defun triples--add-schema-op (type &rest props)
  "Return the operation store schema for TYPE, with PROPS.
PROPS is a list of either property symbols, or lists of
properties of the type and the meta-properties associated with
them."
  (cons 'replace-subject
        (cons `(,type base/type schema)
              (cl-loop for p in props
                       nconc
                       (let* ((pname (if (symbolp p) p (car p)))
                              (pprops (when (listp p) (cdr p)))
                              (pcombined (intern (format "%s/%s" type pname))))
                         (cons (list type 'schema/property pname)
                               (seq-filter #'identity
                                (triples--plist-mapcar
                                   (lambda (k v)
                                     ;; If V is nil, that's the default, so don't
                                     ;; store anything.
                                     (when v
                                       (list pcombined (triples--decolon k) v)))
                                   pprops))))))))

(defun triples-set-type (db subject type &rest properties)
  "Create operation to replace PROPERTIES for TYPE for SUBJECT in DB.
PROPERTIES is a plist of properties, without TYPE prefixes."
  (let ((op (triples--set-type-op subject type properties)))
    (triples-verify-schema-compliant db (cdr op))
    (triples--add db op)))

(defmacro triples-with-transaction (db &rest body)
  "Create a transaction using DB, executing BODY.
The transaction will abort if an error is thrown."
  (declare (indent 0) (debug t))
  `(triples--with-transaction ,db (lambda () ,@body)))

(defmacro triples--eval-when-fboundp (sym form)
  "Delay macroexpansion to runtime if SYM is not yet `fboundp'."
  (declare (indent 1) (debug (symbolp form)))
  (if (fboundp sym)
      form
    `(eval ',form t)))

(defun triples--with-transaction (db body-fun)
  (pcase triples-sqlite-interface
    ('builtin  (condition-case nil
                   (progn
                     (sqlite-transaction db)
                     (funcall body-fun)
                     (sqlite-commit db))
                 (error (sqlite-rollback db))))
    ('emacsql (funcall (triples--eval-when-fboundp emacsql-with-transaction
                         (lambda (db body-fun)
                           (emacsql-with-transaction db (funcall body-fun))))
                       db body-fun))))

(defun triples-set-types (db subject &rest combined-props)
  "Set all data for types in COMBINED-PROPS in DB for SUBJECT.
COMBINED-PROPS is a plist which takes combined properties such as
:named/name and their values. All other data related to the types
given in the COMBINED-PROPS will be removed."
  (let ((type-to-plist (make-hash-table)))
    (triples--plist-mapc
     (lambda (cp val)
       (pcase-let ((`(,type . ,prop) (triples-combined-to-type-and-prop cp)))
         (puthash (triples--decolon type)
                  (plist-put (gethash (triples--decolon type) type-to-plist)
                             (triples--encolon prop) val) type-to-plist)))
     combined-props)
    (triples-with-transaction
      db
      (cl-loop for k being the hash-keys of type-to-plist using (hash-values v)
               do (apply #'triples-set-type db subject k v)))))

(defun triples--set-type-op (subject type properties)
  "Create operation to replace PROPERTIES for TYPE for SUBJECT.
PROPERTIES is a plist of properties, without TYPE prefixes."
  (cons 'replace-subject-type
        (cons (list subject 'base/type type)
              (triples--plist-mapcan
               (lambda (prop v)
                 (if (listp v)
                     (cl-loop for e in v for i from 0
                              collect
                              (list subject
                                    (triples-type-and-prop-to-combined type prop)
                                    e
                                    (list :index i)))
                   (list (list subject (triples-type-and-prop-to-combined type prop) v))))
               properties))))

(defun triples-get-type (db subject type)
  "From DB get data associated with TYPE for SUBJECT."
  (let ((preds (make-hash-table :test #'equal)))
    (mapc (lambda (db-triple)
            (puthash (nth 1 db-triple)
                     (cons (cons (nth 2 db-triple) (nth 3 db-triple))
                           (gethash (nth 1 db-triple) preds))
                     preds))
          (triples-db-select-pred-prefix db subject type))
    (append
     (cl-loop for k being the hash-keys of preds using (hash-values v)
              nconc (list (triples--encolon (cdr (triples-combined-to-type-and-prop k)))
                          (if (and (car v)
                                   (plist-get (cdar v) :index))
                              (mapcar #'car (sort v (lambda (a b)
                                                      (< (plist-get (cdr a) :index)
                                                         (plist-get (cdr b) :index)))))
                            (caar v))))
     (cl-loop for pred in (triples-predicates-for-type db type)
              nconc
              (let ((reversed-prop (plist-get
                                    (triples-properties-for-predicate
                                     db (triples-type-and-prop-to-combined type pred))
                                    :base/virtual-reversed)))
                (when reversed-prop
                  (let ((result
                         (triples-db-select db nil reversed-prop subject nil '(subject))))
                    (when result (cons (triples--encolon pred) (list (mapcar #'car result)))))))))))

(defun triples-remove-type (db subject type)
  "Remove TYPE for SUBJECT in DB, and all associated data."
  (triples-with-transaction
    db
    (triples-db-delete db subject 'base/type type)
    (triples-db-delete-subject-predicate-prefix db subject type)))

(defun triples-get-types (db subject)
  "From DB, get all types for SUBJECT."
  (mapcar #'car
          (triples-db-select db subject 'base/type nil nil '(object))))

(defun triples-get-subject (db subject)
  "From DB return all properties for SUBJECT as a single plist."
  (mapcan (lambda (type)
            (triples--plist-mapcan
             (lambda (k v)
               (list (intern (format ":%s/%s" type (triples--decolon k))) v))
             (triples-get-type db subject type)))
          (triples-get-types db subject)))

(defun triples-set-subject (db subject &rest type-vals-cons)
  "From DB set properties of SUBJECT to TYPE-VALS-CONS data.
TYPE-VALS-CONS is a list of conses, combining a type and a plist of values."
  (triples-with-transaction db
    (triples-delete-subject db subject)
    (mapc (lambda (cons)
            (apply #'triples-set-type db subject cons))
          type-vals-cons)))

(defun triples-delete-subject (db subject)
  "Delete all data in DB associated with SUBJECT."
  (triples-db-delete db subject))

(defun triples-search (db cpred text)
  "Search DB for instances of combined property CPRED with TEXT."
  (triples-db-select-predicate-object-fragment db cpred text))

(defun triples-with-predicate (db cpred)
  "Return all triples in DB with CPRED as its combined predicate."
  (triples-db-select db nil cpred))

(defun triples-subjects-with-predicate-object (db cpred obj)
  "Return all subjects in DB with CPRED equal to OBJ."
  (mapcar #'car (triples-db-select db nil cpred obj)))

(defun triples-subjects-of-type (db type)
  "Return a list of all subjects with a particular TYPE in DB."
  (triples-subjects-with-predicate-object db 'base/type type))

(defun triples-combined-to-type-and-prop (combined)
  "Return cons of type and prop that form the COMBINED normal representation.
This is something of form `:type/prop'."
  (let ((s (split-string (format "%s" combined) "/")))
    (cons (triples--decolon (nth 0 s)) (intern (nth 1 s)))))

(defun triples-type-and-prop-to-combined (type prop)
  "Format TYPE and PROP to a combined format - type/prop."
  (intern (format "%s/%s" (triples--decolon type) (triples--decolon prop))))

(defun triples--plist-mapc (fn plist)
  "Map FN over PLIST, for only side effects.
FN must take two arguments: the key and the value."
  (let ((plist-index plist))
    (while plist-index
      (let ((key (pop plist-index)))
        (funcall fn key (pop plist-index))))))

(defun triples--plist-mapcar (fn plist)
  "Map FN over PLIST, returning an element for every property.
FN must take two arguments: the key and the value."
  (let ((plist-index plist)
        (result))
    (while plist-index
      (let ((key (pop plist-index)))
        (push (funcall fn key (pop plist-index)) result)))
    (nreverse result)))

(defun triples--plist-mapcan (fn plist)
  "Map FN over PLIST, nconcing elements together.
FN must take two arguments: the key and the value."
  (let ((plist-index plist)
        (result))
    (while plist-index
      (let ((key (pop plist-index)))
        (setq result (nconc result (funcall fn key (pop plist-index))))))
    result))

;; Standard properties

(defun triples-verify-base/unique-compliant (uniquep triple)
  "Verify that TRIPLE has an index or not, based on UNIQUEP."
  (if uniquep
      (when (member :index (nth 3 triple))
        (error "Invalid triple found: %s, violates base/unique, should be just one value" triple))
    (unless (member :index (nth 3 triple))
      (error "Invalid triple found: %s, violates base/unique, should be a list of values" triple))))

(defun triples-verify-base/type-compliant (type triple)
  "Verify that TRIPLE's object is of TYPE."
  (unless (eq (type-of (nth 2 triple)) type)
    (error "Triple %s has an object with the wrong type: expected type of %s but was %s"
           triple type (type-of (nth 2 triple)))))

(defun triples-verify-base/virtual-reversed-compliant (_ triple)
  "Virtual reversed properties shouldn't be set manually, so are never compliant."
  (error "Invalid triple found: %s, should not be setting a `base/virtual-reversed' property"
         triple))

(provide 'triples)

;;; triples.el ends here
