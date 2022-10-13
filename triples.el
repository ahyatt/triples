;;; triples.el --- A flexible triple-based database for us in apps.  -*- lexical-binding: t; -*-

;; Copyright (c) 2022  Free Software Foundation, Inc.

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/triples
;; Package-Requires: ((emacsql "3.0.0") (emacs "29") cl-lib (seq "2.0"))
;; Keywords: triples, kg
;; Version: 0.0
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

(require 'cl-macs)
(require 'emacsql)
(require 'seq)

;;; Code:

(defun triples-connect (file)
  "Connect to the database FILE and make sure it is populated."
  (let* ((db (emacsql-sqlite3 file))
         (triple-table-exists
          (emacsql db [:select name
                       :from sqlite_master
                       :where (= type table) :and (= name 'triples)])))
    (unless triple-table-exists
      (emacsql db [:create-table triples ([(subject text :not-null)
                                               (predicate text :not-null)
                                               (object :not-null)
                                               (properties)])])
      (emacsql db [:create-index subject_idx :on triples [subject]])
      (emacsql db [:create-index subject_predicate_idx :on triples [subject predicate]])
      (emacsql db [:create-index predicate_object_idx :on triples [predicate object]])
      (emacsql db [:create-unique-index subject_predicate_object_properties_idx :on triples [subject predicate object properties]]))
    db))

(defun triples--ensure-property-val (vec)
  "Return a VEC has 4 elements.
We add a bogus value as a property because we want to be able
to enforce unique constraints, which sqlite will not do will NULL
values."
  (if (= (length vec) 4)
      vec
    (vconcat vec '((:empty t)))))

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

(defun triples--decolon (sym)
  "Remove colon from SYM."
  (intern (string-replace ":" "" (format "%s" sym))))

(defun triples--encolon (sym)
  "Add a colon to SYM."
  (intern (format ":%s" sym)))

(defun triples--add (db op)
  "Perform OP on DB."
  (pcase (car op)
      ('replace-subject
       (mapc
        (lambda (sub)
          (emacsql db [:delete :from triples :where (= subject $s1)] sub))
        (triples--subjects (cdr op))))
      ('replace-subject-type
       (mapc (lambda (sub-triples)
               (mapc (lambda (type)
                       ;; We have to ignore base, which keeps type information in general.
                       (unless (eq type 'base)
                         (emacsql db [:delete :from triples :where (= subject $s1)
                                      :and (like predicate $r2)]
                                  (car sub-triples) (format "%s/%%" type))))
                     (seq-uniq
                      (mapcar #'car (mapcar #'triples-combined-to-type-and-prop
                                                     (mapcar #'cl-second (cdr sub-triples)))))))
             (triples--group-by-subjects (cdr op)))))
    (mapc (lambda (triple)
            (emacsql db [:replace :into triples
                         :values $v1] (triples--ensure-property-val
                                       (apply #'vector triple))))
          (cdr op)))

(defun triples-properties-for-predicate (db cpred)
  "Return the properties in DB for combined predicate CPRED as a plist."
  (mapcan (lambda (row)
            (list (intern (format ":%s" (nth 1 row))) (nth 2 row)))
          (emacsql db [:select * :from triples :where (= subject $s1)] cpred)))

(defun triples-predicates-for-type (db type)
  "Return all predicates defined for TYPE in DB."
  (mapcar #'car
          (emacsql db [:select object :from triples :where (= subject $s1)
                       :and (= predicate 'schema/property)] type)))

(defun triples-verify-schema-compliant (db triples)
  "Error if TRIPLES is not compliant with schema in DB."
  (mapc (lambda (triple)
          (pcase-let ((`(,type . ,prop) (triples-combined-to-type-and-prop (nth 1 triple))))
            (unless (or (eq type 'base)
                        (emacsql db [:select * :from triples :where (= subject $s1)
                                     :and (= predicate 'schema/property) :and (= object $s2)]
                                 type prop))
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
    (emacsql-with-transaction db
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
          (emacsql db [:select * :from triples :where (= subject $s1)
                       :and (like predicate $r2)] subject (format "%s/%%" type)))
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
                  (let ((result (emacsql db [:select subject :from triples :where (= object $s1)
                                             :and (= predicate $s2)] subject reversed-prop)))
                    (when result (cons (triples--encolon pred) (list (mapcar #'car result)))))))))))

(defun triples-remove-type (db subject type)
  "Remove TYPE for SUBJECT in DB, and all associated data."
  (emacsql-with-transaction db
    (emacsql db [:delete :from triples :where (= subject $s1)
                 :and (= predicate 'base/type)] subject)
    (emacsql db [:delete :from triples :where (= subject $s1)
                 :and (like $r2)] subject (format "%s/%%" type))))

(defun triples-get-types (db subject)
  "From DB, get all types for SUBJECT."
  (mapcar #'car (emacsql db [:select object :from triples :where (= subject $s1)
                             :and (= predicate 'base/type)]
                         subject)))

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
  (emacsql-with-transaction db
    (triples-delete-subject db subject)
    (mapc (lambda (cons)
            (apply #'triples-set-type db subject cons))
          type-vals-cons)))

(defun triples-delete-subject (db subject)
  "Delete all data in DB associated with SUBJECT."
  (emacsql-with-transaction db
    (emacsql db [:delete :from triples :where (= subject $s1)] subject)))

(defun triples-search (db cpred text)
  "Search DB for instances of combined property CPRED with TEXT."
  (emacsql db [:select * :from triples :where (= predicate $i1)
               :and (like object $r2)] (triples--decolon cpred)
                       (format "%%%s%%" text)))

(defun triples-with-predicate (db cpred)
  "Return all triples in DB with CPRED as its combined predicate."
  (emacsql db [:select * :from triples :where (= predicate $i1)] (triples--decolon cpred)))

(defun triples-subjects-with-predicate-object (db cpred obj)
  "Return all subjects in DB with CPRED equal to OBJ."
  (emacsql db [:select subject :from triples :where (= predicate $i1) :and (= object $s2)]
           (triples--decolon cpred) obj))

(defun triples-subjects-of-type (db type)
  "Return a list of all subjects with a particular TYPE in DB."
  (mapcar #'car (triples-subjects-with-predicate-object db 'base/type type)))

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
