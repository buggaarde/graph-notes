;;; graph-notes-db --- The sqlite backend for graph notes -*- lexical-binding: t; -*-

;; Copyright © 2020 Simon Bugge Siggaard <buggaarde@gmail.com>

;; Author: Simon Bugge Siggaard <buggaarde@gmail.com>
;; URL: https://github.com/buggaarde/graph-notes
;; Keywords: non-hierarchical, notes, note, notetaking, productivity, creativity
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (emacsql "3.0.0") (emacsql-sqlite "1.0.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:
(require 'emacsql)
(require 'emacsql-sqlite)

;;;; Schemas:
(defvar graph-notes-db--tags-schema
  '([(id integer :primary-key)
	 file-name])
  "The table containing all graph-note tags.

The table also contains the name of the file representing the tag.")

(defvar graph-notes-db--links-schema
  '([(id integer :primary-key)
	 (from-tag-id integer)
	 (to-tag-id integer)]
	(:foreign-key [from-tag-id] :references tags [id])
	(:foreign-key [to-tag-id] :references tags [id]))
  "The table connects all links between tags.")

(defvar graph-notes-db--synonyms-schema
  '([(id integer :primary-key)
	 name
	 (tag-id integer)]
	(:foreign-key [tag-id] :references tags [id]))
  "The table associates string names with the more abstract foreign-key tag id.")

;;;; Helper functions:
(defun graph-notes-db--backlinks-to-file (db file-name)
  "Return all tags containing backlinks to tag given by file name.

DB is the sqlite database.
FILE-NAME is the file name."
  (emacsql db [:select [links.from-tag-id]
			   :from links
			   :inner-join tags
			   :on (= links.to-tag-id tags.id)
			   :where (= tags.file-name $r1)]
		   file-name))

(defun graph-notes-db--tags-in-file (db file-name)
  "Return all tags in given file.

DB is the sqlite database.
FILE-NAME is the file name."
  (emacsql db [:select [links.to-tag-id]
			   :from links
			   :inner-join tags
			   :on (= links.from-tag-id tags.id)
			   :where (= tags.file-name $r1)]
		   file-name))

(defun graph-notes-db--tag-synonyms (db tag)
  "Return all synonyms for the given tag.

DB is the sqlite database.
TAG is the tag id."
  (emacsql db [:select [synonyms.name]
			   :from synonyms
			   :inner-join tag
			   :on (= synonyms.tag-id tags.id)
			   :where (= tags.id $s1)]
		   tag))

(defun graph-notes-db--tag-id-from-file-name (db file-name)
  "Return the id of the tag corresponding to the current file name.

DB is the sqlite database.
FILE-NAME is the file name."
  (emacsql db [:select [id]
			   :from tags
			   :where (= file-name $r1)]
		   file-name))

(provide 'graph-notes-db)
;;; graph-notes-db.el ends here
