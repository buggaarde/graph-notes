;;; graph-notes.el --- Non-hierarchical note taking in Emacs -*- lexical-binding: t -*-

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
(require 'dash)
(require 'f)
(require 'graph-notes-db)
(require 's)

;;;; Customizable Variables
(defgroup graph-notes nil
  "Non-hierarchical note taking"
  :prefix "graph-notes-"
  :link '(url-link :tag "github" "https://github.com/buggaarde/graph-notes"))

(require 'graph-notes-backlinks)

(defvar graph-notes-db (emacsql-sqlite "~/graph-notes.db"))
(emacsql graph-notes-db [:create-table :if-not-exists tags $S1] graph-notes-db--tags-schema)
(emacsql graph-notes-db [:create-table :if-not-exists files $S1] graph-notes-db--files-schema)
(emacsql graph-notes-db [:create-table :if-not-exists file-tag $S1] graph-notes-db--file-tag-schema)
(emacsql graph-notes-db [:create-table :if-not-exists synonyms $S1] graph-notes-db--synonyms-schema)

(defcustom graph-notes-directory (expand-file-name "~/graph-notes/")
  "All files in this directory are part of the note graph."
  :type 'directory
  :set (lambda (symbol value)
		 "All files in the specified directory have graph-nodes-mode enabled."
		 (let ((directory (expand-file-name value)))
		   (remove-hook 'find-file-hook #'graph-notes--directory-hook)
		   ;; (remove-function after-focus-change-function
		   ;; 					#'graph-notes--rebuild-backlinks-hook)
		   
		   (defun graph-notes--directory-hook ()
			 (when (string-match
					  (s-lex-format "${directory}.*") (buffer-file-name))
			   (graph-notes-mode 1)))

		   ;; (defun graph-notes--rebuild-backlinks-hook ()
		   ;; 	 (when (string-match
		   ;; 			(s-lex-format "${directory}.*") (buffer-file-name))
		   ;; 	   (message (buffer-name))
		   ;; 	   (graph-notes--grep-file-name-in-current-directory)))
		   
		   (add-hook 'find-file-hook #'graph-notes--directory-hook)
		   ;; (add-function :after after-focus-change-function
		   ;; 				 #'graph-notes--rebuild-backlinks-hook)

		   (setq symbol value)))
  :group 'graph-notes)

(defcustom graph-notes-file-extension ".org"
  "All note files must have the same file ending."
  :type 'string
  :group 'graph-notes)


;;;; Core
(defvar graph-notes--all-tags '())

(define-button-type 'graph-notes--default-button
  'face 'default)

(defun graph-notes-insert-tag (tag)
  "Insert a tag and create the necessary files accordingly.

TAG is the name of the tag inserted, as well as the name of the file created."
  (interactive)
  (progn
	(graph-notes--create-new-tag
	 (s-lex-format "${tag}"))
	(insert-text-button
	 (s-lex-format "${tag}")
	 'face 'graph-notes--default-button)))

(defun graph-notes--create-new-tag (tag)
  "Create a new tag by adding to the graph-notes--all-tags list.
Also create a file with the appropriate name.

TAG is the name of the tag."
  (interactive)
  (let ((raw-extension (s-chop-prefix "." graph-notes-file-extension)))
	(f-touch (s-lex-format "${graph-notes-directory}/${tag}.${raw-extension}"))
	(setq graph-notes--all-tags
		  (-concat graph-notes--all-tags `(,(s-lex-format "${tag}"))))))

(defun graph-notes--list-all-tags ()
  "Return the list of all tags."
  graph-notes--all-tags)

(defun graph-notes--remove-tag (tag)
  "Remove a tag.
TAG is the name of the tag."
  (let ((raw-extension (s-chop-prefix "." graph-notes-file-extension)))
	(setq graph-notes--all-tags (--remove (s-equals? it tag) graph-notes--all-tags))
	(f-delete (s-lex-format "${graph-notes-base-directory}/${tag}.${raw-extension}"))))

(defun graph-notes--grep-file-name-in-current-directory ()
  "Display the result of a grep for the current file name (without extension) in a new buffer, and go to that buffer."
	(interactive)
	(let* ((content-buffer (buffer-name))
		   (b-name (car (s-split "[.]" content-buffer)))
		   (backlinks-buffer
			(get-buffer-create (s-lex-format "backlinks<${b-name}>")))
		   (grep-buffer
			(get-buffer-create (s-lex-format "grep:backlinks<${b-name}>")))
		   (grep-string (with-temp-buffer
						  (shell-command
						   (s-lex-format "grep -nwrH -C1 --exclude-dir=.git \"${b-name}\" .") t)
						  (buffer-string))))

	  (with-current-buffer grep-buffer
		(erase-buffer)
		(insert grep-string))

	  (save-current-buffer
		(with-current-buffer backlinks-buffer
		  (graph-notes-backlinks--create-links-buffer-from-grep grep-buffer content-buffer)
		  (beginning-of-buffer)
		  (graph-notes-backlinks-mode)
		  (visual-line-mode)
		  (pop-to-buffer backlinks-buffer)))))

(defvar graph-notes-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-c n l") 'graph-notes--grep-file-name-in-current-directory)
	map))

(define-minor-mode graph-notes-mode
  "Organize your notes in a graph"
  :lighter " graph-notes"
  :group 'graph-notes
  :keymap graph-notes-mode-map)

(provide 'graph-notes)
;;; graph-notes ends here
