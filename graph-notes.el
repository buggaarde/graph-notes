;;; package --- Summary

;;; Commentary:

;;; Code:
(require 'dash)
(require 'f)
(require 's)
(require 'gn-mode)

(defvar graph-notes-base-directory nil)
(defvar graph-notes--default-file-extension ".org")
(defvar graph-notes-file-extension graph-notes--default-file-extension)
(defvar graph-notes--all-tags '())

(defun graph-notes--create-new-tag (tag)
  "Create a new tag by adding to the graph-notes--all-tags list.
Also create a file with the appropriate name.

TAG is the name of the tag."
  (interactive)
  (let ((raw-extension (s-chop-prefix "." graph-notes-file-extension)))
	(f-touch (s-lex-format "${graph-notes-base-directory}/${tag}.${raw-extension}"))
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

(defun graph-notes--grep-file-name-in-current-directory-and-go-to-buffer ()
  "Display the result of a grep for the current file name (without extension) in a new buffer, and go to that buffer."
	(interactive)
	(let* ((b-name (car (s-split "[.]" (buffer-name))))
		   (in-links-buffer
			(get-buffer-create (s-lex-format "in-links<${b-name}>")))
		   (grep-buffer
			(get-buffer-create (s-lex-format "*grep:in-links<${b-name}>*")))
		   (grep-string (with-temp-buffer
						  (shell-command
						   (s-lex-format "grep -nwrH -C1 --exclude-dir=.git \"${b-name}\" .") t)
						  (buffer-string))))

	  (set-buffer grep-buffer)
	  (erase-buffer)
	  (insert grep-string)
		  
	  (set-buffer in-links-buffer)
	  (gn--create-links-buffer-from-grep grep-buffer)
	  (beginning-of-buffer)
	  (gn-mode)
	  (pop-to-buffer in-links-buffer)))

(defvar graph-notes-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-c n l") 'graph-notes--grep-file-name-in-current-directory-and-go-to-buffer)
	map))

(define-minor-mode graph-notes-mode
  "Organize your notes in a graph"
  :lighter " graph-notes"
  :group 'graph-notes
  :keymap graph-notes-mode-map)

(provide 'graph-notes)
;;; graph-notes ends here
