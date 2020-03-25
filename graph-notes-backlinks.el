;; -*- lexical-binding: t; -*-
;;; package --- Summary

;;; Commentary:

;;; Code:
(require 's)

(defvar graph-notes-backlinks--file-name-regex "\\(.*\\):[0-9]+:")
(defvar graph-notes-backlinks--linum-regex "^:\\([0-9]+\\):")

(define-button-type 'graph-notes-backlinks--default-button
  'face 'default)

(defun graph-notes-backlinks--create-links-buffer-from-grep (grep-buffer)
  "Documentation.

GREP-BUFFER is the buffer that contains the output of the grep"
  (save-current-buffer
  	(let ((grep-button (with-current-buffer grep-buffer
  						 (let* ((grep-button (lambda (tag-and-context)
  											   (let* ((file-name
  													   (nth 1 (s-match graph-notes-backlinks--file-name-regex tag-and-context)))
  													  (raw-button-content
  													   (s-replace file-name "" tag-and-context))
  													  (linenumber
  													   (string-to-number
  														(nth 1 (s-match graph-notes-backlinks--linum-regex raw-button-content)))))
												 
  												 (insert (propertize file-name 'font-lock-face 'bold))
  												 (insert "\n")
  												 (insert-text-button
  												  raw-button-content
  												  'type 'graph-notes-backlinks--default-button
  												  'action (lambda (button)
  															(progn
  															  (find-file file-name)
  															  (goto-char (point-min))
  															  (forward-line (1- linenumber))))
												  
  												  'follow-link t
  												  'help-echo "Click to go to file")
												 
  												 (insert "\n\n\n")))))
  						   grep-button)))
  		  (grep-list (with-current-buffer grep-buffer
					   (s-split "\n--\n--\n" (buffer-string)))))
  	  (erase-buffer)
  	  (mapc grep-button grep-list))))

(defvar graph-notes-backlinks--font-locks nil "Test.")

(setq graph-notes-backlinks--font-locks
	  '(((graph-notes-backlinks--tag-name-from-links-buffer) . (0 font-lock-constant-face))))

(defun graph-notes-backlinks--tag-name-from-links-buffer ()
  "All tag names are of the form `in-links<tag>'."
  (s-chop-suffix ">" (cdr (s-split "<" (buffer-name)))))

(defun graph-notes-backlinks--quit-and-kill-buffer ()
  "Both quit and kill the current buffer."
  (interactive)
  (kill-buffer-and-window))

(defvar graph-notes-backlinks-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "q") 'graph-notes-backlinks--quit-and-kill-buffer)
	map)
  "The keymap for the graph-notes links buffer.")

(define-derived-mode graph-notes-backlinks-mode fundamental-mode "backlinks"
  "The major mode of the graph-notes links buffer.

//{graph-notes-backlinks-mode-map}"
  (setq font-lock-defaults '(graph-notes-backlinks--font-locks)))

(provide 'graph-notes-backlinks)
;;; graph-notes-backlinks ends here
