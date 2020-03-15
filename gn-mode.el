;; -*- lexical-binding: t; -*-
;;; package --- Summary

;;; Commentary:

;;; Code:
(require 's)

(defvar gn--file-name-regex "\\(.*\\):[0-9]+:")
(defvar gn--linum-regex "^:\\([0-9]+\\):")

(define-button-type 'gn--default-button
  'face 'default)

(defun gn--create-links-buffer-from-grep (grep-buffer)
  "Documentation.

GREP-BUFFER is the buffer that contains the output of the grep"
  (let ((this-buffer (buffer-name)))
	(set-buffer grep-buffer)
	(let* ((grep-list (s-split "\n--\n--\n" (buffer-string)))
		   (grep-button (lambda (tag-and-context)
						  (let* ((file-name (nth 1 (s-match gn--file-name-regex tag-and-context)))
								 (raw-button-content (s-replace file-name "" tag-and-context))
								 (linenumber (string-to-number (nth 1 (s-match gn--linum-regex raw-button-content)))))
														
							(insert (propertize file-name 'font-lock-face 'bold))
							(insert "\n")
							(insert-text-button
							 raw-button-content
							 'type 'gn--default-button
							 'action (lambda (button)
									   (progn
										 (find-file file-name)
										 (goto-char (point-min))
										 (forward-line (1- linenumber))))
				
							 'follow-link t
							 'help-echo "Click to go to file")
							
							(insert "\n\n\n")))))
	  (set-buffer this-buffer)
	  (erase-buffer)
	  (mapc grep-button grep-list))))

(defvar gn--font-locks nil "Test.")

(setq gn--font-locks
	  '(((gn--tag-name-from-links-buffer) . (0 font-lock-constant-face))))

(defun gn--tag-name-from-links-buffer ()
  "All tag names are of the form `in-links<tag>'."
  (s-chop-suffix ">" (cdr (s-split "<" (buffer-name)))))

(defun gn--quit-and-kill-buffer ()
  "Both quit and kill the current buffer."
  (interactive)
  (kill-buffer-and-window))

(defvar current-context 1)

(defvar gn-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "q") 'gn--quit-and-kill-buffer)
	map)
  "The keymap for the graph-notes links buffer.")

(define-derived-mode gn-mode fundamental-mode "gn"
  "The major mode of the graph-notes links buffer.

//{gn-mode-map}"
  (setq font-lock-defaults '(gn--font-locks)))

(provide 'gn-mode)
;;; gn-mode ends here
