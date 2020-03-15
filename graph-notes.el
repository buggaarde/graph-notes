;;; package --- Summary

;;; Commentary:

;;; Code:
(require 's)
(require 'gn-mode)

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
