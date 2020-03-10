;;; package --- Summary

;;; Commentary:

;;; Code:
(require 's)
(require 'gn-mode)

(defun graph-notes-grep-current-file-name-and-go-to-buffer ()
  "Display the result of a grep for the current file name (without extension) in a new buffer, and go to that buffer."
	(interactive)
	(let ((b-name (car (s-split "[.]" (buffer-name)))))
	  (let ((buffer (generate-new-buffer (s-lex-format "in-links<${b-name}>"))))
		(shell-command (s-lex-format "grep -nwrH -C1 --exclude-dir=.git \"${b-name}\" .") buffer)
		(set-buffer buffer)
		(gn-mode)
		(pop-to-buffer buffer))))

(defvar graph-notes-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-c n l") 'graph-notes-grep-current-file-name-and-go-to-buffer)
	map))

(define-minor-mode graph-notes-mode
  "Organize your notes in a graph"
  :lighter " graph-notes"
  :group 'graph-notes
  :keymap graph-notes-mode-map)

(provide 'graph-notes)
;;; graph-notes ends here
