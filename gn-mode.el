;;; package --- Summary

;;; Commentary:

;;; Code:
(require 's)

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
