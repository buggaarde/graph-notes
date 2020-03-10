;;; package --- Summary

;;; Commentary:

;;; Code:
(require 's)

(defun gn--default-button-pressed (button)
  "The default action when a button is pressed in the links buffer.

BUTTON is the button that carries out the default action."
  (message (format "Button is pressed!")))

(define-button-type 'gn--in-link-button
  'face 'default
  'action 'gn--default-button-pressed
  'help-echo "click this button"
  'follow-link t)

(defvar gn--file-name-regex "\\(.*\\):[0-9]+:")
(defvar gn--linum-regex "^[-:][0-9]+[-:]")

(defun gn--create-links-buffer-from-grep (grep-buffer)
  "Documentation.

GREP-BUFFER is the buffer that contains the output of the grep"
  (progn
	(let ((this-buffer (buffer-name)))
	  (set-buffer grep-buffer)
	  (let ((grep-list (s-split "\n--\n--\n" (buffer-string)))
			(grep-button
			 (lambda (tag-and-context)
			   (let ((file-name (nth 1 (s-match gn--file-name-regex tag-and-context))))
				 (let ((raw-button-content (s-replace file-name "" tag-and-context)))
				   ;; (let ((formatted-button-content
				   ;; 		  (s-join "\n"
				   ;; 				  (mapcar (lambda (line)
				   ;; 							(let ((n (car (s-match gn--linum-regex line))))
				   ;; 							  (s-replace n (s-append " " n) line)))
				   ;; 						  (s-lines raw-button-content))))))
				   (insert (propertize file-name 'font-lock-face 'bold))
				   (insert "\n")
				   (insert-text-button
					(make-text-button raw-button-content (length raw-button-content))
					:type 'gn--in-link-button)
				   (insert "\n\n\n"))))))
		(set-buffer this-buffer)
		(erase-buffer)
		(mapc grep-button grep-list)))))

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

(defun gn--next-line-with-context ()
  (interactive)
  (next-line (+ 2 (+ 1 (* 2 current-context)))))

(defun gn--previous-line-with-context ()
  (interactive)
  (next-line (+ 2 (+ 1 (* 2 current-context)))))

(defvar gn-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "q") 'gn--quit-and-kill-buffer)
	(define-key map (kbd "<DOWN>") 'gn--next-line-with-context)
	(define-key map (kbd "<UP>") 'gn--previous-line-with-context)
	map)
  "The keymap for the graph-notes links buffer.")

(define-derived-mode gn-mode fundamental-mode "gn"
  "The major mode of the graph-notes links buffer.

//{gn-mode-map}"
  (setq font-lock-defaults '(gn--font-locks)))

(provide 'gn-mode)
;;; gn-mode ends here
