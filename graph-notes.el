(require 's)

(define-minor-mode graph-notes-mode
  "Organize your notes in a graph"
  :lighter " graph-notes")

(define-minor-mode gn-mode
  "The minor mode of the graph-notes links buffer"
  :lighter " gn")


(let ((b-name (car (s-split "[.]" (buffer-name)))))
  (let ((buffer (generate-new-buffer (s-lex-format "in-links<${b-name}>"))))
	(shell-command (s-lex-format "grep -nwrH -C1 \"${b-name}\" .") buffer)
	(pop-to-buffer buffer)))
