;; -*- lexical-binding: t; -*-

;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^A file-extension \"\\([^\"]+\\)\"$"
	   (lambda (extension)
		 (setq graph-notes-file-extension extension)))

(Given "^A tag \"\\([^\"]+\\)\"$"
	   (lambda (tag)
		 (graph-notes--create-new-tag (s-lex-format "${tag}"))))

(When "^I execute \"\\([^\"]+\\)\"$"
	  (lambda (command)
		(funcall (intern command))))

(When "^I execute \"\\([^\"]+\\)\" with arg \"\\([^\"]+\\)\"$"
	  (lambda (command arg)
		(funcall (intern command) (intern arg))))

(Then "^The file \"\\([^\"]+\\)\" should exist$"
	  (lambda (file)
		(cl-assert
		 (f-exists? (s-lex-format "${graph-notes-base-directory}/${file}")))))

(Then "^The file \"\\([^\"]+\\)\" should not exist$"
	  (lambda (file)
		(cl-assert (not (f-exists? file)))))

(Then "^The tag \"\\([^\"]+\\)\" should be in the tags-list$"
	  (lambda (tag)
		(cl-assert (member tag (graph-notes--list-all-tags)))))

(Then "^The tag \"\\([^\"]+\\)\" should not be in the tags-list$"
	  (lambda (tag)
		(cl-assert (not (member tag (graph-notes--list-all-tags))))))

(And "^I see the following text$"
  (lambda (text)
	(insert text)))

(Then "^I should see the following text$"
  (lambda (text)
    (cl-assert (s-equals? (buffer-string) text))))
