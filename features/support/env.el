(require 'f)

(defvar graph-notes-support-path
  (f-dirname load-file-name))

(defvar graph-notes-features-path
  (f-parent graph-notes-support-path))

(defvar graph-notes-root-path
  (f-parent graph-notes-features-path))

(add-to-list 'load-path graph-notes-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'graph-notes)
  (require 'espuds)
  (require 'ert))

(Setup
 ;; Before anything has been run
 (unless (f-exists? "./test_base_directory/")
   (f-mkdir "./test_base_directory/"))
 (setq graph-notes-base-directory "./test_base_directory"))

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 (setq graph-notes-file-extension graph-notes--default-file-extension)
 (setq graph-notes--all-tags '())
 (unless (not (f-exists? "./test_base_directory/"))
   (mapc
	(lambda (file) (f-delete (f-full file)))
	(f-files "./test_base_directory/"))))

(Teardown
 ;; After when everything has been run
 (unless (not (f-exists? "./test_base_directory/"))
   (f-delete "./test_base_directory/" t))
 (setq graph-notes-base-directory nil))
