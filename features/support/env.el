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
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
