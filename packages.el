;; -*- no-byte-compile: t; -*-
;;; .config/doom/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:host github :repo "username/repo"))
;; (package! builtin-package :disable t)
(package! kubel :recipe (:host github :repo "abrochard/kubel"))
(package! ob-restclient)
(package! org-beautify-theme)
;; (package! org-fancy-priorities)
;; (package! org-super-agenda)
(package! org-superstar)
(package! org-ref)
(package! ob-tmux)
;; (package! org-plot :recipe (:local-repo "lisp" :no-byte-compile t))
;; (package! robot-mode)
(package! jupyter)
(package! key-chord)
(package! nyan-mode)

;; Create Anki flashcards from Emacs
(package! anki-connect)
(package! anki-editor)

;; C++
(package! company-tabnine)
;; (package! dap-mode)
;; (package! walkman :recipe (:host github :repo "abrochard/walkman"))

;; Fix all-the-icon mapping issues
(package! doom-modeline :pin "918730eff72e155cfb31b012493a7f425bc48ff8")
