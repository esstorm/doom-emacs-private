;; -*- no-byte-compile: t; -*-
;;; .config/doom/packages.el

;;; Examples:
;; (package! kubel :recipe (:host github :repo "abrochard/kubel"))

;;; Org
(package! ob-restclient)
(package! org-beautify-theme)
(package! ob-tmux)
(package! org-superstar)
;; (package! org-fancy-priorities)
;; (package! org-super-agenda)
;; (package! org-ref)

;; (package! org-plot :recipe (:local-repo "lisp" :no-byte-compile t))
;; (package! robot-mode)

(package! key-chord)
(package! nyan-mode)

;;; Anki
;; (package! anki-connect)
;; (package! anki-editor)

;; C++
;; (package! company-tabnine)
;; (package! dap-mode)
;; (package! walkman :recipe (:host github :repo "abrochard/walkman"))

;; Fix all-the-icon mapping issues
(package! doom-modeline :pin "918730eff72e155cfb31b012493a7f425bc48ff8")
