;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq
      ;; doom-scratch-initial-major-mode 'lisp-interaction-mode
      doom-theme 'doom-dracula

      ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
      ;; disable it by default.
      lsp-ui-sideline-enable nil
      lsp-enable-symbol-highlighting nil)

;;
;;; UI

;; "monospace" means use the system default. However, the default is usually two
;; points larger than I'd like, so I specify size 12 here.
(setq doom-font (font-spec :family "SF Mono" :size 18 :weight 'semi-light))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))


;;
;;; Keybinds

(map! :n [tab] (general-predicate-dispatch nil
                 (and (featurep! :editor fold)
                      (save-excursion (end-of-line) (invisible-p (point))))
                 #'+fold/toggle
                 (fboundp 'evil-jump-item)
                 #'evil-jump-item)
      :v [tab] (general-predicate-dispatch nil
                 (and (bound-and-true-p yas-minor-mode)
                      (or (eq evil-visual-selection 'line)
                          (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                 #'yas-insert-snippet
                 (fboundp 'evil-jump-item)
                 #'evil-jump-item)
      :leader
      "h L" #'global-keycast-mode
      "f t" #'find-in-dotfiles
      "f T" #'browse-dotfiles
      "f n" #'find-in-orgnotes
      "f N" #'browse-orgnotes)


;;
;;; Modules

;; I prefer search matching to be ordered; it's more precise
(add-to-list 'ivy-re-builders-alist '(counsel-projectile-find-file . ivy--regex-plus))

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Silence all that useless output
(setq direnv-always-show-summary nil)

;; Set plantuml path
(setq plantuml-jar-path "~/bin/plantuml.jar")
(setq org-plantuml-jar-path "~/bin/plantuml.jar")

;;; :tools magit
;(setq magit-repository-directories '(("~/projects" . 2))
      ;magit-save-repository-buffers nil
      ;;; Don't restore the wconf after quitting magit, it's jarring
      ;magit-inhibit-save-previous-winconf t
      ;transient-values '((magit-commit "--gpg-sign=5F6C0EA160557395")
                         ;(magit-rebase "--autosquash" "--gpg-sign=5F6C0EA160557395")
                         ;(magit-pull "--rebase" "--gpg-sign=5F6C0EA160557395")))

;; :lang org
(setq org-directory "~/org"
      ;; org-archive-location (concat org-directory ".archive/%s::")
      ;; org-roam-directory (concat org-directory "notes/")
      org-journal-encrypt-journal t
      org-journal-file-format "%Y%m%d.org"
      org-ellipsis " ▼ ")

(after! org
  (custom-set-faces!
    ;; '(outline-1 :weight extra-bold :height 1.25)
    ;; '(outline-2 :weight bold :height 1.35)
    ;; '(outline-3 :weight bold :height 1.12)
    ;; '(outline-4 :weight semi-bold :height 1.09)
    ;; '(outline-5 :weight semi-bold :height 1.06)
    ;; '(outline-6 :weight semi-bold :height 1.03)
    ;; '(outline-8 :weight semi-bold)
    ;; '(outline-9 :weight semi-bold)
    '(org-document-title :height 1.8)))

;; Patch up the evil-org key map, so that org is usable with daemon
;; https://github.com/hlissner/doom-emacs/issues/1897
(after! evil-org
  (evil-define-key '(normal visual) evil-org-mode-map
    (kbd "TAB") 'org-cycle))

;;;; Exports everything to ./exports
;; (defadvice org-export-output-file-name (before org-add-export-dir activate)
;;   "Modifies org-export to place exported files in a different directory"
;;   (when (not pub-dir)
;;       (setq pub-dir "exports")
;;       (when (not (file-directory-p pub-dir))
;;        (make-directory pub-dir))))

;; Exports to separate directories depending on file extension
(defvar org-export-output-directory-prefix "export_" "prefix of directory used for org-mode export")
    (defadvice org-export-output-file-name (before org-add-export-dir activate)
      "Modifies org-export to place exported files in a different directory"
      (when (not pub-dir)
          (setq pub-dir (concat org-export-output-directory-prefix (substring extension 1)))
          (when (not (file-directory-p pub-dir))
           (make-directory pub-dir))))

;; (after! org
;;   (add-to-list 'org-modules 'org-habit t))

;; (setq org-publish-project-alist
;;   '(("html"
;;      :base-directory "~/notes/org/"
;;      :base-extension "org"
;;      :publishing-directory "~/notes/org/exports"
;;      :publishing-function org-publish-org-to-html)
;;     ("pdf"
;;      :base-directory "~/notes/org/"
;;      :base-extension "org"
;;      :publishing-directory "~/notes/org/exports"
;;      :publishing-function org-publish-org-to-pdf)
;;     ("all" :components ("html" "pdf"))))

;;; :ui doom-dashboard
(setq fancy-splash-image (concat doom-private-dir "splash.png"))

;;; :ui modeline
;; (custom-set-faces!
;;   `(doom-modeline-bar-inactive :background ,(face-background 'mode-line-inactive)))

;; (use-package! keypression
;;   :defer t
;;   :config
;;   (setq ;;keypression-use-child-frame nil
;;         keypression-fade-out-delay 1.0
;;         keypression-frame-justify 'keypression-left-justified
;;         keypression-cast-command-name t
;;         keypression-cast-command-name-format "%s  %s"
;;         keypression-combine-same-keystrokes t
;;         keypression-font-face-attribute '(:width normal :height 200 :weight bold)))


;;
;;; Language customizations

(custom-set-faces!
  `(markdown-code-face :background ,(doom-darken 'bg 0.075)))
