;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(setq
 ;; doom-scratch-initial-major-mode 'lisp-interaction-mode
 doom-theme 'doom-dracula
 ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
 ;; disable it by default.
 lsp-ui-sideline-enable nil
 lsp-enable-symbol-highlighting nil)

;;; :ui doom-dashboard
(setq fancy-splash-image (concat doom-private-dir "splash.png"))

;;; :ui modeline
;; (custom-set-faces!
;;   `(doom-modeline-bar-inactive :background ,(face-background 'mode-line-inactive)))
;;
;; (setq doom-modeline-minor-modes (fboundp 'minions-mode))
;; (setq nyan-wavy-trail t nyan-minimum-window-width 96 nyan-bar-length 12)
;; (use-package nyan-mode
;;   :config
;;   (nyan-mode t)
;;   (nyan-animate-cat t))

(use-package nyan-mode
  :init
  (setq-default nyan-animate-nyancat t
                nyan-wavy-trail t)
  :config
  (nyan-mode t))

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 18 :weight 'semi-light))

(after! dap-mode
  (setq dap-python-debugger 'debugpy)
  (setq dap-auto-configure-features '(sessions locals controls tooltip)))
;; (setq dap-auto-configure-mode t)
;; (requires 'dap-cpptools)

(custom-set-faces!
  '(markdown-header-delimiter-face :foreground "#616161" :height 0.9)
  '(markdown-header-face-1 :height 1.8 :foreground "#A3BE8C" :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-2 :height 1.4 :foreground "#EBCB8B" :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-3 :height 1.2 :foreground "#D08770" :weight extra-bold :inherit markdown-header-face)
  '(markdown-header-face-4 :height 1.15 :foreground "#BF616A" :weight bold :inherit markdown-header-face)
  '(markdown-header-face-5 :height 1.1 :foreground "#b48ead" :weight bold :inherit markdown-header-face)
  '(markdown-header-face-6 :height 1.05 :foreground "#5e81ac" :weight semi-bold :inherit markdown-header-face))

(custom-set-faces!
  `(markdown-code-face :background ,(doom-darken 'bg 0.075)))

(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

(with-eval-after-load "ox-latex"
  (add-to-list 'org-latex-classes
               '("jdf2"
                 "\\documentclass{jdf2}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq key-chord-two-keys-delay 0.8)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq ob-async-no-async-languages-alist '("jupyter-python"))

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

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq direnv-always-show-summary nil)

(setq plantuml-jar-path "~/bin/plantuml.jar")
(setq org-plantuml-jar-path "~/bin/plantuml.jar")

;; (setq magit-repository-directories '(("~/projects" . 2))
;;     magit-save-repository-buffers nil
;;     magit-inhibit-save-previous-winconf t
;;     transient-values '((magit-commit "--gpg-sign=5F6C0EA160557395")
;;     (magit-rebase "--autosquash" "--gpg-sign=5F6C0EA160557395")
;;     (magit-pull "--rebase" "--gpg-sign=5F6C0EA160557395")))

(setq org-babel-tmux-terminal "alacritty")

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

(after! evil-org
  (evil-define-key '(normal visual) evil-org-mode-map
    (kbd "TAB") 'org-cycle))

(defvar org-export-output-directory-prefix "export_" "prefix of directory used for org-mode export")
(defadvice org-export-output-file-name (before org-add-export-dir activate)
  "Modifies org-export to place exported files in a different directory"
  (when (not pub-dir)
    (setq pub-dir (concat org-export-output-directory-prefix (substring extension 1)))
    (when (not (file-directory-p pub-dir))
      (make-directory pub-dir))))

;;; use :tangle-dir to specify which directory to tangle file to
(defun org-in-tangle-dir (sub-path)
  "Expand the SUB-PATH into the directory given by the tangle-dir
property if that property exists, else use the
`default-directory'."
  (expand-file-name sub-path
                    (or
                     (org-entry-get (point) "tangle-dir" 'inherit)
                     (default-directory))))

(setq org-html-checkbox-type 'html)

(after! org
  (setq org-agenda-files '("~/org/agenda.org")))

(setq
 ;; org-fancy-priorities-list '("[A]" "[B]" "[C]")
 ;; org-fancy-priorities-list '("❗" "[B]" "[C]")
 org-fancy-priorities-list '("🟥" "🟧" "🟨")
 org-priority-faces
 '((?A :foreground "#ff6c6b" :weight bold)
   (?B :foreground "#98be65" :weight bold)
   (?C :foreground "#c678dd" :weight bold))
 org-agenda-block-separator 8411)

(setq org-agenda-custom-commands
      '(("v" "A better agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (tags "PRIORITY=\"B\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Medium-priority unfinished tasks:")))
          (tags "PRIORITY=\"C\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Low-priority unfinished tasks:")))
          (agenda "")
          (alltodo "")))))

;; (after! org
;;   (add-to-list 'org-modules 'org-habit t))

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

(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.config/doom/snippets"))) ;; replace with your folder for snippets

(setq projectile-project-search-path '("~/Development/" "~/Code/"))

(after! projectile
  (setq projectile-enable-caching nil
        projectile-indexing-method 'alien))
