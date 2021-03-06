;;; ~/.config/doom/autoload/esstorm.el -*- lexical-binding: t; -*-

;;;###autoload
(defun find-in-dotfiles ()
  "Open a file somewhere in ~/.dotfiles via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/.dotfiles")))

;;;###autoload
(defun find-in-orgnotes ()
  "Open a file somewhere in ~/notes/org via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/org")))

;;;###autoload
(defun browse-dotfiles ()
  "Browse the files in ~/.dotfiles."
  (interactive)
  (doom-project-browse (expand-file-name "~/.dotfiles")))

;;;###autoload
(defun browse-orgnotes ()
  "Browse the files in ~/notes/org."
  (interactive)
  (doom-project-browse (expand-file-name "~/org")))
