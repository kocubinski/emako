;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Matt Kocubinski"
      user-mail-address "mkocubinski@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
;; (setq doom-font "MesloLGS NF-13")
(setq doom-font (font-spec :family "MesloLGS NF" :size 14))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(doom-load-envvars-file "~/.emacs.d/.local/env")

(after! lsp-mode
  (setq lsp-log-max t))

(require 'auth-source)
(use-package grip-mode
  :init
  (progn
    (setq grip-binary-path (concat (getenv "HOME") "/.local/bin/grip"))
    ;; only render on save
    (setq grip-update-after-change nil)
    (let ((credential (auth-source-user-and-password "api.github.com")))
      (setq grip-github-user (car credential)
            grip-github-password (cadr credential)))))

(require 'flyspell)
(defun the-org-mode-hook ()
  (auto-fill-mode 1)
  (set-fill-column 110)
  (flyspell-mode)
  ;;(define-key org-mode-map (kbd "C-,") nil)
  ;;(define-key flyspell-mode-map (kbd "C-,") nil)
  )

(require 'ox-tufte)

(add-hook 'org-mode-hook #'the-org-mode-hook)

;; Perhaps for MacOs only?
(use-package flyspell
  :init
  (progn
    (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
    (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; org mode export
(setq org-publish-project-alist
      '(("blog"
         :base-directory "~/src/notebook/org/blog"
         :base-extension "org"
         :publishing-directory "~/src/notebook/org/public/blog/"
         :publishing-function org-html-publish-to-tufte-html
         :headline-level 3
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"tufte.css\"/><link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\"/>"
         :html-preamble t
         :html-postamble nil
         :html-link-up "index.html"
         :html-link-home "index.html"
         )))
