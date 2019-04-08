;; A lot of the inspiration for this .emacs file comes from here:

;; https://github.com/velkyel/dotfiles/blob/bb90dd2551bbb8b45f9560c2cba2d32256728a4b/.emacs

;;
;; keep garbage collection overhead under control
;;
(setq my-gc-threshold (* 64 1024 1024))

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold my-gc-threshold)))

(add-hook 'minibuffer-setup-hook
          (lambda () (setq gc-cons-threshold most-positive-fixnum)))

(add-hook 'minibuffer-exit-hook
          (lambda () (setq gc-cons-threshold my-gc-threshold)))

;;
;; initialize package manager
;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;;
;; Helpful key combos
;;
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cr" 'revert-buffer)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cw" 'what-line)
(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "C-\\") 'fill-paragraph)
(require 'magit)
(global-set-key "\C-xg" 'magit-status)

;;
;; cmake
;;
;;(setq load-path (cons (expand-file-name "/dir/with/cmake-mode") load-path))
(require 'cmake-mode)

;;
;; slime
;;
(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(slime-setup '(slime-fancy slime-company))

;;
;; markdown mode
;;
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;
;; configure some UI items
;;
(menu-bar-mode -1)
(column-number-mode t)
(size-indication-mode t)
(ido-mode t)

;;(defun setup-my-fringe ()
;;  (fringe-mode '(8 . 0)))

(when (display-graphic-p)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1))

(setq frame-title-format
      '(buffer-file-name "%f"
                         (dired-directory dired-directory "%b")))
;;  (add-hook 'window-setup-hook 'setup-my-fringe)
;;  (add-hook 'after-make-frame-functions 'setup-my-fringe))

(setq inhibit-startup-message t
      initial-scratch-message nil)

;;
;; GDB
;;
(setq-default gdb-many-windows nil)

;;
;; rtags stuff
;; https://github.com/Andersbakken/rtags/wiki/Usage
;;

(require 'rtags)
(require 'company)
(global-company-mode)
;; company mode is not super helpful in gdb
(add-hook 'gdb-mode-hook 'my-gdb-hook)
(defun my-gdb-hook ()
  (company-mode -1))
;; use rtags flycheck mode -- clang warnings shown inline
(require 'flycheck)
(require 'flycheck-rtags)
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++17")))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq-default c-basic-offset 2
	      indent-tabs-mode nil)

(defun setup-flycheck-rtags()
  "Configure flycheck-rtags for better experience."
  (flycheck-select-checker 'rtags)
  ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil)
  (rtags-set-periodic-reparse-timeout 2.0)
  (setq-local flycheck-highlighting-mode nil))
;; (setq-default flycheck-display-errors-delay 0.3
;; 	      flycheck-check-syntax-automatically '(mode-enabled idle-change))
 
;; start rtags (if needed)
(rtags-start-process-unless-running)

(define-key c-mode-base-map (kbd "M-.")
  (function rtags-find-symbol-at-point))
(define-key c-mode-base-map (kbd "M-,")
  (function rtags-find-references-at-point))
(define-key c-mode-base-map (kbd "M-[")
  (function rtags-location-stack-back))
(define-key c-mode-base-map (kbd "M-]")
  (function rtags-location-stack-forward))
(define-key c-mode-base-map (kbd "C-?")
  (function rtags-display-summary))

;; disable prelude's use of C-c r, as this is the rtags keyboard prefix
;;(define-key prelude-mode-map (kbd "C-c r") nil)
;; install standard rtags keybindings. Do M-. on the symbol below to
;; jump to definition and see the keybindings.
(rtags-enable-standard-keybindings)
;; comment this out if you don't have or don't use helm
;;(setq rtags-use-helm t)
;; company completion setup
(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(rtags-set-periodic-reparse-timeout 1)
;;(setq rtags-enable-unsaved-reparsing t)
(setq rtags-completions-enabled t)
(push 'company-rtags company-backends)
(define-key c-mode-base-map (kbd "C-/") (function company-complete))
  ;; c-mode-common-hook is also called by c++-mode
(add-hook 'c-mode-common-hook #'setup-flycheck-rtags)
(add-hook 'c++-mode-common-hook #'setup-flycheck-rtags)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(package-selected-packages
   (quote
    (slime-company slime cmake-mode magit markdown-mode flycheck-rtags company-rtags)))
 '(rtags-enable-unsaved-reparsing t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
