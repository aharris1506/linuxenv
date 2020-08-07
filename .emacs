;;
;; Any key path setup first
;;

;; make sure correct cc-mode is found
(add-to-list 'load-path "/home/aharris/.emacs.d/lisp/latest-cc-mode")
(add-to-list 'load-path "/home/aharris/.emacs.d/lisp")

;;
;; keep garbage collection overhead under control
;; https://github.com/velkyel/dotfiles/blob/bb90dd2551bbb8b45f9560c2cba2d32256728a4b/.emacs
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
;; Helpful key combos
;;
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cr" 'revert-buffer)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cw" 'what-line)
(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "C-\\") 'fill-paragraph)

;;
;; sucks when I fat-finger C-x C-c...
;;
(setq confirm-kill-emacs 'y-or-n-p)

;;
;; Some meta-key fixes
;;
(setq-default x-super-keysym 'meta)

;;
;; keep whitespace under control
;;
(add-hook 'before-save-hook 'whitespace-cleanup)

;;
;; configure some UI items
;;
(menu-bar-mode -1)
(column-number-mode t)
(size-indication-mode t)
(ido-mode t)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1))
(setq frame-title-format
      '(buffer-file-name "%f"
                         (dired-directory dired-directory "%b")))
(setq inhibit-startup-message t
      initial-scratch-message nil)
(global-auto-revert-mode t)

;;
;; initialize package manager
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))
(require 'req-package)

;;
;; Magit for GIT
;;
;;(use-package magit :ensure t)
;;(global-set-key "\C-xg" 'magit-status)

;;
;; cmake
;;
(req-package cmake-mode :ensure t)

;;
;; slime
;;
;;(setq inferior-lisp-program "/usr/bin/sbcl")
;;(use-package slime :ensure t)
;; (use-package slime-company :ensure t)
;; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;; (slime-setup '(slime-fancy slime-company))

;;
;; markdown mode
;;
(req-package markdown-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;
;; yaml
;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))


;;
;; enable 'modern' c++ font-lock and new cc-mode for C++11,14,17
;;
;; 'modern' c++ font lock:
;;   https://github.com/ludwigpacifici/modern-cpp-font-lock
;;
(req-package modern-cpp-font-lock :ensure t)
(modern-c++-font-lock-global-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-noise-macro-names (quote ("constexpr")))
 '(custom-enabled-themes (quote (wombat)))
 '(elpy-syntax-check-command "pylint")
 '(package-selected-packages
   (quote
    (yaml-mode elpy pylint flycheck modern-cpp-font-lock cmake-mode markdown-mode flycheck-rtags company-rtags)))
 '(rtags-enable-unsaved-reparsing t))

;;
;; rtags stuff
;; https://github.com/Andersbakken/rtags/wiki/Usage
;;
(require 'init-rtags) ;; in .emacs.d/lisp
(use-package company :ensure t)
(push 'company-rtags company-backends)
(global-company-mode)
(define-key c-mode-base-map (kbd "C-/")
  (function company-complete))
;; company mode is not super helpful in gdb
(add-hook 'gdb-mode-hook 'my-gdb-hook)
(defun my-gdb-hook ()
  (company-mode -1))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq-default c-basic-offset 2
              indent-tabs-mode nil)


(add-to-list 'auto-mode-alist '("\\.iss\\'" . asm-mode))

;;
;; GDB
;;
(setq-default gdb-many-windows nil)

;;
;; elpy
;;
(req-package blacken :ensure t)
(req-package elpy :ensure t :init (elpy-enable))
(setq elpy-rpc-virtualenv-path 'current)
(add-hook 'elpy-mode-hook 'flycheck-mode)
(add-hook 'elpy-mode-hook (lambda ()
                            (highlight-indentation-mode -1)
                            (add-hook 'before-save-hook
                                      'elpy-black-fix-code nil t)))
(add-hook 'python-mode-hook '(lambda () (elpy-mode)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
