;;
;; Any key path setup first
;;
(add-to-list 'load-path "/home/aharris/.emacs.d/lisp/cc-mode-5.33")

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

;; make sure correct cc-mode is found
(add-to-list 'load-path "/home/aharris/.emacs.d/lisp/cc-mode-5.33")

;;
;; initialize package manager
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))

;;
;; Magit for GIT
;;
(use-package magit :ensure t)
(global-set-key "\C-xg" 'magit-status)

;;
;; cmake
;;
(use-package cmake-mode :ensure t)

;;
;; slime
;;
(setq inferior-lisp-program "/usr/bin/sbcl")
(use-package slime :ensure t)
(use-package slime-company :ensure t)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(slime-setup '(slime-fancy slime-company))

;;
;; markdown mode
;;
(use-package markdown-mode :ensure t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;
;; GDB
;;
(setq-default gdb-many-windows nil)

;;
;; enable 'modern' c++ font-lock and new cc-mode for C++11,14,17
;;
;; This involves downloading and installing cc-mode 5.33:
;;   https://github.com/arximboldi/cc-mode
;; And 'modern' c++ font lock:
;;   https://github.com/ludwigpacifici/modern-cpp-font-lock
;;
(use-package modern-cpp-font-lock :ensure t)
(modern-c++-font-lock-global-mode t)
;; fix for constexpr C++17 bug
(custom-set-variables '(c-noise-macro-names '("constexpr")))

;;
;; rtags stuff
;; https://github.com/Andersbakken/rtags/wiki/Usage
;;
(use-package rtags :ensure t)
(setq-default rtags-path "/opt/bin")
(use-package company :ensure t)
(global-company-mode)
;; company mode is not super helpful in gdb
(add-hook 'gdb-mode-hook 'my-gdb-hook)
(defun my-gdb-hook ()
  (company-mode -1))
;; use rtags flycheck mode -- clang warnings shown inline
(use-package flycheck :ensure t)
(use-package flycheck-rtags :ensure t)
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

(define-key c-mode-base-map (kbd "M-/")
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
(setq rtags-completions-enabled t)
(push 'company-rtags company-backends)
(define-key c-mode-base-map (kbd "C-/") (function company-complete))
  ;; c-mode-common-hook is also called by c++-mode
(add-hook 'c-mode-common-hook #'setup-flycheck-rtags)
(add-hook 'c++-mode-common-hook #'setup-flycheck-rtags)

;; pylint: https://docs.pylint.org/en/1.6.0/ide-integration.html
;; Configure flymake for Python
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

;; Set as a minor mode for Python
(add-hook 'python-mode-hook '(lambda () (flymake-mode)))

;; Configure to wait a bit longer after edits before starting
(setq-default flymake-no-changes-timeout '3)

;; Keymaps to navigate to the errors
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-cn" 'flymake-goto-next-error)))
(add-hook 'python-mode-hook '(lambda () (define-key python-mode-map "\C-cp" 'flymake-goto-prev-error)))

;; To avoid having to mouse hover for the error message, these functions make flymake error messages
;; appear in the minibuffer
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (require 'cl)
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
      (let ((err (car (second elem))))
        (message "%s" (flymake-ler-text err)))))))

(add-hook 'post-command-hook 'show-fly-err-at-point)

;; Python flycheck
(custom-set-variables
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(package-selected-packages
   (quote
    (modern-cpp-font-lock yaml-mode slime-company slime cmake-mode magit markdown-mode flycheck-rtags company-rtags)))
 '(rtags-enable-unsaved-reparsing t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
