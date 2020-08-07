(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               ))
  (add-hook hook
            (lambda ()
              (require'rtags)
              ;;(require 'helm-config)
              ;;(require 'ac-rtags)
              (require 'flycheck)
              (require 'flycheck-rtags)

              ;; Run rtags daemon to provide tags request service.
              (rtags-start-process-unless-running)

              (rtags-enable-standard-keybindings)
              (setq rtags-autostart-diagnostics t)
              (rtags-diagnostics)
              (rtags-set-periodic-reparse-timeout 1)
              (setq rtags-completions-enabled t)
              ;; Just kill window and buffer, don't break stack position.
              ;;              (setq rtags-bury-buffer-function 'delete-current-buffer-and-window)

              ;; Split window force at below.
              ;;(setq rtags-split-window-function 'split-window-below)

              ;; Enable flycheck.
              (flycheck-mode)

              ;; Flycheck setup.
              (flycheck-select-checker 'rtags)
              (setq flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
              (setq flycheck-check-syntax-automatically nil)

              ;; If your project is build with cmake,
              ;; you need use command "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ." to generate file `compile_commands.json'
              ;; then use command "rc -J json_file_directory" to index C/C++ project tag index.
              ;;
              ;; If your project is build with qmake,
              ;; you need use command "bear --append make" to generate `compile_commands.json' file after command "qmake .."
              ;; then use command "rc -J json_file_directory" to index C/C++ project tag index.
              ;; (defun rtags-find-references-at-point+ ()
              ;;   (interactive)
              ;;   (rtags-find-references-at-point)
              ;;   ;; Switch window after poup rtag window.
              ;;   (other-window 1)
              ;;   )

              ;; (lazy-set-key
              ;;  '(
              ;;    ("C-8" . rtags-find-symbol-at-point)
              ;;    ("C-9" . rtags-find-references-at-point+)
              ;;    ("M-," . rtags-location-stack-back)
              ;;    ("M-." . rtags-location-stack-forward)
              ;;    ("M-'" . rtags-display-summary)
              ;;    ("C-." . rtags-rename-symbol)
              ;;    ("M-s-j" . flycheck-next-error)
              ;;    ("M-s-k" . flycheck-previous-error)
              ;;    )
              ;;  c-mode-base-map
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
              )
            ))

(provide 'init-rtags)
