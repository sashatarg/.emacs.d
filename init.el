(package-initialize)

;;(defalias 'yes-or-no-p 'y-or-n-p)
;;(define-key global-map (kbd "C-z") #'undo)

(setq org-startup-indented t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq inhibit-startup-message t)

(defun package-safe-install (&rest packages)
    (dolist (package packages)
      (unless (package-installed-p package)
        (package-install package))
      (require package)))

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(package-safe-install 'magit)

(windmove-default-keybindings)
(put 'upcase-region 'disabled nil)

(package-safe-install 'helm)
(require 'helm-config)
(setq
   helm-ff-file-name-history-use-recentf t
   ;; do not display invisible candidates
   helm-quick-update t
   ;; open helm buffer inside current window, not occupy whole other window
   ;; this makes helm behave like a popup
   helm-split-window-in-side-p t
   ;; fuzzy matching buffer names when non--nil
   helm-buffers-fuzzy-matching t
   ;; move to end or beginning of source when reaching top or bottom of source.
   helm-move-to-line-cycle-in-source t)

(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x <tab>") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(defun helm-backspace ()
    "Forward to `backward-delete-char'.
  On error (read-only), quit without selecting."
    (interactive)
    (condition-case nil
        (backward-delete-char 1)
      (error
       (helm-keyboard-quit))))

(define-key helm-map (kbd "DEL") 'helm-backspace)

(package-safe-install 'elpy)
(recentf-mode)
(setq recentf-max-saved-items 500)

(load-theme 'leuven t)
(blink-cursor-mode -1)
(tool-bar-mode -1)

(package-safe-install 'undo-tree)

(define-key global-map (kbd "s-y") #'undo-tree-redo)
(define-key global-map (kbd "s-z") #'undo-tree-undo)

(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-replace-disputed-keys t)
(setq org-ellipsis " \u25bc")


;; projectile
;; ---
(package-safe-install 'projectile)
(projectile-global-mode)

;; make scrolling smoother
(progn
  ;; only start scrolling when cursor is very close to edge of screen
  (setq scroll-margin 1)
  ;; don't recenter the cursor
  (setq scroll-conservatively 1000)
  (setq-default scroll-up-aggressively 0.01
                scroll-down-aggressively 0.01)
  )

;; change smoothness of mouse scrolling
(progn
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed t))

;; have C-x k kill current buffer
(defun my/kill-this-buffer ()
  "Kill current buffer without prompt"
  (interactive)
  (kill-buffer (current-buffer)))
(bind-key "C-x k" #'my/kill-this-buffer)

;; Auto-added configurations

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:background "White" :foreground "#3C3C3C" :overline nil :weight bold :height 1.0))))
 '(org-level-2 ((t (:background "#E5F4FB" :foreground "#123555" :overline nil :weight normal :height 1.0)))))

