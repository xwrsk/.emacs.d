;; -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setf inferior-lisp-program "sbcl")


(defun ensure-package-installed (package-list)
  (package-initialize)
  (mapcar 
   (lambda (package) 
     (unless (package-installed-p package)
       (unless package-archive-contents
	 (package-refresh-contents))
       (package-install package)))
   package-list))


(require 'cl-lib)
(defun set-font-depend-on-os (font-size)
  (cl-labels ((select-first (sl lst)
			    (cl-dolist (s sl)
			      (when (member s (font-family-list))
				(cl-return s))))
	      (form-font (family font-size)
			 (cl-concatenate 'string family " "
					 (number-to-string font-size))))
    (let* ((perfer-font-family-list '("Droid Sans Mono"
				      "文泉驿等宽正黑"
				      "Microsoft Yahei"
				      "DejaVu Sans Mono"))
	   (default-font (form-font (select-first
				     perfer-font-family-list
				     (font-family-list))
				    font-size)))
      (print default-font)
      (add-to-list 'default-frame-alist `(font . ,default-font))
      ;; (set-default-font default-font)
      )))


(defun turn-on-paredit ()
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  (add-hook 'racket-mode-hook           #'enable-paredit-mode))



(defun turn-on-helm-gtags ()
  (setq
   helm-gtags-ignore-case t
   helm-gtags-auto-update t
   helm-gtags-use-input-at-cursor t
   helm-gtags-pulse-at-cursor t
   helm-gtags-prefix-key "\C-cg"
   helm-gtags-suggested-key-mapping t
   )

  (require 'helm-gtags)
  ;; Enable helm-gtags-mode
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)

  (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  ;; (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
  )
(defun turn-on-sr-speedbar ()
  (setf speedbar-show-unknown-files t)
  )

(defun set-global-key-binding ()
  (global-set-key (kbd "C-s-j")  'windmove-down)
  (global-set-key (kbd "C-s-k") 'windmove-up)
  (global-set-key (kbd "C-s-h") 'windmove-left)
  (global-set-key (kbd "C-s-l")  'windmove-right)

  (global-set-key (kbd "s-<up>") 'enlarge-window)
  (global-set-key (kbd "s-<down>") 'shrink-window)
  (global-set-key (kbd "s-<right>") 'enlarge-window-horizontally) 
  (global-set-key (kbd "s-<left>") 'shrink-window-horizontally) )


(defun magit-setting ()
  (setf magit-auto-revert-mode nil))


(defun toggle-term-mode ()
  (interactive)
  (if (equal major-mode 'term-mode)
      (text-mode)
    (progn
      (term-mode)
      (term-char-mode))
    ))

(defun multi-term-setting ()
  ;; (define-key term-mode-map (kbd "M-DEL") 'term-send-backward-kill-word)
  (add-hook
   'term-mode-hook
   (lambda ()
     (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
     (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))
     (add-to-list 'term-bind-key-alist '("M-DEL" . term-send-backward-kill-word))
     (add-to-list 'term-bind-key-alist '("M-d" . term-send-forward-kill-word))
     (add-to-list 'term-bind-key-alist '("M-`" . toggle-term-mode))))
  (define-key text-mode-map (kbd "M-`") 'toggle-term-mode)
  )

(defun c-mode-setting ()
  (add-hook 'c-mode-hook (lambda ()
			   (local-set-key (kbd "C-j") 'newline))))

(defun lisp-mode-symbol-prettity ()
  (defconst prettify-symbols-alist
    '(("lambda" . ?λ)
      ;; ("==" . ?≡)
      ))
  (add-hook 'lisp-mode-hook		'prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook       'prettify-symbols-mode)
  (add-hook 'lisp-mode-hook		'prettify-symbols-mode)
  (add-hook 'lisp-interaction-mode-hook 'prettify-symbols-mode)
  (add-hook 'scheme-mode-hook           'prettify-symbols-mode)
  (add-hook 'racket-mode-hook           'prettify-symbols-mode)
  )
(defun inferior-scheme-mode-setting ()
  (add-hook 'inferior-scheme-mode-hook 'prettify-symbols-mode)
  (add-hook 'inferior-scheme-mode-hook (lambda ()
					 (local-set-key (kbd "C-j") 'newline)))
  )

(defun global-setting ()
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (show-paren-mode 1)
  (setq ring-bell-function 'ignore)
  (setf inhibit-startup-screen t)
  (setf column-number-mode t)
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (load-theme 'sanityinc-solarized-light t)
  (set-font-depend-on-os 14)
  (turn-on-paredit)
  (turn-on-helm-gtags)
  (turn-on-sr-speedbar)
  (magit-setting)
  (multi-term-setting)
  (c-mode-setting)
  (lisp-mode-symbol-prettity)
  (inferior-scheme-mode-setting)
  (set-global-key-binding)
  (setf scheme-program-name "petite --libdirs /home/xuweirong/project/scheme-implementation/scheme-to-c:/home/xuweirong/project/scheme-implementation/nanopass-framework/")
  (setq backup-directory-alist `(("." .  "~/.emacs.d/emacs-backup"))))

(defun main ()
  (ensure-package-installed 
   '(color-theme-sanityinc-solarized
     paredit
     racket-mode
     helm
     helm-gtags
     sr-speedbar
     multiple-cursors
     magit
     highlight-symbol
     ))
  (global-setting))

(main)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-symbol-idle-delay 1.0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
