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
  (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
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

  (global-set-key [C-up] 'enlarge-window)
  (global-set-key [C-down] 'shrink-window)
  (global-set-key [C-right] 'enlarge-window-horizontally) 
  (global-set-key [C-left] 'shrink-window-horizontally) )

(defun global-setting ()
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (show-paren-mode 1)
  (setf inhibit-startup-screen t)
  (setf column-number-mode t)
  (load-theme 'sanityinc-solarized-light t)
  (set-font-depend-on-os 14)
  (turn-on-paredit)
  (turn-on-helm-gtags)
  (turn-on-sr-speedbar)
  (set-global-key-binding)
  (setq backup-directory-alist `(("." .  "~/.emacs.d/emacs-backup"))))

(defun main ()
  (ensure-package-installed 
   '(color-theme-sanityinc-solarized
     paredit
     racket-mode
     helm
     helm-gtags
     sr-speedbar
     ))
  (global-setting))

(main)

