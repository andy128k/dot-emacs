;; interoperability of clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(global-set-key [C-f4]
                (lambda ()
                  (interactive)
                  (kill-buffer (current-buffer))))

(global-set-key [C-f9] 
                (lambda ()
                  (interactive)
                  (if defining-kbd-macro
                      (end-kbd-macro)
                    (start-kbd-macro nil))))
(global-set-key [f9] 'call-last-kbd-macro)

;; file-name-directory
(setq dot-emacs-basedir (concat (file-name-directory (file-truename "~/.emacs.el")) "third-party/"))

(add-to-list 'load-path dot-emacs-basedir)

(global-linum-mode 1)

;; подсветка всех окружающих скобок
(require 'highlight-parentheses)
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; remove splash
(setq inhibit-startup-message t)
;; remove toolbar
(tool-bar-mode -1)
;; remove menu
(menu-bar-mode -1)
;; remove scrolls
(scroll-bar-mode -1)

;; Использовать окружение UTF-8
(set-language-environment 'UTF-8)
;; UTF-8 для вывода на экран
(set-terminal-coding-system 'utf-8)
;; UTF-8 для ввода с клавиатуры
(set-keyboard-coding-system 'utf-8)

(setq default-input-method 'russian-computer)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; scrolling
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position 't)

(global-font-lock-mode 1)

;; показ текущей колонки
(column-number-mode 1)

;; color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-charcoal-black)

;; expand frame
(when window-system
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

;;; slime
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

;;
;; tabbar
;;
(when (require 'tabbar nil t)
  (defun is-emacs-buffer (name)
    (or
     (string= (substring name 0 2) " *")
     (string= (substring name 0 1) "*")))

  (setq tabbar-buffer-groups-function
	(lambda (buffer)
	  (with-current-buffer (get-buffer buffer)
	    (cond
	     ((is-emacs-buffer (buffer-name))
	      '("Emacs Buffers"))
	     (t
	      '("User Buffers"))))))
  (tabbar-mode)

  (global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
  (global-set-key (kbd "<C-S-iso-lefttab>") 'tabbar-backward-tab)

  (set-face-attribute 'tabbar-default-face nil
		      :height 1.5
		      :background "#B8B8B8")

  (set-face-attribute 'tabbar-button-face nil
		      :height 1.5
		      :box '(:line-width 4 :color "#B8B8B8" :style nil))
  
  (set-face-attribute 'tabbar-selected-face nil
		      :foreground "white" :background "#333333"
		      :height 1.5
		      :font "DejaVu Sans" :weight 'semi-bold
		      :box nil)
  
  (set-face-attribute 'tabbar-unselected-face nil
		      :foreground "black" :background "#EEEEEE"
		      :height 1.5
		      :font "DejaVu Sans"
		      :box '(:line-width 1 :color "#333333" :style nil)))

(setq enable-local-variables :all)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;
;; popwin
;;
(add-to-list 'load-path (concat dot-emacs-basedir "popwin/"))
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

