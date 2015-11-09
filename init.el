;; Add the user-contributed repositories
(require 'package)
(package-initialize)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; update package info
(when (not package-archive-contents)
  (package-refresh-contents))

;; install packages
(defun install-package-if-possible (pkg)
  (when (and (not (package-installed-p pkg))
	     (assoc pkg package-archive-contents))
    (package-install pkg)))

(dolist (pkg '(color-theme
	       highlight-parentheses
	       popwin
	       popup-switcher
	       ag
	       scala-mode2
	       js2-mode))
  (install-package-if-possible pkg))

(when (memq window-system '(mac ns))
  (install-package-if-possible 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; interoperability of clipboard
(setq x-select-enable-clipboard t)
(unless (string= system-type "darwin")
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; do not kill *scratch*
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
        ad-do-it)))

;; hotkeys
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

(defalias 'yes-or-no-p 'y-or-n-p)

;; osx key bindings
(when (eq system-type 'darwin)
  (global-set-key '[(kp-delete)] 'delete-char)
  (global-set-key (kbd "<home>") 'beginning-of-line)
  (global-set-key (kbd "<end>") 'end-of-line)
  (setq mac-command-modifier 'meta))

;; line numbers on gutter
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%4d ")
(set-face-attribute 'linum nil
		    :foreground "#ee9"
		    :background "#555")

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
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; remove menu
(menu-bar-mode -1)
;; remove scrolls
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; do not make backup files
(setq make-backup-files nil)

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

(setq enable-local-variables :all)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; tramp
(require 'tramp)
(setq password-cache-expiry nil)

;; color theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-charcoal-black)

;; expand frame
(when window-system
  (toggle-frame-maximized))

(defun find-files (root pattern)
  (mapcar (lambda (f)
            (expand-file-name f root))
          (directory-files root nil pattern)))

(defun first-existing (files)
  (catch 'file
    (dolist (f files)
      (when (file-exists-p f)
        (throw 'file f)))))

;;;
;;; slime
;;;
(let ((ql (expand-file-name "~/quicklisp/slime-helper.el")))
  (when (file-exists-p ql)
    (load ql)
    ;; Replace "sbcl" with the path to your implementation
    (setq inferior-lisp-program "sbcl")))

;;;
;;; closure-template
;;;
(let* ((quicklisp-dir "~/quicklisp")
       (default-dir (concat quicklisp-dir "/dists/quicklisp/software"))
       (dirs (cons
              (concat quicklisp-dir "/local-projects/cl-closure-template")
              (when (file-exists-p default-dir)
		(find-files  (concat default-dir "/") "cl-closure-template*"))))
       (files (mapcar (lambda (d)
                        (expand-file-name "closure-template-html-mode.el" d))
                      dirs))
       (file (first-existing files)))
  (when file
    (load file)
    (require 'closure-template-html-mode)))

;;
;; popwin
;;
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;; auto-complete
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)

;; switch buffers
(require 'popup-switcher)
(setq psw-in-window-center t)
(global-set-key (kbd "<C-tab>") 'psw-switch-buffer)

(setq default-directory "~/")

;; ag
(setq ag-reuse-buffers 't)
(setq ag-highlight-search t)
(global-set-key [f7] 'ag-project)
(global-set-key [C-f7] 'ag)

;; navigate between windows with Hyper-arrows
(windmove-default-keybindings 'hyper)

;; buffer names
(require 'uniquify)
(custom-set-variables
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

;; SPHP

(add-to-list 'auto-mode-alist
	     '("\\.sphp$" . (lambda ()
			      (common-lisp-mode)
			      (set (make-local-variable 'lisp-indent-function)
				   'common-lisp-indent-function)
			      (let ((keywords '(final-class font-lock-keyword-face (4 &body)
					        public      font-lock-keyword-face ((&whole 2 &rest ((1) &lambda &body)))
						protected   font-lock-keyword-face (2)
						private     font-lock-keyword-face (2))))
				(loop for k on keywords by 'cdddr
				      do
				      (put (car k) 'common-lisp-indent-function (caddr k))
				      (add-to-list 'font-lock-keywords
						   (cons (symbol-name (car k))
							 (cadr k))))))))


;; js
(setq js-indent-level 2)

;; js2
(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;
;; server
;;
;; alias e='emacsclient -n'
(server-start)

