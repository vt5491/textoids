;; my elisp directories
(defvar elisp-path '("~/vtstuff/elisp/" ))
;;(add-to-list 'load-path "c:/vtstuff/github/vt5491/elisp/vt-dev-color-theme-buffer-local")
;;(add-to-list 'load-path "c:/vtstuff/github/vt5491/elisp/vt-dev-color-theme-buffer-local-noflet")
(add-to-list 'load-path "~/vtstuff/elisp")
(add-to-list 'load-path "~/vtstuff/elisp/vt-plugins/vt-dev-custom")
(require 'vt-dev-custom)
;; for some reason auto-loading doesn't work on my module, so load explicitly
;;(load "vt-dev-load-theme-buffer-local")
;;(load "vt-functions")
;;(autoload 'vt-functions "vt-functions" t)
(require 'vt-functions)
;;make it so you can have a different color-them in each frame
(setq color-theme-is-global nil)
;; (fset 'vt-insert-sandwich
;;    [?\C-o ?# ?v ?t ?  ?a ?d ?d return ?# ?v ?t ?  ?e ?n ?d ?\C-o])
;; ;;vt map the vt-insert-sandwich to c-c v
;; (global-set-key (kbd "C-c v") 'vt-insert-sandwich)
;; make C-c v <letter> a whole class of my special stuff
;;(global-set-key (kbd "C-c v r") (lambda () (interactive) (insert "%R ")))
(global-set-key (kbd "C-c v r") (lambda () (interactive) (revert-buffer nil t)))
;;this s will force spaces instead of tabs
(setq-default tab-width 4 indent-tabs-mode nil)
;;turn off menu icons at top row
(tool-bar-mode 0)
;; only scroll 1 line when at bottom of the screen
(setq scroll-step 1)
;; add..make M-g be a goto line
(global-set-key (kbd "M-g") 'goto-line)
;; add 2010-10-27 to make lines indent automatically
;; see http://stackoverflow.com/questions/344966/sane-tab-in-emacs
(global-set-key (kbd "RET") 'newline-and-indent)
;; add so matching braces are highlighted
(show-paren-mode 1)
;; turn off blinking cursor
(blink-cursor-mode)

;; 2015-02-23
;; start maximized (not quite "fullscreen", which hides gnome bar at the bottom)
;; actually, this starts "full" fullscreen
;;(toggle-frame-maximized)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(auto-save-interval 600)
 '(blink-cursor-mode nil)
 '(ecb-methods-menu-sorter nil)
 '(ecb-tip-of-the-day nil)
 '(ispell-dictionary "american")
 '(ispell-program-name "c:\\Program Files (x86)\\Aspell\\bin\\aspell.exe")
 '(org-src-fontify-natively t)
;; '(quote (ecb-options-version "2.40") t)
 '(scroll-conservatively 80)
 '(show-paren-mode t)
 '(sql-mysql-options (quote ("-A")) t))
;;vt-win
;; the following are new things I added to the windows emacs
;;vt add on 2012-02-06. Change default frame size
;;(add-to-list 'default-frame-alist '(height . 41))
;;(add-to-list 'default-frame-alist '(width . 146))
;;(add-to-list 'default-frame-alist '(fullwidth . fullheight))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;vt map c-w into backward-kill-region per Steve Yegge blog at
;;https://sites.google.com/site/steveyegge2/effective-emacs
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;;vt set c-x c-m for alt-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key (kbd "C-x m") 'execute-extended-command)

;; 2016-03-09. global hot key for toggling smartparens on/off.
;; We can't do it as a smart-parens-mode key because the mapping won't be in
;; effect when we disable smartparens
(global-set-key (kbd "C-c v p") 'smartparens-mode)
;; getting tired of typing 'load-theme-buffer-local too
(global-set-key (kbd "C-c v l") 'load-theme-buffer-local)

;;vt all qrr for query-replace-regexp
(defalias 'qrr 'query-replace-regexp)

;;vt 2012-02-15 attempt at vim o
;; http://stackoverflow.com/questions/2173324/emacs-equivalents-of-vims-dd-o-o
(defadvice open-line (around vi-style-open-line activate)
 "Make open-line behave more like vi."
 (beginning-of-line)
 ad-do-it
 (indent-according-to-mode))

;;vt 2012-02-20
;; define a back-window function and bind it to c-x p
(defun back-window ()
  (interactive)
  (other-window -1))

(global-set-key "\C-xp" 'back-window)
;; and bind c-x n to other-window as a dup of c-x-o
(global-set-key "\C-xn" 'other-window)

;;vt 2012-04-18 stab at creating a 'search from top'
(defun vt-top-search ()
  (interactive)
  (beginning-of-buffer)
  (isearch-forward))

(global-set-key (kbd "C-c t") 'vt-top-search)

;;vt 2012-07-16 comment out a line with #vt at the beginning
;; this version will leave cursor at beginning of line
;; with a #vt
;;(fset 'vt-comment-out
;;   "\355#vt")
;; just a #
(fset 'vt-comment-out
   "\355#")

;;vt map the vt-comment-out to c-c #
(global-set-key (kbd "C-c #") 'vt-comment-out)

;;vt 2012-11-27 limit shell buffer sizes to 20k lines, to prevent memory gobbling on ubuntu
;; Note: I was able to intially set this with c-x c-e.  But after that, the only way I could get it
;; to change was to run 'eval-buffer' on the entire .emacs
(setq comint-buffer-maximum-size 20000)
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)
;; set up mysql for emacs 24
(setq sql-product 'mysql)
;;vt 2014-09-02 add font-size increase/decrease hot key for when working on laptop
;; vt 2015-02-25  just rely on defaults of c-x c -, and c-x c + (that's ctrl-x ctrl <minus or plus key>)
;;(global-set-key "\C-c+" 'text-scale-increase)
;;(global-set-key "\C-c-" 'text-scale-decrease)
;; overlay selected text when pasting
(delete-selection-mode 1)

;; turn off the annoying audio bell
(setq ring-bell-function 'ignore)
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))
;; we need this to make the changes to load-theme-buffer-local work
;; I don't know why these two don't autoload
;;(require 'noflet)
;; Note: do not need the require on 'noflet here, since the require for it in 'vt-dev-load-theme-buffer-local-noflet)
;; takes care of it
;;(require 'vt-dev-load-theme-buffer-local-noflet)

;; make the default font size 20% bigger on every buffer
;; note: this is overkill given the global scale up on ubuntu images
;;(set-face-attribute 'default nil :height 120)

;; this command will stop annoying "^M" when printing on windows
(defun vt-remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(add-hook 'text-mode-hook 'vt-remove-dos-eol)

;;;; enable paredit-mode in al list modes
;;(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;;(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;;(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;;(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;;(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;;(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;;(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;;(add-hook 'clojure-mode-hook          #'enable-paredit-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(vt-face-black-\#000000 ((t (:background "#000000" :foreground "#000000"))) t)
 '(vt-face-blue-\#0000ff ((t (:background "#0000ff" :foreground "#0000ff"))) t)
 '(vt-face-green-\#00ff00 ((t (:background "#00ff00" :foreground "#00ff00"))) t)
 '(vt-face-grey-\#7f7f7f ((t (:background "#7f7f7f" :foreground "#7f7f7f"))) t)
 '(vt-face-red-\#ff0000 ((t (:background "#ff0000" :foreground "#ff0000"))) t)
 '(vt-face-white-\#ffffff ((t (:background "#ffffff" :foreground "#ffffff"))) t)
 '(vt-face-yellow-\#ffff00 ((t (:background "#ffff00" :foreground "#ffff00"))) t))

;; 2014-10-20 enable auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)

;; 2014-09-12 set up R-related stuff
;; bind slurp to ctrl-<left-arrow> key:
;;(define-key sp-keymap "\M-[1;5C" 'sp-forward-slurp-sexp)
(autoload 'R-mode "ess-site.el" "" t)
(add-to-list 'auto-mode-alist '("\\.R\\'" . R-mode))

;; turn off annoying underscore completion in R-mode
;;(ess-toggle-underscore nil)
;; use paredit key-bindings if smartparens is enabled
;; 2015-02-23: smartparens key bindings are "evil" because they override C-r (and you can't override)
;; and thus you lose some good key-binding in cider-repl
;;(add-hook 'smartparens-enabled-hook   #'sp-use-paredit-bindings)

;; 2015-02-23
;; override smartparens M-r keybinding, as we need it in cider
;;(define-key smartparens-mode-map (kbd "your-key") 'function)
;; when coming into r-mode, enable smartparents mode
(add-hook 'R-mode-hook                #'smartparens-mode)
;;(add-hook 'R-mode-hook                #'ess-toggle-underscore nil)
(add-hook 'R-mode-hook '(lambda () (ess-toggle-underscore nil)))

;; 2014-11-03. Setup ipython mode. use m-x run-python to lauch
;; note: specify:
;; (setq python-shell-interpreter-args "console --existing 86b2f95b-c3a7-40d0-8079-928dd26107a8")
;; for example, to hook up to an existing notebook kernel
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; jedi is a package that gives you popup python auto-completion and help
;; http://tkf.github.io/emacs-jedi/latest/
;; Note: you have to run "M-x jedi:install-server" for this to work
;; Note2: you don't have to run this each time. e.g since I've already
;; run it, you shouldn't to run ever again, unless you want to refresh the server with
;; the latest code.
(add-hook 'python-mode-hook 'jedi:setup)

;; The melpa version of "ein" doesn't work, so I have to use the one from github
;; instead: https://github.com/millejoh/emacs-ipython-notebook
;; see github post https://github.com/tkf/emacs-ipython-notebook/issues/154 for more info
;; Note: this overrides the MELPA version that is on the path.  Maybe, on this next
;; melpa package update for ein you can remove this override
(add-to-list 'load-path "~/vtstuff/elisp/packages/emacs-ipython-notebook/lisp")

;; 2015-03-03
;; add clomacs.  Melpa package is not available
(add-to-list 'load-path "~/vtstuff/elisp/packages/clomacs/src/elisp/")
;;(require 'clomacs)
(autoload 'clomacs "clomacs.el" "vt: clj to emac interop" t)

;; 2015-03-11
;; add elisp-format.  Melpa package not available
(add-to-list 'load-path "~/vtstuff/elisp/packages/elisp-format/")
(require 'elisp-format)
;;(autoload 'elisp-format "elisp-format.el" "vt: elisp formatter" t)

;; 2015-03-27 add clojurescritp babel mode
(add-to-list 'load-path "/home/vturner/vtstuff/elisp/packages/babel-clojurescript")
;;(autoload 'ob-clojurescript "ob-clojurescript.el" "vt: org-mode cljs babel interface" t)
(require 'ob-clojurescript)

;; hook ein to use jedi completion.  Don't seem to get popups though.
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
(setq jedi:complete-on-dot t)

;; 2014-12-23
;; add in more ruby support
;; 2015-03-09 comment out for now, since I'm not currently using ruby
;; and this delays startup
;; (require 'rvm)
;; (rvm-use-default) ;; use rvm's default ruby for the current Emacs session

;; 2015-01-26
;; add in improved buffer handling
(defalias 'list-buffers 'ibuffer)
;;(ido-mode)

;; 2015-01-27
;; add paren mode
;;(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;;(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook       #'smartparens-strict-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;;(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;;(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'smartparens-strict-mode)
(add-hook 'lisp-mode-hook             #'smartparens-strict-mode)
(add-hook 'lisp-interaction-mode-hook #'smartparens-strict-mode)
(add-hook 'scheme-mode-hook           #'smartparens-strict-mode)
;;(add-hook 'clojure-mode-hook          #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'smartparens-strict-mode)
;;(add-hook 'org-mode-hook              #'enable-paredit-mode)
(add-hook 'org-mode-hook              #'smartparens-strict-mode)
(add-hook 'js2-mode-hook              #'smartparens-strict-mode)

;; 2015-01-28
;; add some better bindings for paredit slurp and barf (when running in lisp mode)
(defun vt-lisp-mode-keys ()
  "Modify keymaps used by `lisp-mode'."
  (local-set-key (kbd "C-c l") 'sp-forward-slurp-sexp)
  (local-set-key (kbd "C-c b") 'sp-forward-barf-sexp))

;; add to emacs-lisp-mode-hook and org-mode
(add-hook 'emacs-lisp-mode-hook       'vt-lisp-mode-keys)
(add-hook 'org-mode-hook              'vt-lisp-mode-keys)
(add-hook 'clojure-mode-hook          'vt-lisp-mode-keys)
(add-hook 'js2-mode-hook              'vt-lisp-mode-keys)

;; org-mode stuff
;; don't prompt if running code from org mode
(setq org-confirm-babel-evaluate nil)
(setq org-src-ask-before-returning-to-edit-buffer nil)

;; set up a local hot key to auto-insert code blocks
;;(setq vt-org-mode-src-lang-token "emacs-lisp")
;;(setq vt-org-mode-src-lang-token "clojure")
(setq vt-org-mode-src-lang-token "js")

(defun vt-insert-org-src-block ()
  "allows you to enter the markup for org src blocks"
  (interactive)
  (move-beginning-of-line 1)
  ;;(insert "#+BEGIN_SRC emacs-lisp :tangle yes\n\n")
  ;;(insert "#+BEGIN_SRC clojure :tangle yes\n\n")
  ;;(insert "#+BEGIN_SRC " vt-org-mode-src-lang-token " :tangle yes\n\n")
  (insert "#+BEGIN_SRC " vt-org-mode-src-lang-token " \n\n")
  (insert "#+END_SRC")
  (forward-line -1))

;; org-mode collpase macro
;; (fset 'vt-org-collapse
;;    [?\C-c ?\C-p tab ?\C-l])

;; Note: the previous macro doesn't handle the edge case where we are on the
;; header itself.  Thus we need to resort to api level
(defun vt-org-collapse-and-center ()
  "collapse the current heading space and center heading in center" 
  (interactive) 
  (when (not (org-at-heading-p)) 
    (outline-previous-visible-heading 1)) 
  (org-cycle) 
  (with-selected-window (get-buffer-window (current-buffer)) 
    (recenter-top-bottom)))


(defun vt-org-mode-keys ()
  "add/modify keymaps used by org-mode"
  (local-set-key (kbd "C-c v s") 'vt-insert-org-src-block)
  (local-set-key (kbd "C-x C-e") 'cider-eval-last-sexp)
  (local-set-key (kbd "C-c v c") 'vt-org-collapse-and-center))

;; set it up eveytime we run org mode
(add-hook 'org-mode-hook       'vt-org-mode-keys)
;; enable autocomplet as well
(add-hook 'org-mode-hook       'auto-complete-mode)
;; visual-line-mode will wrap text at screen end
(add-hook 'org-mode-hook       'visual-line-mode)

;; this allows you to collapse local subsection while you're in the middle of it.
;; see http://stackoverflow.com/questions/12737317/collapsing-the-current-outline-in-emacs-org-mode
(setq org-cycle-emulate-tab 'white)

;; 2015-03-07
;; enable org mode and clojure
;;(require 'org)
(require 'ob-clojure)
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
;;(setq org-babel-clojure-backend 'cider)
(setq org-babel-clojure-backend 'skewer)
;;(require 'cider)
;; note: just run M-x slime if you want to invoke the clisp common list REPL
;;(setq inferior-lisp-program "/usr/bin/clisp")
(setq inferior-lisp-program "/usr/bin/clojure")
;; 2015-02-02 clojure stuff.  Mostly copied from ubuntu-laptop image
;; don't show a full stack trace upon cider (clojure) errors
(setq cider-show-error-buffer nil)
;; 2015-02-25 cider stuff, as recommended under the github for cider (https://github.com/clojure-emacs/cider)
(add-hook 'cider-mode-hook #'eldoc-mode)
;; log communiction with the nrepl server (goto *nrepl-messages*)
;;(setq nrepl-log-messages t)
;; hide nrepl housekeeping buffers from showing when doing c-x b
(setq nrepl-hide-special-buffers t)
;; use company-mode instead of ac for cider
(global-company-mode)
;; enable rainbow paren highlighting
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
;; smartparens overrides M-r in all modes.  We really want it back in cider mode
;; Note:  Since We want our hook to run after smart-parens,we have to define it first.
;; note: you cannot override smartparens key bindings easily
;; see http://stackoverflow.com/questions/16605571/why-cant-i-change-paredit-keybindings
;; Note: I don't have this problem anymore now that I'm using smartparens keybindings.
;;(defun vt-cider-repl-mode-keys ()
;;  "Modify keymaps used by `cider-repl-mode'."
;;  (local-set-key (kbd "C-c M-r") 'cider-repl-previous-matching-input)
;;  )
;;(add-hook 'cider-repl-mode-hook          'vt-cider-repl-mode-keys)

;; enable smart-parens
(add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
;;(add-hook 'cider-repl-mode-hook          #'enable-paredit-mode)
;; end cider

;; 2015-02-02 enable auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)

;;bind C-c s to search-forward
(global-set-key (kbd "C-c s") 'search-forward)

;; 2015-02-13
;; bind C-c z to the user supplied "run" command in the cljs repl
(defun vt-cljs-repl-mode-keys ()
  "set some additional key bindings for cljs repl mode"
  (interactive)
  (fset 'vt-cljs-run
     [?\( ?v ?t ?- ?d ?o ?i ?t ?\) return])
  (local-set-key (kbd "C-c z") 'vt-cljs-run))

;; 2015-02-02 add ac mode to cider REPL
;; 2015-02-23 comment out as part of migration to company-mode
;; (require 'ac-cider)
;; (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
;; (add-hook 'cider-mode-hook 'ac-cider-setup)
;; (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
;; (add-hook 'cider-repl-mode-hook 'vt-cljs-repl-mode-keys)

(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))


;; 2015-02-23.  re-bind C-A-x key bindings (that I'm using for workspace shifting) to C-s-x (super or windows key)
(require 'smartparens-config)
(define-key smartparens-mode-map (kbd "C-s-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-s-b") 'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "C-s-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-s-p") 'sp-previous-sexp)


;; 2015-03-03
;; add elisp .ert file (unit test) to list of files that should open in emacs-lisp-mode
(add-to-list 'auto-mode-alist '("\\.ert\\'" . emacs-lisp-mode))

;; 2015-03-09
;; disable it because I can't control which window the '*command-log* buffer shows in.
;; in short, it's not worth it.
;; autoload command logging if giving a demo.  Since we will almost never
;; use this, make it autoload
;; shouldn't have to autoload, since it's melpa... but I do for some reason.
;;(autoload 'mwe-log-commands "mwe-log-commands.el" "" t)
;; have to do a require.  Shouldn't have to , but do
;;(require 'mwe-log-commands)

;; 2015-03-16
;; set up some default buffer level color themes.
(add-hook 'cider-mode-hook (lambda nil
  (load-theme-buffer-local 'tango (cider-current-repl-buffer))))

;; 2015-03-17
;; add in elisp code to allow formatting of clojure code
;; These will define and run ok without a repl, but you need a repl
;; up to use them.
(require 'clomacs)
(clomacs-defun vt-elisp-fmt-wrapper vt-clj-fmt)

;; You need to have this definded on the clj side:
;; (defn vt-clj-fmt
;;   "format the passed code in a perltidy-like fashion.  Select a region of clj code in emacs then M-x vt-clj-fm"
;;   [s]
;;   (let [rgn (read-string s)]
;;     (with-out-str (clojure.pprint/write rgn :dispatch clojure.pprint/code-dispatch))))

(defun vt-clj-fmt-rgn (beg end)
  "elisp wrapper to clojure function 'vt-clj-fmt.  Select a clj region and this command will format it for you.
 Note: this function does not distinguish between naked newlines and newlines embedded in quotes (as part of a string in the function, for example).  Thus, any newlines in a string will be converted to actual newlines
"
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (when (and beg end)
    (let ((r (vt-elisp-fmt-wrapper
               (replace-regexp-in-string
                  (regexp-quote "\"")
                  (regexp-quote "\\\"")
                  (buffer-substring-no-properties beg end)))))
      (delete-active-region)
      (insert
         (replace-regexp-in-string
            (regexp-quote "\\\"")
            (regexp-quote "\"")
              (replace-regexp-in-string "\\\\n" "\n" r))))))

;; 2015-04-20
;; enable js2-mode for for js file instead of the default of 'js-mode
;;(autoload 'js2-mode "js2" nil t)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; 2015-06-04
;; change js default indent from 4 to 2.  This seems to be more of the "modern" norm
(setq js-indent-level 2)

;; '(js2-basic-offset 2)  
;; '(js2-bounce-indent-p t)

(add-hook 'js2-mode-hook (lambda ()
                           (setq
;;                                indent-tabs-mode t
;;                                 tab-width 2
;;                                 js-indent-level 2
;;                                 js2-basic-offset 2
;;                                 js2-bounce-indent-p t
                            js2-basic-offset 2
                                 )))
;; and enable skewer-mode
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;; allow eval-region in skewer mode per
;;http://stackoverflow.com/questions/20220090/undock-chrome-developer-tools
(defun skewer-eval-region (beg end)
  "Execute the region as JavaScript code in the attached browsers."
  (interactive "r")
  (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer))

;; setup some stuff for simple-http
;; Note: simple-httpd doesn't do well with nested heirarchies.  If your index.html
;; is not at the root level, you have to fully qualify paths relative to this dir.
;; This is not true if you're using apache as your server.  Simple solution is to
;; make httpd-root the same dir your index.html is in.
;; Do not need to cycle simple-httpd server if you change this (don't forget to run C-x C-e though)
;;(setq httpd-root "~/vtstuff/public-html")
(setq httpd-root "~/vtstuff/public-html/projects/VRSketch")

;; 2015-05-15 new and improved vt-sandwich
(defun vt-get-comment-token-for-buffer (buffer)
  "return the comment token associated with the curent buffer e.g
js2-mode = \"//\"

sample call:
(vt-get-comment-token-for-buffer (current-buffer))
(vt-get-comment-token-for-buffer (get-buffer  \"RiftSandbox.js\"))
"
  (let ((mode (buffer-local-value 'major-mode buffer))) 
      (cond
       ((equal mode 'lisp-interaction-mode) (make-string 2 ?\;))
       ((equal mode 'js2-mode) (make-string 2 ?/))
   )))

(defun vt-sandwich ()
  (interactive)
  (let ((comment-token (vt-get-comment-token-for-buffer (current-buffer))))
    (move-end-of-line 1)
    (newline-and-indent)
    (insert comment-token "vt add")
    (newline-and-indent)
    (newline-and-indent)
    (insert comment-token "vt end")
    (previous-line)
    (move-beginning-of-line 1)
  ))

(global-set-key (kbd "C-c v s") 'vt-sandwich)
