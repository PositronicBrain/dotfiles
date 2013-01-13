;;; init.el --- an emacs init file
;; Copyright (C) 2012 Federico Squartini
;; Maintainer federico.squartini@gmail.com

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; Wrapper to make .emacs self-compiling.
(defvar init-top-level t)
(if init-top-level

    (let ((init-top-level nil))
      (if (file-newer-than-file-p "~/.emacs.d/init.el" "~/.emacs.d/init.elc")
          (progn
            (load "~/.emacs.d/init.el")
            (byte-compile-file "~/.emacs.d/init.el")
            )
        (load "~/.emacs.d/init.elc")))
  (progn
    ;; Calculate load time
    (defvar *emacs-load-start* (current-time))

    ;; ~/.emacs.d/ contains all additional .el files

    (add-to-list 'load-path "~/.emacs.d/")

    ;; ============================
    ;; User info
    ;; ============================

    (setq user-mail-address "federico.squartini@gmail.com")
    (setq user-full-name "Federico Squartini")

    ;; ============================
    ;; Whitespace mode
    ;; ============================

    ;; display only tails of lines longer than 80 columns, tabs and
    ;; trailing whitespaces
    (setq whitespace-line-column 80
          whitespace-style '(face tabs trailing lines-tail))

    (global-whitespace-mode t)

    ;; ============================
    ;; Dired stuff
    ;; ============================

    (require 'dired)
    (require 'dired-x)
    ;; (require 'ange-ftp)
    (autoload 'wdired-change-to-wdired-mode "wdired")
    (add-hook 'dired-load-hook
              '(lambda ()
                 (load "dired-x")
                 (define-key dired-mode-map "r"
                   'wdired-change-to-wdired-mode)
                 (define-key dired-mode-map
                   [menu-bar immediate wdired-change-to-wdired-mode]
                   '("Edit File Names" . wdired-change-to-wdired-mode))
                 (load "ls-lisp")
                 ))

    (eval-after-load "dired"
      '(progn
         (setq ls-lisp-dirs-first t)
         (setq dired-recursive-copies 'always)
         (setq dired-recursive-deletes 'always)
         (setq dired-listing-switches "-labX")
         (setq dired-omit-mode t)
         ;;focus follows mouse
         (setq focus-follows-mouse t)
         ))

    (defun kill-all-dired-buffers()
      "Kill all dired buffers."
      (interactive)
      (save-excursion
        (let((count 0))
          (dolist(buffer (buffer-list))
            (set-buffer buffer)
            (when (equal major-mode 'dired-mode)
              (setq count (1+ count))
              (kill-buffer buffer)))
          (message "Killed %i dired buffer(s)." count ))))

    ;; If images are supported then display them when visiting them.
    (if (fboundp 'auto-image-file-mode)
        (auto-image-file-mode 1))

    ;; ============================
    ;; Session management
    ;; ============================

    ;;Minibuffer history saving
    (require 'savehist)
    (savehist-mode 1)
    (setq history-length 250)

    ;;Save backups in specific directory

    (setq
     backup-by-copying t                ; don't clobber symlinks
     backup-directory-alist
     '(("." . "~/.saves"))              ; don't litter my fs tree
     delete-old-versions t
     kept-new-versions 6
     kept-old-versions 2
     version-control t)                 ; use versioned backups


    ;; ============================
    ;; Look and feel
    ;; ============================

    ;; disable startup message
    (setq inhibit-startup-message t)

    ;;this sets the window title to the buffer name.
    (setq frame-title-format '("%b"))

    ;; http://levien.com/type/myfonts/inconsolata.html
    (set-frame-font "Inconsolata 18")

    ;;Color customization
    (add-to-list 'custom-theme-load-path "~/.emacs.d/")

    ;;https://github.com/PositronicBrain/bluelambda 
    (load-theme 'bluelambda t)

    (setq european-calendar-style t)
    (line-number-mode 1)
    (column-number-mode 1)
    ;; do not move cursor when pasting
    (global-set-key [mouse-2] 'yank)
    ;; highlighting
    (global-font-lock-mode 1)
    (setq font-lock-maximum-decoration 1)
    ;; no scroll bar
    (scroll-bar-mode -1)
    ;; Disable loading of menu bar.
    (menu-bar-mode -1)
    ;; Disable loading of tool bar.
    (tool-bar-mode -1)

    ;; stop cursor blinking
    (when (fboundp 'blink-cursor-mode)
      (blink-cursor-mode -1))
    ;; Make minibuffer larger if there is more to see
    (minibuffer-electric-default-mode 1)
    ;; Make some minor modes names shorter
    (when (require 'diminish nil 'noerror)
      (eval-after-load "flymake"
        '(diminish 'flymake-mode "Fmake"))
      (eval-after-load "compilation"
        '(diminish 'compilation-mode "comp"))
      )

    ;; ============================
    ;; Behaviour
    ;; ============================

    ;; http://emacswiki.org/emacs/DeleteSelectionMode
    (delete-selection-mode 1)

    ;; For copy and paste
    (setq x-select-enable-clipboard t)


    ;; http://www.padator.org/emacs/dircolors.el
    ;; Use dircolors to colorize the intermediate
    ;; output of things like C-x C-f
    (when (not (require 'dircolors nil 'noerror))
      (message "dircolors package not found!"))
    ;;Show Absolute Path on the Title Frame
    (setq frame-title-format "%f")
    ;; Bell
    ;; get a visual signal instead of beeping
    (setq visible-bell 1)
    ;; Killing line also deletes \n
    (setq-default kill-whole-line t)
    ;;Never use tabs!
    (setq-default indent-tabs-mode nil)
    ;;standard width
    (setq-default fill-column 74)    ; Wrap at col 74
    (setq-default tab-width 8)       ; Show tabs as 8 cols
    ;; Highlight matching parenthesis () [] {} <>
    ;; (depending on the mode)
    (require 'paren)
    (show-paren-mode t)

    ;; alias y to yes and n to no
    (defalias 'yes-or-no-p 'y-or-n-p)
    ;; iswitch buffer is better than the standard buffer manager
    (iswitchb-mode 1)

    ;; keys for buffer creation and navigation
    (global-set-key [(control x) (control b)] 'iswitchb-buffer)
    (global-set-key [(control x) (f)] 'find-file)

    ;; Make buffer names unique
    (require 'uniquify)
    (setq uniquify-buffer-name-style 'post-forward)

    ;; highlight matches from searches
    (setq isearch-highlight t)
    (setq search-highlight t)
    (setq-default transient-mark-mode t)

    ;; goto line function C-c C-g
    (global-set-key "\M-g" 'goto-line)

    ;; don't automatically add new lines when scrolling down at
    ;; the bottom of a buffer
    (setq next-line-add-newlines nil)

    ;; scroll just one line when hitting the bottom of the window
    (setq scroll-margin 7)
    (setq scroll-step 1)
    (setq scroll-conservatively 100000)
    ;; Pgup/dn will return exactly to the starting point.
    (setq scroll-preserve-screen-position 1)

    ;;Automatically indent lines in text mode
    (add-hook 'text-mode-hook 'turn-on-auto-fill)

    ;;wheel support
    (if (load "mwheel" t)
        (mwheel-install))

    ;; This allows to browse the kill ring
    ;; use M-y and C-h m to show available bindings

    ;; http://emacswiki.org/emacs/BrowseKillRing

    (if (require 'browse-kill-ring nil 'noerror)
        (browse-kill-ring-default-keybindings)
      (message "browse-kill-ring package not found!"))

    ;; ============================
    ;; Setup shell stuff
    ;; ============================

    (setq comint-buffer-maximum-size 2048)
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

    ;;Password hiding
    (add-hook 'comint-output-filter-functions
              'comint-watch-for-password-prompt)

    ;; ============================
    ;; Key mappings
    ;; ============================

    (global-set-key [f1] 'dired)
    (global-set-key [f2] 'kill-all-dired-buffers)

    ;; Disable <insert> key.
    (global-set-key
     [(insert)]
     (lambda () (interactive) (message "Insert disabled")))

    ;;unset C-z in window mode
    (when window-system
      (global-unset-key "\C-z"))

    ;; ============================
    ;; Perl development
    ;; ============================

    ;; cperl-mode is preferred to perl-mode
    ;; "Brevity is the soul of wit" <foo at acm.org>
    (defalias 'perl-mode 'cperl-mode)

    ;; ============================
    ;; Printer configuration
    ;; ============================

    (setq printer-name "lab_printer") ;; Ghostscript doesn't understand -P
    ;;(setq ps-lpr-command "lpr")
    ;;(setq ps-lpr-switches '("-o sides=two-sided-long-edge" "-o brightness=60"))

    ;; ============================
    ;; Spell checking
    ;; ============================

    (setq-default  ispell-program-name "/usr/bin/aspell")
    ;; Faster spell checking
    (setq ispell-extra-args '("--sug-mode=ultra"))

    ;; Turn on Flyspell mode automatically in Text mode and related modes.
    (autoload 'flyspell-mode "flyspell"
      "On-the-fly spelling checker." t)

    (set-language-environment "UTF-8")
    (set-buffer-file-coding-system 'utf-8)

    ;; Add good shortcut for flyspell. The hook makes sure when
    ;; flyspell-mode is on, the buffer gets scanned.

    (defun flyspell nil
      "Do the expected default, which is run flyspell on the whole buffer."
      (interactive)
      (flyspell-buffer))
    (add-hook 'flyspell-mode-hook 'flyspell-buffer)

    ;; After we add a word to Ispell or correct something, flyspell's
    ;; highlighting may become outdated. Let's re-run highlighting
    ;; after a correction.
    (defadvice ispell (after advice)
      (flyspell-buffer))
    (ad-activate 'ispell t)
    (defadvice ispell-word (after advice)
      (flyspell-buffer))
    (ad-activate 'ispell-word t)

    ;; ============================
    ;; Line numbers mode
    ;; ============================

    (autoload 'linum "Line numbers" t)

    ;; ============================
    ;; Latex development - Auctex
    ;; ============================

    ;; http://www.gnu.org/software/auctex

    (load "auctex.el" nil t t)
    (load "preview-latex.el" nil t t)

    (eval-after-load "auctex"
      '(progn
         (setq TeX-auto-save t)
         (setq TeX-parse-self t)
         ))

    (eval-after-load "preview-latex"
      '(progn
         (setq preview-auto-cache-preamble)
         ))

    (add-hook 'LaTeX-mode-hook
              (lambda()
                (TeX-PDF-mode)
                (flyspell-mode)))

    (setq ispell-parser 'tex)

    ;; Maximum higlighting is useful in latex

    (setq font-lock-maximum-decoration
          '((latex-mode . 2)))

    ;; ============================
    ;; Haskell
    ;; ============================

    ;; http://www.mew.org/~kazu/proj/ghc-mod/en/

    (add-to-list 'load-path "/usr/share/ghc-mod-1.11.3/")
    (autoload 'haskell-mode "haskell-mode" nil t)
    (autoload 'literate-haskell-mode "haskell-mode" nil t)
    (autoload 'haskell-cabal-mode "haskell-mode" nil t)
    (autoload 'ghc-init "ghc" nil t)

    ;; Interaction with ghci "C-c C-l"
    (require 'inf-haskell)

    (setq auto-mode-alist
          (append auto-mode-alist
                  '(("\\.[hg]s\\'" . haskell-mode)
                    ("\\.hi\\'"    . haskell-mode)
                    ("\\.hsc\\'"   . haskell-mode)
                    ("\\.l[hg]s$"  . literate-haskell-mode)
                    ("\\.cabal\\'" . haskell-cabal-mode))))


    ;; Color matching parenthesis
    (when (require 'rainbow-delimiters nil 'noerror)
      (add-hook 'haskell-mode-hook 'rainbow-delimiters-mode))

    (add-hook 'haskell-mode-hook
              (lambda ()
                (turn-on-haskell-indent)
                (turn-on-haskell-doc-mode)
                (turn-on-haskell-decl-scan)
                ;; Spellcheck comments)
                ;;(flyspell-prog-mode)
                (setq haskell-literate-default 'bird)
                (setq inferior-haskell-find-project-root nil)
                (ghc-init) (flymake-mode)
                ))

    ;; ============================
    ;; Maxima
    ;; ============================

    (add-to-list 'load-path "/usr/share/maxima/5.21.1/emacs/")
    (setq auto-mode-alist (cons '("\\.max" . maxima-mode)
                                auto-mode-alist))
    (autoload 'maxima-mode "maxima" "Maxima mode" t)
    (autoload 'maxima "maxima" "Run Maxima interactively" t)
    (autoload 'maxima-minor-mode "maxima"
      "Minor mode for working with Maxima" t)

    ;; Emaxima, literate mode
    (autoload 'emaxima-mode "emaxima" "EMaxima" t)

    ;; mark as emaxima file
    (add-hook 'emaxima-mode-hook 'emaxima-mark-file-as-emaxima)

    ;; ============================
    ;; org mode
    ;; ============================

    (org-remember-insinuate)
    (define-key global-map "\C-cr" 'org-remember)
    (eval-after-load "org-remember-mode"
      '(progn
         (setq org-directory "~/Documents/notes")
         (setq org-default-notes-file (concat org-directory "/notes.org"))

         ))
    (setq org-remember-templates
          '(("Todo" ?t "* TODO %?\n  %i\n  %a"
             "~/Documents/notes/TODO.org" "Tasks")
            ("Ideas" ?i "* %^{Title}\n  %i\n  %a"
             "~/Documents/notes/ideas.org" "A place for ideas")
            ("Read" ?r "* %^{Title}\n  %i\n  %a"
             "~/Documents/notes/must_read.org"
             "Books and papers which I have to check")
            ("Snippets" ?s "* %^{Title}\n  %i\n  %a"
             "~/Documents/notes/snippets.org"
             "Notes from books and papers")
            ("Misc" ?m "* %^{Title}\n  %i\n  %a"
             "~/Documents/notes/misc.org" "Miscellanous stuff")
            ("Lab" ?l "* %^{Title}\n  %i\n  %a"
             "~/Documents/notes/lab.org" "Lab diary")))

    ;; ==========================================
    ;; http://www.emacswiki.org/emacs/SmlModeLine
    ;; ==========================================

    ;; add an indicator of where in  the buffer we are
    (if (require 'sml-modeline nil 'noerror)
        (sml-modeline-mode 1)
      (message "SmlModeLine package not found!"))

    ;; ========================================
    ;; http://www.emacswiki.org/emacs/MultiTerm
    ;; ========================================

    ;; managing multiple terminal buffers in Emacs.
    (autoload 'multi-term "multi-term" nil t)
    (autoload 'multi-term-next "multi-term" nil t)
    ;;(require 'multi-term)
    (eval-after-load "multi-term"
      '(progn
         (setq multi-term-program "/bin/bash")

         ))
    (global-set-key [f3] 'multi-term)
    (global-set-key [f4] 'multi-term-next)

    ;; ====================================
    ;; http://www.emacswiki.org/emacs/UnDoc
    ;; ====================================

    ;; Read doc document

    (autoload 'undoc "undoc" "" t)
    (autoload 'undoc-current-buffer "undoc" "" t)
    (autoload 'undoc-region-after-mime-decode "undoc" "" t)

    ;; ============
    ;; Rainbow mode
    ;; ============

    ;; https://github.com/emacsmirror/rainbow-mode

    (when (not (require 'rainbow-mode nil 'noerror))
      (message "rainbow-mode package not found!"))

    ;; ============================
    ;; Tramp
    ;; ============================

    ;; Edit remote files as if they were loval ones

    (setq tramp-default-method "ssh")

    ;; ============================
    ;; Emacs sql
    ;; ============================

    ;; everything in one window
    ;; (require 'sql)
    ;; (defalias 'sql-get-login 'ignore)
    ;; (add-to-list 'same-window-buffer-names "*SQL*")
    ;; (setq sql-server "ensembldb.ensembl.org")
    ;; (setq sql-user "anonymous")
    ;; (setq sql-input-ring-file-name "~/.mysql_history")

    ;; ============================
    ;; ESS, Emacs Speaks Statistics
    ;; ============================

    (when (not (require 'ess-site nil 'noerror))
      (message "ESS, Emacs Speaks Statistics package not found!"))

    ;; ============================
    ;; LLVM
    ;; ============================


    (autoload 'llvm-mode "llvm-mode" nil t)

    (setq auto-mode-alist
          (append auto-mode-alist
                  '(("\\.ll\\'" . llvm-mode)
                    )))

    ;; print load time
    (message ".emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
                                      (- (+ hi lo) (+ (first *emacs-load-start*) (second *emacs-load-start*)))))
    ))
