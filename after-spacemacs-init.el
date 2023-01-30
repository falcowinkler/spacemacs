(require 'ox-twbs)
(require 'emojify)

(require 'magit); To avoid Symbol’s value as variable is void: smerge-basic-map
(spacemacs/set-leader-keys "y" smerge-basic-map)

(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 ))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 ))))

(setq org-roam-v2-ack t)

(org-babel-do-load-languages
   'org-babel-load-languages '((C . t)
                               (python . t)
                               (ledger . t)
                               (haskell . nil)
                               (dot . t)))
(setq org-babel-python-command "python3")
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("python" "dot" "latex" "ledger"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(setq org-publish-project-alist 
          '(("site"
             :base-directory "~/Desktop/falcowinkler.github.io/org"
             :base-extension "org"
             :publishing-directory "~/Desktop/falcowinkler.github.io"
             :recursive t
             :publishing-function org-twbs-publish-to-html
             :headline-levels 4
             :auto-preamble t
             :auto-postamble nil
             :html-head-extra "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/python_course.css\">")
            ("site-images"
             :base-directory "~/Desktop/falcowinkler.github.io/org"
             :base-extension "png\\|jpg\\|svg"
             :recursive t
             :publishing-directory "~/Desktop/falcowinkler.github.io/images"
             :publishing-function org-publish-attachment
 )))

(setq org-src-preserve-indentation t)
; Include encrypted files to org agenda
(unless (string-match-p "\\.gpg" org-agenda-file-regexp)
  (setq org-agenda-file-regexp
        (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                  org-agenda-file-regexp)))

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(defun org-swift-code-block ()
  (interactive)
  (save-excursion
    (insert "#+BEGIN_SRC swift\n#+END_SRC"))
  (next-line))

(defun org-python-code-block ()
  (interactive)
  (save-excursion
    (insert "#+BEGIN_SRC python :results output :exports both\n\n#+END_SRC"))
  (next-line))

(defun org-elisp-code-block ()
  (interactive)
  (save-excursion
    (insert "#+BEGIN_SRC emacs-lisp\n\n#+END_SRC"))
  (next-line))

(defun org-dot-code-block ()
  (interactive)
  (save-excursion
    (insert (concat "#+BEGIN_SRC dot :file x.svg :cmdline -Kdot -Tsvg\n"
                    "\n"
                    "#+END_SRC\n"
                    "#+attr_html: :src /images/reading-notes/functional-programming/simple-types.svg\n"
                    "#+RESULTS:\n"
                    "[[file:x.svg]]"))
    (next-line)))

; For some reason emacs doesn't pick up the path when run from Appliactions
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))
(custom-set-variables
 '(org-directory "~/Dropbox/org")
 '(org-agenda-files (list org-directory)))

(setq org-todo-keywords
      '(
        (sequence "IDEA(i)" "TODO(t)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
        (sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)")
        ))
(setq org-todo-keyword-faces
      '(("IDEA" . (:foreground "GoldenRod" :weight bold))
        ("NEXT" . (:foreground "IndianRed1" :weight bold))
        ("STARTED" . (:foreground "OrangeRed" :weight bold))
        ("WAITING" . (:foreground "coral" :weight bold))
        ("CANCELED" . (:foreground "LimeGreen" :weight bold))
        ("DELEGATED" . (:foreground "LimeGreen" :weight bold))
        ("SOMEDAY" . (:foreground "LimeGreen" :weight bold))
        ))

(setq org-default-notes-file (concat org-directory "~/inbox.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file "~/Dropbox/org/inbox.org")
         "* TODO %?\n  %i\n  %a")
        ))

(setq org-agenda-files '("~/Dropbox/org/"))

(setq org-twbs-postamble nil)
(setq org-re-reveal-root "file:///Users/fawi/reveal.js")
(setq org-re-reveal-revealjs-version "3.6.0")
;(setq org-reveal-root "file:///Users/falco.winkler/reveal.js-3.8.0")
(setq org-reveal-title-slide nil)

(setq org-latex-pdf-process (quote ("texi2dvi -p -b -V %f")))

(setq org-roam-directory (file-truename "~/Dropbox/org/roam"))
(org-roam-db-autosync-mode)
(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)
        ("d" "default-encrypted" plain "%?"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg"
                            "#+title: ${title}\n")
         :unnarrowed t)
        ("s" "solution" plain
         "* Current state\n%?\n* Proposed Solutions\n\n* Advantages\n\n* Disadvantages"
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)
        ("p" "person" plain
         (file "~/.emacs.d/templates/person.org")
         :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                            "#+title: ${title}\n")
        :unnarrowed t)))

(setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)

(require 'epa-file)
(setf epa-pinentry-mode 'loopback)
(setq epa-file-select-keys nil)
(setq epa-file-encrypt-to '("falcowinkler@icloud.com"))

;; (dap-register-debug-template
;;  "default pytest debur"
;;  (list :type "python"
;;        :request "launch"
;;        :args "-m pytest -sv"
;;        :target-module "tests"
;;        :name "Default pytest debug"))

(global-set-key (kbd "M-i") 'imenu)

(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

; usually more reliable
(setq pytest-cmd-format-string  "cd '%s' && python -m pytest")

(setq-default dotspacemacs-configuration-layers
  '((javascript :variables javascript-repl `nodejs)))

(with-eval-after-load 'treemacs
  (treemacs-resize-icons 15))

(require 'jq-mode)

;; --- jq support
(defun restclient-jq-result-end-point ()
  (save-excursion
    (goto-char (point-max))
    (or (and (re-search-backward "^[^/].*" nil t)
	     (line-end-position))
	(point-max))))

(defun restclient-jq-get-var (jq-pattern)
  (with-temp-buffer
    (let ((output (current-buffer)))
      (with-current-buffer restclient-same-buffer-response-name
        (call-process-region
         (point-min)
         (restclient-jq-result-end-point)
         shell-file-name
         nil
         output
         nil
         shell-command-switch
         (format "%s %s %s"
                 jq-interactive-command
		 "-r"
                 (shell-quote-argument jq-pattern))))
      (string-trim (buffer-string)))))

(defun restclient-jq-json-var-function (args args-offset)
  (save-match-data
    (and (string-match "\\(:[^: \n]+\\) \\(.*\\)$" args)
         (let ((var-name (match-string 1 args))
               (jq-patt (match-string 2 args)))
           (lambda ()
             (let ((resp-val (restclient-jq-get-var jq-patt)))
               (restclient-remove-var var-name)
               (restclient-set-var var-name resp-val)
               (message "restclient var [%s = \"%s\"] " var-name resp-val)))))))

(defun restclient-jq-interactive-result ()
  (interactive)
  (flush-lines "^//.*") ;; jq doesnt like comments
  (jq-interactively (point-min) (restclient-jq-result-end-point)))

;; todo: eval-after-load should be used in configuration, not
;; packages. Replace with a better solution.
(eval-after-load 'restclient
  '(progn
     (restclient-register-result-func
      "jq-set-var" #'restclient-jq-json-var-function
      "Set a restclient variable with the value jq expression,
       takes var & jq expression as args.
       eg. -> jq-set-var :my-token .token")
     (define-key restclient-response-mode-map  (kbd "C-c C-j") #'restclient-jq-interactive-result)))
