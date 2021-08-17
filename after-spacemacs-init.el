;(require 'ox-twbs) 
(require 'emojify)

(spacemacs/set-leader-keys "y" smerge-basic-map)

(org-babel-do-load-languages
   'org-babel-load-languages '((C . t)
                               (python . t)
                               (dot . t)))
(setq org-babel-python-command "python3")

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

(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("python" "dot"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
; For some reason emacs doesn't pick up the path when run from Appliactions
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))

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

(setq org-agenda-files '("~/org"))

(setq org-twbs-postamble nil)
(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.7.0")
;(setq org-reveal-root "file:///Users/falco.winkler/reveal.js-3.8.0")
(setq org-reveal-title-slide nil)

(setq org-latex-pdf-process (quote ("texi2dvi -p -b -V %f")))

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
