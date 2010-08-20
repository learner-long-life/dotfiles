;;;
;;; 6170.el: Emacs customizations for 6.170
;;;
;;; To use this file: add
;;;   (load "/mit/6.170/etc/emacs/6170.el")
;;; somewhere in your ".emacs" file.
;;;
;;; The capabilities of 6170.el are listed in the Tools handout.
;;; under 'Using Emacs to edit Java code':
;;;   http://www.mit.edu/~6.170/supplemental-info/tools.html#Emacs-Java

;;; Get required prerequisite packages loaded.
(require 'cc-mode)
(require 'compile)

(setq load-path (append '("~/.elisp"
;;			  "/mit/6.170/etc/emacs/pcl-cvs-2.9.9"
;;			  "/mit/6.170/etc/emacs/elib-1.0" ; needed by pcl-cvs
			  )
			load-path))

(load "mouse-goto-error")

(setq jdk-index-filename "jdk-index.el")
(autoload 'jdk-lookup "jdk-lookup" "Look up Java entity." t)

;;(load "pcl-cvs-startup")		; autoload CVS integration

;;; Define a global variable to control whether any modifications are
;;; actually made.  Use
;;;  (setq make-6170-changes nil)
;;; from your .emacs before you load this file to just define the
;;; functions.
(defvar make-6170-changes t
  "*If t, make default changes to java-mode-hook when 6170.el is loaded.
To avoid the default changes but still define functions, set this
variable to nil before loading 6170.el.")

;;; Define an elisp function to make the default command for M-x compile
;;; be 'javac *.java' if no makefile exists in the current directory.
;;;
;;; MODIFIES: compile-command
;;; EFFECTS: If there is no file named makefile or Makefile in the
;;;          current directory, makes compile-command be buffer-local
;;;          and sets its value to "javac -g *.java".
(defun make-compile-command-javac ()
  "Maybe change `compile-command' in the current buffer to run 'javac'.

If a Makefile exists in the current directory, don't make the change.
It might be useful to add this as a hook in certain modes, like this:

    (add-hook 'java-mode-hook 'make-compile-command-javac)"
  (interactive)
  (unless (or (file-exists-p "makefile")
	      (file-exists-p "Makefile"))
    (make-local-variable 'compile-command)
    ;;; For Jikes:
    ; (setq compile-command "cjikes -g +E +F *.java")
    (setq compile-command "javac -g *.java")))

(defadvice compile (before save-before-compile activate)
  "Save current buffer before performing compilation.
This avoids a question, the answer to which woulbe be sure to be \"Yes\"."
  ;; test of buffer-modified-p prevents "(No changes need to be saved)" message
  (if (and buffer-file-name (buffer-modified-p))
      (save-buffer)))


;;; Define an elisp function to reduce the value of c-basic-offset
;;; from its default of 4 to 2.  This has the effect of "narrowing"
;;; the indentation, since c-basic-offset essentially determines
;;; the number of spaces per level of indentation.
;;;
;;; c-basic-offset isn't buffer-local, but it seems to get overwritten
;;; when visiting new files; thus, this should probably be added to
;;; a mode hook.
;;;
;;; MODIFIES: c-basic-offset
;;; EFFECTS: Sets c-basic-offset to 2.
(defun make-c-indentation-small ()
  "Changes the value of c-basic-offset to 2.
This has the effect of reducing the size of indentation in C,
C++, and Java mode buffers."
  (interactive)
  (setq c-basic-offset 2))

;;; Define an elisp function to make C-c C-c do the same thing as
;;; M-x compile.
;;;
;;; MODIFIES: Local keymap
;;; EFFECTS: Rebinds C-c C-c to 'compile.
(defun make-cc-cc-compile ()
  "Rebinds C-c C-c to run `compile'."
  (interactive)
  (local-set-key [(control c) (control c)] 'compile))

;;; Define an elisp function to make C-c C-r do the same thing as
;;; M-x comment-region.
;;;
;;; MODIFIES: Local keymap
;;; EFFECTS: Rebinds C-c C-r to 'comment-region.
(defun make-cc-cr-comment ()
  "Rebinds C-c C-r to run `comment-region'.
C-u C-c C-r will remove commenting in the selected region."
  (interactive)
  (local-set-key [(control c) (control r)] 'comment-region))

;;; Swap RET and newline.  This will generally make RET run
;;; newline-and-indent, and C-j just insert a newline.
;;;
;;; MODIFIES: Local keymap
;;; EFFECTS: Rebinds RET to 'newline-and-indent, and C-j to 'newline.
(defun swap-ret-and-newline ()
  "Swaps the effects of RET and newline.
Pressing RET now runs `newline-and-indent', while pressing C-j just
inserts a newline."
  (interactive)
  (local-set-key "\15" 'newline-and-indent)
  (local-set-key "\12" 'newline))

(defun bind-C-f-to-jdk-lookup ()
  "Bind C-f to `jdk-lookup' in `java-mode-map'."
  (define-key java-mode-map "\C-hf" 'jdk-lookup))

;; Consider adding this to write-file-hooks in Java Mode.
(defun untabify-buffer ()
  "Convert all tabs in buffer to multiple spaces, preserving columns.
See `untabify'."
  (untabify (point-min) (point-max)))


;;; Add the previously-defined function to a hook that gets run
;;; on entering java-mode.  Many of these things are more generally
;;; useful, and you might want to add them to, for example,
;;; c-common-mode-hook, which also affects c- and c++-mode.
;;;
;;; Only make these changes if make-6170-changes is t (see above).
(when make-6170-changes
  (add-hook 'java-mode-hook 'make-compile-command-javac)
  (add-hook 'java-mode-hook 'make-c-indentation-small)
  (add-hook 'java-mode-hook 'make-cc-cc-compile)
  (add-hook 'java-mode-hook 'make-cc-cr-comment)
  (add-hook 'java-mode-hook 'swap-ret-and-newline)
  (add-hook 'java-mode-hook 'bind-C-f-to-jdk-lookup)

  ;;; Enable font-lock-mode (syntax highlighting) in all modes that
  ;;; support it.  (This isn't necessary in XEmacs.)  If you don't like
  ;;; this, but you do like the java-mode fontification, you can add
  ;;; 'turn-on-font-lock to java-mode-hook:
  ;;;   (add-hook 'java-mode-hook 'turn-on-font-lock)
  (if (fboundp 'global-font-lock-mode)
      (global-font-lock-mode 1))
  )
