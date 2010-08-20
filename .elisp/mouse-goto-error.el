;; mouse-goto-error.el
;;; Middle clicking on a Java stack back trace
;;; takes user to code for clicked upon frame.

;;; Sometimes, code differs between Emacs and XEmacs.
(defvar running-emacs-fsf (not (featurep 'xemacs)))
(defvar running-xemacs (featurep 'xemacs))
(defmacro emacs-fsf (&rest body)
  "Execute BODY if running (FSF) Emacs."
  (` (if running-emacs-fsf
	 (progn (,@ body)))))
(defmacro xemacs (&rest body)
  "Execute BODY if running XEmacs."
  (` (if running-xemacs
	 (progn (,@ body)))))



;; Bind mouse-2 to `mouse-yank-or-goto-error'; by default, it is bound to
;; `mouse-yank-at-click'.
;; Different code for Emacs and XEmacs.
(emacs-fsf
  (eval-after-load "shell"
    '(define-key shell-mode-map [mouse-2] 'mouse-yank-or-goto-error))
  ;; Make this work in Fundamental Mode.
  (define-key global-map [mouse-2] 'mouse-yank-or-goto-error))
(xemacs
  ;; In XEmacs, [mouse-2] attaches itself to button2up, not button2.  Thus,
  ;; both button2 (bound to mouse-yank) and this would get run.
  (eval-after-load "shell"
    '(define-key shell-mode-map [button2] 'mouse-yank-or-goto-error))
  ;; Make this work in Fundamental Mode.
  (define-key global-map [button2] 'mouse-yank-or-goto-error))



(defun mouse-yank-or-goto-error (click arg)
  "If an error message is under point, go to the error.  Otherwise,
call `mouse-yank-at-click'.
Arguments CLICK and ARG are as for `mouse-yank-at-click'.
In many cases -- for instance, in compilation buffers -- using `next-error'
is easier than clicking on the error."
  (interactive "@e\nP")
  (let ((click-point (or (emacs-fsf (posn-point (event-end click)))
			 (xemacs (event-point click))
			 (error "Where is the click?")))
	file line)
    (if (save-excursion
	  (goto-char click-point)
	  (beginning-of-line)
	  ;; Hack; should generalize
	  (if (looking-at "[-.a-zA-Z0-9]*: In function `")
	      (forward-line 1))
	  (require 'compile)
	  (let ((end-pos (save-excursion (end-of-line 1) (point)))
		(regexp-list compilation-error-regexp-alist))
	    (while regexp-list
	      (let ((regexp-info (car regexp-list)))
		(if (or (looking-at (car regexp-info))
			;; Check at preceding char, too:  many regexps start with "\n".
			(and (not (bobp))
			     (progn (backward-char 1)
				    (prog1 (looking-at (car regexp-info))
				      (forward-char 1))))
			;; not all regexps start match at beginning of line,
			;; but don't only search: let those anchored span
			;; multiple lines
			(re-search-forward (car regexp-info) end-pos t))
		    (progn
		      ;; (message "%s" (match-data))
		      (setq file (let ((raw (match-string (nth 1 regexp-info)))
				       (format (nth 4 regexp-info)))
				   (if format
				       (format format raw)
				     raw))
			    line (string-to-int (match-string (nth 2 regexp-info)))
			    ;; short-circuit testing
			    regexp-list nil))
		  (setq regexp-list (cdr regexp-list)))))
	    ;; return non-nil if matched
	    file))
      ;; Click was on an error/warning
      (let ((buffer (or (let ((abs-file (expand-file-name file)))
			  (and (file-exists-p abs-file)
			       (find-file-noselect abs-file)))
			(get-buffer file)
			(let ((completed (file-name-completion file default-directory)))
			  (and completed
			       (file-exists-p completed)
			       (find-file-noselect completed))))))
	;; If we didn't find it yet, try looking in TAGS tables and directories.
	(if (not buffer)
	    ;; not flet because flet is defined in cl-macs, not standard
	    (let ((find-in-tags-table
		   (function
		    (lambda (tags-file)
		      (if (file-exists-p tags-file)
			  (let ((tags-buf (find-file-noselect tags-file)))
			    (save-excursion
			      (set-buffer tags-buf)
			      (if (or (not (boundp 'tags-included-tables-function))
				      (not tags-included-tables-function))
				  (progn
				    (require 'etags)
				    ;; Necessary lest we get errors if we ever
				    ;; use this buffer as a tags table
				    ;; (etags-recognize-tags-table)
				    (initialize-new-tags-table)))
			      (goto-char (point-min))
			      (if (re-search-forward (concat "\f\n\\(.*/\\)?" file) nil t)
				  ;; return the buffer
				  (find-file-noselect
				   (concat default-directory
					   (buffer-substring (point) (line-beginning-position))))))))))))
	      (setq buffer
		    (or (and tags-file-name
			     (funcall find-in-tags-table tags-file-name))
			(let ((tags-files tags-table-list)
			      (result nil))
			  (while (and (not result) tags-files)
			    (setq result (funcall find-in-tags-table (car tags-files))
				  tags-files (cdr tags-files)))
			  result)
			(funcall find-in-tags-table (concat default-directory "TAGS"))
			;; for Java
			(save-excursion
			  (goto-char click-point)
			  (beginning-of-line)
			  ;; eg, "	at joie.code.BranchInstruction.getString(BranchInstruction.java:64)"

			  (and (looking-at "	at \\([^()]+\\.\\)?[^.]+\\.[^.]+([^()]+.java:[0-9]+)$")
			       ;; We dropped the last two components from the dotted operation name.
			       (let ((dir (or (match-string 1) "")))
				 (while (string-match "\\." dir)
				   (setq dir (replace-match "/" t t dir)))
				 (require 'dired)
				 (let ((dir-plus-file (concat dir file))
				       (java-dirs (split-string (getenv "CLASSPATH") ":"))
				       (result nil))
				   (while java-dirs
				     (if (file-directory-p (car java-dirs))
					 (let ((maybe-absname (concat
							       (file-name-as-directory (car java-dirs))
							       dir-plus-file)))
					   (if (file-exists-p maybe-absname)
					       (setq result
						     (find-file-noselect maybe-absname)
						     java-dirs nil))))
				     (setq java-dirs (cdr java-dirs)))
				   result))))
			;; Should use TAGS in Cecil/src, but find-in-tags-table
			;; doesn't understand include (though it would be easy
			;; to do so via tags-included-tables).
			(funcall find-in-tags-table vortex-compiler-tags-file)
			(and (file-exists-p (concat "~/vortex/Cecil/src/links/" file))
			     (find-file-noselect (concat "~/vortex/Cecil/src/links/" file)))))))
	(if (not buffer)
	    (error "Can't find file %s" file))
	(pop-to-buffer buffer)
	(if (= line 0)
	    (message "unknown line; not moving point")
	  (goto-line line)))
      ;; Click was not on an error/warning
      (mouse-yank-at-click click arg))))
