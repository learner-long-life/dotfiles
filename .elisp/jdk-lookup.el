;;; jdk-lookup.el
;;; Displays (in a separate browser window) JDK documentation for a Java
;;; package, class, method, or field, whose name is specified with completion.
;;; Originally jdk-goto-ref.el by Greg J. Badros -- 11/3/97.
;;; Substantially rewritten by Michael D. Ernst, 11/21/97, 4/22/99

(defvar jdk-ignored-prefixes
  (mapcar
   ;; quote it as a regexp if it doesn't start with ^
   (function (lambda (s)
	       (if (= ?^ (aref s 0))
		   s
		 (concat "^" (regexp-quote s)))))
   (let ((homedir (getenv "HOME")))
     ;; Leading ^ means it's already a regexp; otherwise, it gets regexp-quoted
     (list

     "http://geyer.lcs.mit.edu/jdk1.3-docs-api/"
     "http://web.mit.edu/6.170/www/javadoc/junit/"
     "http://web.mit.edu/6.170/www/javadoc/j2sdk1.4.1-docs-api/"

      ;; JDK
      ;; This doesn't work because we get the truename, not a symbolic link
      (concat "file:" homedir "/java/jdk/docs/api/")
      ;; "file:/uns/share/www/jdk/"
      ;; "^file:/projects/cse/www/uns/doc/jdk[.0-9]*/api/"
      ;; "^file:/cse/www/uns/doc/jdk[.0-9]*/api/"
      "^file:/g2/jdoc/jdk[.0-9]*/docs/api/"
      "^file:/usr/local/pkg/java/j2?s?dk[.0-9]*/docs/api/"

      ;; Other Javadoc
      (concat "file:" homedir "/java/OROMatcher-1.1/doc/api/")
      (concat "file:" homedir "/java/jakarta-log4j-1.2.7/docs/api/")
      ;; (concat "^file:" homedir "/java/[^/]*/doc/ajax-ship-[0-9]+/")
      (concat "^file:" homedir "/java/[^/]*/\\(java\\)?docs?/\\(api/\\)?")
      (concat "file:" homedir "/java/")
      (concat "file:" homedir "/research/invariants/java/doc/")
      )))
  "*Directories in which Java documentation is found.
These prefixes are stripped from filenames and URLs when determining
what Java entities are documented by a particular HTML file.")

(defvar jdk-index-filename (expand-file-name "~/emacs/jdk-index.el")
  "File mapping Java identifiers to HTML documentation files.
The mapping is created by the jdk-index-to-alist program.")
(defvar jdk-html-refs nil
  "Alist of (id . list-of-refs), read from file `jdk-index-filename'.")
(if (not jdk-html-refs)
    (load-file jdk-index-filename))

(defvar java-keywords
  '("abstract" "boolean" "break" "byte" "case" "catch" "char" "class"
    "const" "continue" "default" "do" "double" "else" "extends" "final"
    "finally" "float" "for" "goto" "if" "implements" "import" "instanceof"
    "int" "interface" "long" "native" "new" "package" "private" "protected"
    "public" "return" "short" "static" "super" "switch" "synchronized"
    "throw" "throws" "transient" "try" "void" "volatile" "while"))

(defun jdk-lookup (id)
  "Visit, via WWW browser, JDK documentation for a Java class or method."
  (interactive
   (list (let ((completion-ignore-case t))
	   (completing-read "JDK doc for: " jdk-html-refs nil t
			    (let* ((raw-guess (current-word))
				   (guess (if (member raw-guess java-keywords)
					      ""
					    raw-guess))
				   (try (try-completion guess jdk-html-refs)))
			      (if (eq try t) guess try))))))
  (let* ((refs (cdr (assoc id jdk-html-refs)))
	 (ref (if (= 1 (length refs))
		  (car refs)
		(let ((refs-as-lists (mapcar #'(lambda (ref)
						 (cons (ref-to-class ref) ref))
					     refs))
		      (completion-ignore-case t)
		      (choice nil))
		  ;; loop because completing-read can return null
		  (while (or (not choice) (equal choice ""))
		    (setq choice
			  (completing-read "Select an index entry: "
					   refs-as-lists
					   nil t
					   (try-completion "" refs-as-lists))))
		  (cdr (assoc choice refs-as-lists))))))
    (funcall browse-url-browser-function ref)))

(defun ref-to-class (str)
  "Given \"java/math/BigInteger.html#abs()\", return \"java.math.BigInteger.abs()\"."
  (let ((prefixes jdk-ignored-prefixes))
    (while prefixes
      (if (string-match (car prefixes) str)
	  (setq str (substring str (match-end 0))
		prefixes nil)
	(setq prefixes (cdr prefixes)))))
  (if (string-match "\\.html#" str)
      (setq str (replace-match "." t t str)))
  (if (string-match "\\.html$" str)
      (setq str (replace-match "" t t str)))
  (while (string-match "/" str)
    (setq str (replace-match "." t t str)))
  str)
;; Testing
;; (equal (ref-to-class "java/math/BigInteger.html#abs()") "java.math.BigInteger.abs()")


(eval-after-load "browse-url"
  ;; add open-paren to list of quoted characters
  '(defun browse-url-netscape (url &optional new-window)
  "Ask the Netscape WWW browser to load URL.

Default to the URL around or before point.  The strings in variable
`browse-url-netscape-arguments' are also passed to Netscape.

When called interactively, if variable `browse-url-new-window-p' is
non-nil, load the document in a new Netscape window, otherwise use a
random existing one.  A non-nil interactive prefix argument reverses
the effect of `browse-url-new-window-p'.

When called non-interactively, optional second argument NEW-WINDOW is
used instead of `browse-url-new-window-p'."
  (interactive (browse-url-interactive-arg "Netscape URL: "))
  ;; URL encode any `confusing' characters in the URL.  This needs to
  ;; include at least commas; presumably also close parens.
  (while (string-match "[,()]" url)
    (setq url (replace-match
	       (format "%%%x" (string-to-char (match-string 0 url))) t t url)))
  (let* ((process-environment (browse-url-process-environment))
         (process (apply 'start-process
			 (concat "netscape " url) nil
			 browse-url-netscape-program
			 (append
			  browse-url-netscape-arguments
			  (if (eq window-system 'w32)
			      (list url)
			    (append
			     (if new-window '("-noraise"))
			     (list "-remote"
				   (concat "openURL(" url
					   (if new-window ",new-window")
					   ")"))))))))
    (set-process-sentinel process
			  (list 'lambda '(process change)
				(list 'browse-url-netscape-sentinel 'process url))))))
