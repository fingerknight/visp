;;; visp.el --- yet another image of Paredit

;; Copyright (C) 2022--2022 Finger Knight

;; Author: Finger Knight <mrdust1880@outlook.com>
;; Version: beta
;; Created: 2022-10-18
;; Keywords: lisp

;; This ia another image of Paredit.
;; Since Lisp code is based on normal structure, Visp aims to let
;; normal mode like in VIM, to edit Lisp code.

;;; Code

(defconst visp-version 1)
(defconst visp-beta-p t)

(defun visp-xemacs-p ()
  ;; No idea where I got this definition from.  Edward O'Connor
  ;; (hober in #emacs) suggested the current definition.
  ;;   (and (boundp 'running-xemacs)
  ;;        running-xemacs)
  (featurep 'xemacs))

(defun visp-gnu-emacs-p ()
  ;; ++ This could probably be improved.
  (not (visp-xemacs-p)))

(defmacro xcond (&rest clauses)
  "Exhaustive COND.
Signal an error if no clause matches."
  `(cond ,@clauses
         (t (error "XCOND lost."))))

(defalias 'visp-warn (if (fboundp 'warn) 'warn 'message))

(defvar visp-sexp-error-type
  (with-temp-buffer
    (insert "(")
    (condition-case condition
        (backward-sexp)
      (error (if (eq (car condition) 'error)
                 (visp-warn "%s%s%s%s%s"
                            "Visp is unable to discriminate"
                            " S-expression parse errors from"
                            " other errors. "
                            " This may cause obscure problems. "
                            " Please upgrade Emacs."))
             (car condition)))))

(defmacro visp-handle-sexp-errors (body &rest handler)
  `(condition-case ()
       ,body
     (,visp-sexp-error-type ,@handler)))

(put 'visp-handle-sexp-errors 'lisp-indent-function 1)

(defmacro visp-ignore-sexp-errors (&rest body)
  `(visp-handle-sexp-errors (progn ,@body)
			    nil))

(put 'visp-ignore-sexp-errors 'lisp-indent-function 0)

(defmacro visp-preserving-column (&rest body)
  "Evaluate BODY and restore point to former column, relative to code.
Assumes BODY will change only indentation.
If point was on code, it moves with the code.
If point was on indentation, it stays in indentation."
  (let ((column (make-symbol "column"))
        (indentation (make-symbol "indentation")))
    `(let ((,column (current-column))
           (,indentation (visp-current-indentation)))
       (let ((value (progn ,@body)))
         (visp-restore-column ,column ,indentation)
         value))))

(put 'visp-preserving-column 'lisp-indent-function 0)


;;;; Minor Mode Definition

(defvar visp-lighter " V"
  "Mode line lighter Visp Mode.")

(defvar visp-mode-map (make-sparse-keymap)
  "Keymap for the visp minor mode.")

(defvar visp-override-check-parens-function
  (lambda (condition) (declare ignore condition) nil)
  "Function to tell whether unbalanced text should inhibit Visp Mode.")

;;;###autoload
(define-minor-mode visp-mode
  "Minor mode for pseudo-structurally editing Lisp code.
With a prefix argument, enable Visp Mode even if there are
  unbalanced parentheses in the buffer.
Visp behaves badly if parentheses are unbalanced, so exercise
  caution when forcing Visp Mode to be enabled, and consider
  fixing unbalanced parentheses instead.
\\<visp-mode-map>"
  :lighter visp-lighter
  ;; Setting `visp-mode' to false here aborts enabling Visp Mode.
  (if (and visp-mode
           (not current-prefix-arg))
      (condition-case condition
          (check-parens)
        (error
         (if (not (funcall visp-override-check-parens-function condition))
             (progn (setq visp-mode nil)
                    (signal (car condition) (cdr condition))))))))

(defun visp-override-check-parens-interactively (condition)
  (y-or-n-p (format "Enable Visp Mode despite condition %S? " condition)))

;;;###autoload
(defun enable-visp-mode ()
  "Turn on pseudo-structural editing of Lisp code."
  (interactive)
  (visp-mode +1))

(defun disable-visp-mode ()
  "Turn off pseudo-structural editing of Lisp code."
  (interactive)
  (visp-mode -1))

(defvar visp-backward-delete-key
  (xcond ((visp-xemacs-p)    "BS")
         ((visp-gnu-emacs-p) "DEL")))

(defvar visp-forward-delete-keys
  (xcond ((visp-xemacs-p)    '("DEL"))
         ((visp-gnu-emacs-p) '("<delete>" "<deletechar>"))))

;;;; Visp Keys

;;; Separating the definition and initialization of this variable
;;; simplifies the development of visp, since re-evaluating DEFVAR
;;; forms doesn't actually do anything.

(defvar visp-commands nil
  "List of visp commands with their keys and examples.")

;;; Each specifier is of the form:
;;;   (key[s] function (example-input example-output) ...)
;;; where key[s] is either a single string suitable for passing to KBD
;;; or a list of such strings.  Entries in this list may also just be
;;; strings, in which case they are headings for the next entries.

(setq visp-commands
 `(
   "Basic Insertion Commands"
   ("("         visp-open-round
                ("(a b |c d)"
                 "(a b (|) c d)")
                ("(foo \"bar |baz\" quux)"
                 "(foo \"bar (|baz\" quux)"))
   (")"         visp-close-round
                ("(a b |c   )" "(a b c)|")
                ("; Hello,| world!"
                 "; Hello,)| world!"))
   ("M-)"       visp-close-round-and-newline
                ("(defun f (x|  ))"
                 "(defun f (x)\n  |)")
                ("; (Foo.|"
                 "; (Foo.)|"))
   ("["         visp-open-square
                ("(a b |c d)"
                 "(a b [|] c d)")
                ("(foo \"bar |baz\" quux)"
                 "(foo \"bar [|baz\" quux)"))
   ("]"         visp-close-square
                ("(define-key keymap [frob|  ] 'frobnicate)"
                 "(define-key keymap [frob]| 'frobnicate)")
                ("; [Bar.|"
                 "; [Bar.]|"))

   ("\""        visp-doublequote
                ("(frob grovel |full lexical)"
                 "(frob grovel \"|\" full lexical)"
                 "(frob grovel \"\"| full lexical)")
                ("(foo \"bar |baz\" quux)"
                 "(foo \"bar \\\"|baz\" quux)")
                ("(frob grovel)   ; full |lexical"
                 "(frob grovel)   ; full \"|lexical"))
   ("M-\""      visp-meta-doublequote
                ("(foo \"bar |baz\" quux)"
                 "(foo \"bar baz\"| quux)")
                ("(foo |(bar #\\x \"baz \\\\ quux\") zot)"
                 ,(concat "(foo \"|(bar #\\\\x \\\"baz \\\\"
                          "\\\\ quux\\\")\" zot)")))
   ("\\"        visp-backslash
                ("(string #|)\n  ; Character to escape: x"
                 "(string #\\x|)")
                ("\"foo|bar\"\n  ; Character to escape: \""
                 "\"foo\\\"|bar\""))
   (";"         visp-semicolon
                ("|(frob grovel)"
                 ";|(frob grovel)")
                ("(frob |grovel)"
                 "(frob ;|grovel\n )")
                ("(frob |grovel (bloit\n               zargh))"
                 "(frob ;|grovel\n (bloit\n  zargh))")
                ("(frob grovel)          |"
                 "(frob grovel)          ;|"))
   ("M-;"       visp-comment-dwim
                ("(foo |bar)   ; baz"
                 "(foo bar)                               ; |baz")
                ("(frob grovel)|"
                 "(frob grovel)                           ;|")
                ("(zot (foo bar)\n|\n     (baz quux))"
                 "(zot (foo bar)\n     ;; |\n     (baz quux))")
                ("(zot (foo bar) |(baz quux))"
                 "(zot (foo bar)\n     ;; |\n     (baz quux))")
                ("|(defun hello-world ...)"
                 ";;; |\n(defun hello-world ...)"))

   ("C-j"       visp-newline
                ("(let ((n (frobbotz))) |(display (+ n 1)\nport))"
                 ,(concat "(let ((n (frobbotz)))"
                          "\n  |(display (+ n 1)"
                          "\n           port))")))

   "Deleting & Killing"
   (("C-d" ,@visp-forward-delete-keys)
                visp-forward-delete
                ("(quu|x \"zot\")" "(quu| \"zot\")")
                ("(quux |\"zot\")"
                 "(quux \"|zot\")"
                 "(quux \"|ot\")")
                ("(foo (|) bar)" "(foo | bar)")
                ("|(foo bar)" "(|foo bar)"))
   (,visp-backward-delete-key
                visp-backward-delete
                ("(\"zot\" q|uux)" "(\"zot\" |uux)")
                ("(\"zot\"| quux)"
                 "(\"zot|\" quux)"
                 "(\"zo|\" quux)")
                ("(foo (|) bar)" "(foo | bar)")
                ("(foo bar)|" "(foo bar|)"))
   ("C-k"       visp-kill
                ("(foo bar)|     ; Useless comment!"
                 "(foo bar)|")
                ("(|foo bar)     ; Useful comment!"
                 "(|)     ; Useful comment!")
                ("|(foo bar)     ; Useless line!"
                 "|")
                ("(foo \"|bar baz\"\n     quux)"
                 "(foo \"|\"\n     quux)"))
   ("M-d"       visp-forward-kill-word
                ("|(foo bar)    ; baz"
                 "(| bar)    ; baz"
                 "(|)    ; baz"
                 "()    ;|")
                (";;;| Frobnicate\n(defun frobnicate ...)"
                 ";;;|\n(defun frobnicate ...)"
                 ";;;\n(| frobnicate ...)"))
   (,(concat "M-" visp-backward-delete-key)
                visp-backward-kill-word
                ("(foo bar)    ; baz\n(quux)|"
                 "(foo bar)    ; baz\n(|)"
                 "(foo bar)    ; |\n()"
                 "(foo |)    ; \n()"
                 "(|)    ; \n()"))

   "Movement & Navigation"
   ("l"     visp-forward
                ("(foo |(bar baz) quux)"
                 "(foo (bar baz)| quux)")
                ("(foo (bar)|)"
                 "(foo (bar))|"))
   ("h"     visp-backward
                ("(foo (bar baz)| quux)"
                 "(foo |(bar baz) quux)")
                ("(|(foo) bar)"
                 "|((foo) bar)"))
   ("b"     visp-backward-up)
   ("f"     visp-forward-down)
   ("d"     visp-different)

   "Depth-Changing Commands"
   ("M-("       visp-wrap-round
                ("(foo |bar baz)"
                 "(foo (|bar) baz)"))
   ("M-s"       visp-splice-sexp
                ("(foo (bar| baz) quux)"
                 "(foo bar| baz quux)"))
   (("M-<up>" "ESC <up>")
                visp-splice-sexp-killing-backward
                ("(foo (let ((x 5)) |(sqrt n)) bar)"
                 "(foo |(sqrt n) bar)"))
   (("M-<down>" "ESC <down>")
                visp-splice-sexp-killing-forward
                ("(a (b c| d e) f)"
                 "(a b c| f)"))
   ("M-r"       visp-raise-sexp
                ("(dynamic-wind in (lambda () |body) out)"
                 "(dynamic-wind in |body out)"
                 "|body"))
   ("M-?"       visp-convolute-sexp
                ("(let ((x 5) (y 3)) (frob |(zwonk)) (wibblethwop))"
                 "(frob |(let ((x 5) (y 3)) (zwonk) (wibblethwop)))"))

   "Barfage & Slurpage"
   (("C-)" "C-<right>")
                visp-forward-slurp-sexp
                ("(foo (bar |baz) quux zot)"
                 "(foo (bar |baz quux) zot)")
                ("(a b ((c| d)) e f)"
                 "(a b ((c| d) e) f)"))
   (("C-}" "C-<left>")
                visp-forward-barf-sexp
                ("(foo (bar |baz quux) zot)"
                 "(foo (bar |baz) quux zot)"))
   (("C-(" "C-M-<left>" "ESC C-<left>")
                visp-backward-slurp-sexp
                ("(foo bar (baz| quux) zot)"
                 "(foo (bar baz| quux) zot)")
                ("(a b ((c| d)) e f)"
                 "(a (b (c| d)) e f)"))
   (("C-{" "C-M-<right>" "ESC C-<right>")
                visp-backward-barf-sexp
                ("(foo (bar baz |quux) zot)"
                 "(foo bar (baz |quux) zot)"))

   "Miscellaneous Commands"
   ("M-S"       visp-split-sexp
                ("(hello| world)"
                 "(hello)| (world)")
                ("\"Hello, |world!\""
                 "\"Hello, \"| \"world!\""))
   ("M-J"       visp-join-sexps
                ("(hello)| (world)"
                 "(hello| world)")
                ("\"Hello, \"| \"world!\""
                 "\"Hello, |world!\"")
                ("hello-\n|  world"
                 "hello-|world"))
   ("C-c C-M-l" visp-recenter-on-sexp)
   ("M-q"       visp-reindent-defun)
   ))

(defun visp-define-keys ()
  (--map-when
   (listp it)
   (let* ((k (car it))
	  (keys (cond ((stringp k) (list k))
		      ((listp k) k)
		      (t (error "Invalid visp command %s."
                                it))))
	  (fn (cadr it))
	  (examples (cddr it)))
     (--map
      (define-key visp-mode-map (read-kbd-macro it) fn)
      keys))
  visp-commands))

(defun visp-function-documentation (fn)
  "Get document of FN.

Fisrtly, try to get doc if it is binded in symbol FN.
Otherwise, use `documentation' to get."
  (let ((original-doc (get fn 'visp-original-documentation))
        (doc (documentation fn 'function-documentation)))
    (or original-doc
        (progn (put fn 'visp-original-documentation doc)
               doc))))

;; (defun visp-annotate-mode-with-examples ()
;;   (let ((contents
;;          (list (visp-function-documentation 'visp-mode))))
;;     (visp-do-commands (spec keys fn examples)
;;         (push (concat "\n\n" spec "\n")
;;               contents)
;;       (let ((name (symbol-name fn)))
;;         (if (string-match (symbol-name 'visp-) name)
;;             (push (concat "\n\n\\[" name "]\t" name
;;                           (if examples
;;                               (mapconcat (lambda (example)
;;                                            (concat
;;                                             "\n"
;;                                             (mapconcat 'identity
;;                                                        example
;;                                                        "\n  --->\n")
;;                                             "\n"))
;;                                          examples
;;                                          "")
;;                               "\n  (no examples)\n"))
;;                   contents))))
;;     (put 'visp-mode 'function-documentation
;;          (apply 'concat (reverse contents))))
;;   ;; PUT returns the huge string we just constructed, which we don't
;;   ;; want it to return.
;;   nil)

;; (defun visp-annotate-functions-with-examples ()
;;   (visp-do-commands (spec keys fn examples)
;;       nil       ; string case
;;     (put fn 'function-documentation
;;          (concat (visp-function-documentation fn)
;;                  "\n\n\\<visp-mode-map>\\[" (symbol-name fn) "]\n"
;;                  (mapconcat (lambda (example)
;;                               (concat "\n"
;;                                       (mapconcat 'identity
;;                                                  example
;;                                                  "\n  ->\n")
;;                                       "\n"))
;;                             examples
;;                             "")))))

;;;;; HTML Examples

;; (defun visp-insert-html-examples ()
;;   "Insert HTML for a visp quick reference table."
;;   (interactive)
;;   (let ((insert-lines
;;          (lambda (&rest lines) (dolist (line lines) (insert line) (newline))))
;;         (initp nil))
;;     (visp-do-commands (spec keys fn examples)
;;         (progn (if initp
;;                    (funcall insert-lines "</table>")
;;                    (setq initp t))
;;                (funcall insert-lines (concat "<h3>" spec "</h3>"))
;;                (funcall insert-lines "<table>"))
;;       (let ((name (symbol-name fn))
;;             (keys
;;              (mapconcat (lambda (key)
;;                           (concat "<tt>" (visp-html-quote key) "</tt>"))
;;                         keys
;;                         ", ")))
;;         (funcall insert-lines "<tr>")
;;         (funcall insert-lines (concat "  <th align=\"left\">" keys "</th>"))
;;         (funcall insert-lines (concat "  <th align=\"left\">" name "</th>"))
;;         (funcall insert-lines "</tr>")
;;         (funcall insert-lines
;;                  "<tr><td colspan=\"2\"><table cellpadding=\"5\"><tr>")
;;         (dolist (example examples)
;;           (let ((prefix "<td><table border=\"1\"><tr><td><table><tr><td><pre>")
;;                 (examples
;;                  (mapconcat 'visp-html-quote
;;                             example
;;                             (concat "</pre></td></tr>"
;;                                     "<tr><th>&darr;</th></tr>"
;;                                     "<tr><td><pre>")))
;;                 (suffix "</pre></td></tr></table></td></tr></table></td>"))
;;             (funcall insert-lines (concat prefix examples suffix))))
;;         (funcall insert-lines "</tr></table></td></tr>")))
;;     (funcall insert-lines "</table>")))

;; (defun visp-html-quote (string)
;;   (with-temp-buffer
;;     (dotimes (i (length string))
;;       (insert (let ((c (elt string i)))
;;                 (cond ((eq c ?\<) "&lt;")
;;                       ((eq c ?\>) "&gt;")
;;                       ((eq c ?\&) "&amp;")
;;                       ((eq c ?\') "&apos;")
;;                       ((eq c ?\") "&quot;")
;;                       (t c)))))
;;     (buffer-string)))

;;;; Delimiter Insertion

(defun visp-conc-name (&rest strings)
  (intern (apply 'concat strings)))

(defmacro define-visp-pair (open close name)
  `(progn
     (defun ,(visp-conc-name "visp-open-" name) (&optional n)
       ,(concat "Insert a balanced " name " pair.
With a prefix argument N, put the closing " name " after N
  S-expressions forward.
If the region is active, `transient-mark-mode' is enabled, and the
  region's start and end fall in the same parenthesis depth, insert a
  " name " pair around the region.
If in a string or a comment, insert a single " name ".
If in a character literal, do nothing.  This prevents changing what was
  in the character literal to a meaningful delimiter unintentionally.")
       (interactive "P")
       (cond ((or (visp-in-string-p)
                  (visp-in-comment-p))
              (insert ,open))
             ((not (visp-in-char-p))
              (visp-insert-pair n ,open ,close 'goto-char)
              (save-excursion (backward-up-list) (indent-sexp)))))
     (defun ,(visp-conc-name "visp-close-" name) ()
       ,(concat "Move past one closing delimiter and reindent.
\(Agnostic to the specific closing delimiter.)
If in a string or comment, insert a single closing " name ".
If in a character literal, do nothing.  This prevents changing what was
  in the character literal to a meaningful delimiter unintentionally.")
       (interactive)
       (visp-move-past-close ,close))
     (defun ,(visp-conc-name "visp-close-" name "-and-newline") ()
       ,(concat "Move past one closing delimiter, add a newline,"
                " and reindent.
If there was a margin comment after the closing delimiter, preserve it
  on the same line.")
       (interactive)
       (visp-move-past-close-and-newline ,close))
     (defun ,(visp-conc-name "visp-wrap-" name)
         (&optional argument)
       ,(concat "Wrap the following S-expression.
See `visp-wrap-sexp' for more details.")
       (interactive "P")
       (visp-wrap-sexp argument ,open ,close))
     (add-to-list 'visp-wrap-commands
                  ',(visp-conc-name "visp-wrap-" name))))

(defvar visp-wrap-commands '(visp-wrap-sexp)
  "List of visp commands that wrap S-expressions.
Used by `visp-yank-pop'; for internal visp use only.")

(define-visp-pair ?\( ?\) "round")
(define-visp-pair ?\[ ?\] "square")
(define-visp-pair ?\{ ?\} "curly")
(define-visp-pair ?\< ?\> "angled")

;;; Aliases for the old names.

(defalias 'visp-open-parenthesis 'visp-open-round)
(defalias 'visp-close-parenthesis 'visp-close-round)
(defalias 'visp-close-parenthesis-and-newline
  'visp-close-round-and-newline)

(defalias 'visp-open-bracket 'visp-open-square)
(defalias 'visp-close-bracket 'visp-close-square)
(defalias 'visp-close-bracket-and-newline
  'visp-close-square-and-newline)

(defvar visp-left-delimiter "[([{]"
  "Opening delimiter.")

(defvar visp-right-delimiter "[])}]"
  "Closing delimiter.")

(defsubst visp-right-p ()
  "Return t if after variable `lispy-right'."
  (looking-back visp-right-delimiter
                (line-beginning-position)))

(defsubst visp-left-p ()
  "Return t if before variable `lispy-left'."
  (looking-at visp-left-delimiter))

(defun visp-different ()
  "Switch to the different side of current sexp."
  (interactive)
  (cond ((and (region-active-p)
              (not (= (region-beginning) (region-end))))
         (exchange-point-and-mark))
        ((visp-right-p)
         (backward-list))
        ((visp-left-p)
         (forward-list))
        (t
         (user-error "Unexpected"))))

(defun visp-move-past-close (close)
  (visp-move-past-close-and close
    (lambda ()
      (visp-blink-paren-match nil))))

(defun visp-move-past-close-and-newline (close)
    (visp-move-past-close-and
     close
     (lambda ()
       (let ((comment-point (visp-find-comment-on-line)))
	 (newline)
	 (if comment-point
	     (save-excursion
	       (forward-line -1)
	       (end-of-line)
	       (indent-to (cdr comment-point))
	       (insert (car comment-point)))))
       (lisp-indent-line)
       (visp-ignore-sexp-errors (indent-sexp))
       (visp-blink-paren-match nil))))

(defun visp-move-past-close-and (close if-moved)
  (if (or (visp-in-string-p)
          (visp-in-comment-p))
      (insert close)
    (when (visp-in-char-p) (forward-char))
    (visp-move-past-close-and-reindent close)
    (funcall if-moved)))

;; (defun visp-find-comment-on-line ()
;;   "Find a margin comment on the current line.
;; Return nil if there is no such comment or if there is anything but
;;   whitespace until such a comment.
;; If such a comment exists, delete the comment (including all leading
;;   whitespace) and return a cons whose car is the comment as a string
;;   and whose cdr is the point of the comment's initial semicolon,
;;   relative to the start of the line."
;;   (save-excursion
;;     (visp-skip-whitespace t (point-at-eol))
;;     (and (eq ?\; (char-after))
;;          (not (eq ?\; (char-after (1+ (point)))))
;;          (not (or (visp-in-string-p)
;;                   (visp-in-char-p)))
;;          (let* ((start                  ;Move to before the semicolon.
;;                  (progn (backward-char) (point)))
;;                 (comment
;;                  (buffer-substring start (point-at-eol))))
;;            (visp-skip-whitespace nil (point-at-bol))
;;            (delete-region (point) (point-at-eol))
;;            (cons comment (- start (point-at-bol)))))))

(defun visp-find-comment-on-line ()
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "[^\\;];")
      (backward-char)
      (let ((pos (- (point) (point-at-bol)))
	    (comment (buffer-substring (point) (point-at-eol))))
	(delete-region (point) (point-at-eol))
	(cons comment pos)))))


(defun visp-insert-pair (n open close forward)
  (let* ((regionp
          (and (visp-region-active-p)
               (visp-region-safe-for-insert-p)))
         (end
          (and regionp
               (not n)
               (prog1 (region-end) (goto-char (region-beginning))))))
    (let ((spacep (visp-space-for-delimiter-p nil open)))
      (if spacep (insert " "))
      (insert open)
      (save-excursion
        ;; Move past the desired region.
        (cond (n
               (funcall forward
                        (visp-scan-sexps-hack (point)
                                                 (prefix-numeric-value n))))
              (regionp
               (funcall forward (+ end (if spacep 2 1)))))
        ;; The string case can happen if we are inserting string
        ;; delimiters.  The comment case may happen by moving to the
        ;; end of a buffer that has a comment with no trailing newline.
        (if (and (not (visp-in-string-p))
                 (visp-in-comment-p))
            (newline))
        (insert close)
        (if (visp-space-for-delimiter-p t close)
            (insert " "))))))

;++ This needs a better name...

(defun visp-scan-sexps-hack (point n)
  (save-excursion
    (goto-char point)
    (let ((direction (if (< 0 n) +1 -1))
          (magnitude (abs n))
          (count 0))
      (catch 'exit
        (while (< count magnitude)
          (let ((p
                 (visp-handle-sexp-errors (scan-sexps (point) direction)
                   nil)))
            (if (not p) (throw 'exit nil))
            (goto-char p))
          (setq count (+ count 1)))))
    (point)))

(defun visp-region-safe-for-insert-p ()
  (save-excursion
    (let ((beginning (region-beginning))
          (end (region-end)))
      (goto-char beginning)
      (let* ((beginning-state (visp-current-parse-state))
             (end-state
              (parse-partial-sexp beginning end nil nil beginning-state)))
        (and (=  (nth 0 beginning-state)   ; 0. depth in parens
                 (nth 0 end-state))
             (eq (nth 3 beginning-state)   ; 3. non-nil if inside a
                 (nth 3 end-state))        ;    string
             (eq (nth 4 beginning-state)   ; 4. comment status, yada
                 (nth 4 end-state))
             (eq (nth 5 beginning-state)   ; 5. t if following char
                 (nth 5 end-state)))))))   ;    quote

(defvar visp-space-for-delimiter-predicates nil
  "List of predicates for whether to put space by delimiter at point.
Each predicate is a function that is is applied to two arguments, ENDP
  and DELIMITER, and that returns a boolean saying whether to put a
  space next to the delimiter -- before/after the delimiter if ENDP is
  false/true, respectively.
If any predicate returns false, no space is inserted: every predicate
  has veto power.
Each predicate may assume that the point is not at the beginning/end of
  the buffer, and that the point is preceded/followed by a word
  constituent, symbol constituent, string quote, or delimiter matching
  DELIMITER, if ENDP is false/true, respectively.
Each predicate should examine only text before/after the point if ENDP is
  false/true, respectively.")

(defun visp-space-for-delimiter-p (endp delimiter)
  ;; If at the buffer limit, don't insert a space.  If there is a word,
  ;; symbol, other quote, or non-matching parenthesis delimiter (i.e. a
  ;; close when want an open the string or an open when we want to
  ;; close the string), do insert a space.
  (and (not (if endp (eobp) (bobp)))
       (memq (char-syntax (if endp (char-after) (char-before)))
             (list ?w ?_ ?\"
                   (let ((matching (matching-paren delimiter)))
                     (and matching (char-syntax matching)))
                   (and (not endp)
                        (eq ?\" (char-syntax delimiter))
                        ?\) )))
       (catch 'exit
         (dolist (predicate visp-space-for-delimiter-predicates)
           (if (not (funcall predicate endp delimiter))
               (throw 'exit nil)))
         t)))

(defun visp-move-past-close-and-reindent (close)
  (let ((open (visp-missing-close)))
    (when open
        (if (eq close (matching-paren open))
            (save-excursion
              (message "Missing closing delimiter: %c" close)
              (insert close))
            (error "Mismatched missing closing delimiter: %c ... %c"
                   open close))))
  (up-list)
  (if (catch 'return                    ; This CATCH returns T if it
        (while t                        ; should delete leading spaces
          (save-excursion               ; and NIL if not.
            (let ((before-paren (1- (point))))
              (back-to-indentation)
              (cond ((not (eq (point) before-paren))
                     ;; Can't call VISP-DELETE-LEADING-WHITESPACE
                     ;; here -- we must return from SAVE-EXCURSION
                     ;; first.
		     (lisp-indent-line)
                     (throw 'return t))
                    ((save-excursion (forward-line -1)
                                     (end-of-line)
                                     (visp-in-comment-p))
                     ;; Moving the closing delimiter any further
                     ;; would put it into a comment, so we just
                     ;; indent the closing delimiter where it is and
                     ;; abort the loop, telling its continuation that
                     ;; no leading whitespace should be deleted.
                     (lisp-indent-line)
                     (throw 'return nil))
                    (t (delete-indentation)))))))
      (visp-delete-leading-whitespace)))

(defun visp-missing-close ()
  (save-excursion
    (visp-handle-sexp-errors (backward-up-list)
      (error "Not inside a list."))
    (let ((open (char-after)))
      (visp-handle-sexp-errors (progn (forward-sexp) nil)
        open))))

(defun visp-delete-leading-whitespace ()
  ;; This assumes that we're on the closing delimiter already.
  (save-excursion
    (backward-char)
    (while (let ((syn (char-syntax (char-before))))
             (and (or (eq syn ?\ ) (eq syn ?-))     ; whitespace syntax
                  ;; The above line is a perfect example of why the
                  ;; following test is necessary.
                  (not (visp-in-char-p (1- (point))))))
      (delete-char -1))))

(defun visp-blink-paren-match (another-line-p)
  (if (and blink-matching-paren
           (or (not show-paren-mode) another-line-p))
      (visp-ignore-sexp-errors
        (save-excursion
          (backward-sexp)
          (forward-sexp)
          ;; SHOW-PAREN-MODE inhibits any blinking, so we disable it
          ;; locally here.
          (let ((show-paren-mode nil))
            (blink-matching-open))))))

(defun visp-doublequote (&optional n)
  "Insert a pair of double-quotes.
With a prefix argument N, wrap the following N S-expressions in
  double-quotes, escaping intermediate characters if necessary.
If the region is active, `transient-mark-mode' is enabled, and the
  region's start and end fall in the same parenthesis depth, insert a
  pair of double-quotes around the region, again escaping intermediate
  characters if necessary.
Inside a comment, insert a literal double-quote.
At the end of a string, move past the closing double-quote.
In the middle of a string, insert a backslash-escaped double-quote.
If in a character literal, do nothing.  This prevents accidentally
  changing a what was in the character literal to become a meaningful
  delimiter unintentionally."
  (interactive "P")
  (cond ((visp-in-string-p)
         (if (eq (point) (- (visp-enclosing-string-end) 1))
             (forward-char)             ; Just move past the closing quote.
           ;; Don't split a \x into an escaped backslash and a string end.
           (if (visp-in-string-escape-p) (forward-char))
           (insert ?\\ ?\" )))
        ((visp-in-comment-p)
         (insert ?\" ))
        ((not (visp-in-char-p))
         (visp-insert-pair n ?\" ?\" 'visp-forward-for-quote))))

(defun visp-meta-doublequote (&optional n)
  "Move to the end of the string.
If not in a string, act as `visp-doublequote'; if not prefix argument
 is specified and the region is not active or `transient-mark-mode' is
 disabled, the default is to wrap one S-expression, however, not zero."
  (interactive "P")
  (if (not (visp-in-string-p))
      (visp-doublequote (or n (and (not (visp-region-active-p)) 1)))
      (goto-char (visp-enclosing-string-end))))

(defun visp-meta-doublequote-and-newline (&optional n)
  "Move to the end of the string, insert a newline, and indent.
If not in a string, act as `visp-doublequote'; if not prefix argument
 is specified and the region is not active or `transient-mark-mode' is
 disabled, the default is to wrap one S-expression, however, not zero."
  (interactive "P")
  (if (not (visp-in-string-p))
      (visp-doublequote (or n (and (not (visp-region-active-p)) 1)))
      (progn (goto-char (visp-enclosing-string-end))
             (newline)
             (lisp-indent-line)
             (visp-ignore-sexp-errors (indent-sexp)))))

(defun visp-forward-for-quote (end)
  (let ((state (visp-current-parse-state)))
    (while (< (point) end)
      (let ((new-state (parse-partial-sexp (point) (1+ (point))
                                           nil nil state)))
        (if (visp-in-string-p new-state)
            (if (not (visp-in-string-escape-p))
                (setq state new-state)
              ;; Escape character: turn it into an escaped escape
              ;; character by appending another backslash.
              (insert ?\\ )
              ;; Now the point is after both escapes, and we want to
              ;; rescan from before the first one to after the second
              ;; one.
              (setq state
                    (parse-partial-sexp (- (point) 2) (point)
                                        nil nil state))
              ;; Advance the end point, since we just inserted a new
              ;; character.
              (setq end (1+ end)))
          ;; String: escape by inserting a backslash before the quote.
          (backward-char)
          (insert ?\\ )
          ;; The point is now between the escape and the quote, and we
          ;; want to rescan from before the escape to after the quote.
          (setq state
                (parse-partial-sexp (1- (point)) (1+ (point))
                                    nil nil state))
          ;; Advance the end point for the same reason as above.
          (setq end (1+ end)))))))

;;;; Escape Insertion

(defun visp-backslash ()
  "Insert a backslash followed by a character to escape."
  (interactive)
  (cond ((visp-in-string-p) (visp-backslash-interactive))
        ((visp-in-comment-p) (insert ?\\))
        ((visp-in-char-p) (forward-char) (visp-backslash-interactive))
        (t (visp-backslash-interactive))))

(defun visp-backslash-interactive ()
  (insert ?\\ )
  ;; Read a character to insert after the backslash.  If anything
  ;; goes wrong -- the user hits delete (entering the rubout
  ;; `character'), aborts with C-g, or enters non-character input
  ;; -- then delete the backslash to avoid a dangling escape.
  (let ((delete-p t))
    (unwind-protect
        (let ((char (read-char "Character to escape: " t)))
          (if (not (eq char ?\^?))
              (progn (message "Character to escape: %c" char)
                     (insert char)
                     (setq delete-p nil))))
      (if delete-p
          (progn (message "Deleting escape.")
                 (delete-char -1))))))

(defun visp-newline ()
  "Insert a newline and indent it.
This is like `newline-and-indent', but it not only indents the line
  that the point is on but also the S-expression following the point,
  if there is one.
Move forward one character first if on an escaped character.
If in a string, just insert a literal newline.
If in a comment and if followed by invalid structure, call
  `indent-new-comment-line' to keep the invalid structure in a
  comment."
  (interactive)
  (cond ((visp-in-string-p)
         (newline))
        ((visp-in-comment-p)
         (if (visp-region-ok-p (point) (point-at-eol))
             (progn (newline-and-indent)
                    (visp-ignore-sexp-errors (indent-sexp)))
             (indent-new-comment-line)))
        (t
         (if (visp-in-char-p)
             (forward-char))
         (newline-and-indent)
         ;; Indent the following S-expression, but don't signal an
         ;; error if there's only a closing delimiter after the point.
         (visp-ignore-sexp-errors (indent-sexp)))))

(defun visp-reindent-defun (&optional argument)
  "Reindent the definition that the point is on.
If the point is in a string or a comment, fill the paragraph instead,
  and with a prefix argument, justify as well."
  (interactive "P")
  (if (or (visp-in-string-p)
          (visp-in-comment-p))
      (if (memq fill-paragraph-function '(t nil))
          (lisp-fill-paragraph argument)
        (funcall fill-paragraph-function argument))
    (visp-preserving-column
      (save-excursion
        (end-of-defun)
        (beginning-of-defun)
        (indent-sexp)))))

;;;; Comment Insertion

(defun visp-semicolon (&optional n)
  "Insert a semicolon.
With a prefix argument N, insert N semicolons.
If in a string, do just that and nothing else.
If in a character literal, move to the beginning of the character
  literal before inserting the semicolon.
If the enclosing list ends on the line after the point, break the line
  after the last S-expression following the point.
If a list begins on the line after the point but ends on a different
  line, break the line after the last S-expression following the point
  before the list."
  (interactive "p")
  (if (or (visp-in-string-p) (visp-in-comment-p))
      (insert (make-string (or n 1) ?\; ))
    (if (visp-in-char-p)
        (backward-char 2))
    (let ((line-break-point (visp-semicolon-find-line-break-point)))
      (if line-break-point
          (visp-semicolon-with-line-break line-break-point (or n 1))
          (insert (make-string (or n 1) ?\; ))))))

(defun visp-semicolon-find-line-break-point ()
  (and (not (eolp))                   ;Implies (not (eobp)).
       (let ((eol (point-at-eol)))
         (save-excursion
           (catch 'exit
             (while t
               (let ((line-break-point (point)))
                 (cond ((visp-handle-sexp-errors (progn (forward-sexp) t)
                          nil)
                        ;; Successfully advanced by an S-expression.
                        ;; If that S-expression started on this line
                        ;; and ended on another one, break here.
                        (cond ((not (eq eol (point-at-eol)))
                               (throw 'exit
                                      (and (save-excursion
                                             (backward-sexp)
                                             (eq eol (point-at-eol)))
                                           line-break-point)))
                              ((eobp)
                               (throw 'exit nil))))
                       ((save-excursion
                          (visp-skip-whitespace t (point-at-eol))
                          (or (eolp) (eobp) (eq (char-after) ?\;)))
                        ;; Can't move further, but there's no closing
                        ;; delimiter we're about to clobber -- either
                        ;; it's on the next line or we're at the end of
                        ;; the buffer.  Don't break the line.
                        (throw 'exit nil))
                       (t
                        ;; Can't move because we hit a delimiter at the
                        ;; end of this line.  Break here.
                        (throw 'exit line-break-point))))))))))

(defun visp-semicolon-with-line-break (line-break-point n)
  (let ((line-break-marker (make-marker)))
    (set-marker line-break-marker line-break-point)
    (set-marker-insertion-type line-break-marker t)
    (insert (make-string (or n 1) ?\; ))
    (save-excursion
      (goto-char line-break-marker)
      (set-marker line-break-marker nil)
      (newline)
      (lisp-indent-line)
      ;; This step is redundant if we are inside a list, but even if we
      ;; are at the top level, we want at least to indent whatever we
      ;; bumped off the line.
      (visp-ignore-sexp-errors (indent-sexp))
      (visp-indent-sexps))))

;;; This is all a horrible, horrible hack, primarily for GNU Emacs 21,
;;; in which there is no `comment-or-uncomment-region'.

(autoload 'comment-forward "newcomment")
(autoload 'comment-normalize-vars "newcomment")
(autoload 'comment-region "newcomment")
(autoload 'comment-search-forward "newcomment")
(autoload 'uncomment-region "newcomment")

(defun visp-initialize-comment-dwim ()
  (require 'newcomment)
  (if (not (fboundp 'comment-or-uncomment-region))
      (defalias 'comment-or-uncomment-region
        (lambda (beginning end &optional argument)
          (interactive "*r\nP")
          (if (save-excursion (goto-char beginning)
                              (comment-forward (point-max))
                              (<= end (point)))
              (uncomment-region beginning end argument)
              (comment-region beginning end argument)))))
  (defalias 'visp-initialize-comment-dwim 'comment-normalize-vars)
  (comment-normalize-vars))

(defvar visp-comment-prefix-toplevel ";;; "
  "String of prefix for top-level comments aligned at the left margin.")

(defvar visp-comment-prefix-code ";; "
  "String of prefix for comments indented at the same depth as code.")

(defvar visp-comment-prefix-margin ";"
  "String of prefix for comments on the same line as code in the margin.")

(defun visp-comment-dwim (&optional argument)
  "Call the Lisp comment command you want (Do What I Mean).
This is like `comment-dwim', but it is specialized for Lisp editing.
If transient mark mode is enabled and the mark is active, comment or
  uncomment the selected region, depending on whether it was entirely
  commented not not already.
If there is already a comment on the current line, with no prefix
  argument, indent to that comment; with a prefix argument, kill that
  comment.
Otherwise, insert a comment appropriate for the context and ensure that
  any code following the comment is moved to the next line.
At the top level, where indentation is calculated to be at column 0,
  insert a triple-semicolon comment; within code, where the indentation
  is calculated to be non-zero, and on the line there is either no code
  at all or code after the point, insert a double-semicolon comment;
  and if the point is after all code on the line, insert a single-
  semicolon margin comment at `comment-column'."
  (interactive "*P")
  (visp-initialize-comment-dwim)
  (cond ((visp-region-active-p)
         (comment-or-uncomment-region (region-beginning)
                                      (region-end)
                                      argument))
        ((visp-comment-on-line-p)
         (if argument
             (comment-kill (if (integerp argument) argument nil))
             (comment-indent)))
        (t (visp-insert-comment))))

(defun visp-comment-on-line-p ()
  "True if there is a comment on the line following point.
This is expected to be called only in `visp-comment-dwim'; do not
  call it elsewhere."
  (save-excursion
    (beginning-of-line)
    (let ((comment-p nil))
      ;; Search forward for a comment beginning.  If there is one, set
      ;; COMMENT-P to true; if not, it will be nil.
      (while (progn
               (setq comment-p          ;t -> no error
                     (comment-search-forward (point-at-eol) t))
               (and comment-p
                    (or (visp-in-string-p)
                        (visp-in-char-p (1- (point))))))
        (forward-char))
      comment-p)))

(defun visp-insert-comment ()
  (let ((code-after-p
         (save-excursion (visp-skip-whitespace t (point-at-eol))
                         (not (eolp))))
        (code-before-p
         (save-excursion (visp-skip-whitespace nil (point-at-bol))
                         (not (bolp)))))
    (cond ((and (bolp)
                (let ((indent
                       (let ((indent (calculate-lisp-indent)))
                         (if (consp indent) (car indent) indent))))
                  (and indent (zerop indent))))
           ;; Top-level comment
           (if code-after-p (save-excursion (newline)))
           (insert visp-comment-prefix-toplevel))
          ((or code-after-p (not code-before-p))
           ;; Code comment
           (if code-before-p
               (newline-and-indent)
               (lisp-indent-line))
           (insert visp-comment-prefix-code)
           (if code-after-p
               (save-excursion
                 (newline)
                 (lisp-indent-line)
                 (visp-indent-sexps))))
          (t
           ;; Margin comment
           (indent-to comment-column 1) ; 1 -> force one leading space
           (insert visp-comment-prefix-margin)))))

;;;; Character Deletion

(defun visp-forward-delete (&optional argument)
  "Delete a character forward or move forward over a delimiter.
If on an opening S-expression delimiter, move forward into the
  S-expression.
If on a closing S-expression delimiter, refuse to delete unless the
  S-expression is empty, in which case delete the whole S-expression.
With a numeric prefix argument N, delete N characters forward.
With a `C-u' prefix argument, simply delete a character forward,
  without regard for delimiter balancing."
  (interactive "P")
  (cond ((or (consp argument) (eobp))
         (delete-char +1))
        ((integerp argument)
         (if (< argument 0)
             (visp-backward-delete argument)
             (while (> argument 0)
               (visp-forward-delete)
               (setq argument (- argument 1)))))
        ((visp-in-string-p)
         (visp-forward-delete-in-string))
        ((visp-in-comment-p)
         (visp-forward-delete-in-comment))
        ((visp-in-char-p)            ; Escape -- delete both chars.
         (delete-char -1)
         (delete-char +1))
        ((eq (char-after) ?\\ )         ; ditto
         (delete-char +2))
        ((let ((syn (char-syntax (char-after))))
           (or (eq syn ?\( )
               (eq syn ?\" )))
         (if (save-excursion
               (visp-handle-sexp-errors (progn (forward-sexp) t)
                 nil))
             (forward-char)
           (message "Deleting spurious opening delimiter.")
           (delete-char +1)))
        ((and (not (visp-in-char-p (1- (point))))
              (eq (char-syntax (char-after)) ?\) )
              (eq (char-before) (matching-paren (char-after))))
         (delete-char -1)               ; Empty list -- delete both
         (delete-char +1))              ;   delimiters.
        ((eq ?\; (char-after))
         (visp-forward-delete-comment-start))
        ((eq (char-syntax (char-after)) ?\) )
         (if (visp-handle-sexp-errors
                 (save-excursion (forward-char) (backward-sexp) t)
               nil)
             (message "End of list!")
             (progn
               (message "Deleting spurious closing delimiter.")
               (delete-char +1))))
        ;; Just delete a single character, if it's not a closing
        ;; delimiter.  (The character literal case is already handled
        ;; by now.)
        (t (delete-char +1))))

(defun visp-forward-delete-in-string ()
  (let ((start+end (visp-string-start+end-points)))
    (cond ((not (eq (point) (cdr start+end)))
           ;; If it's not the close-quote, it's safe to delete.  But
           ;; first handle the case that we're in a string escape.
           (cond ((visp-in-string-escape-p)
                  ;; We're right after the backslash, so backward
                  ;; delete it before deleting the escaped character.
                  (delete-char -1))
                 ((eq (char-after) ?\\ )
                  ;; If we're not in a string escape, but we are on a
                  ;; backslash, it must start the escape for the next
                  ;; character, so delete the backslash before deleting
                  ;; the next character.
                  (delete-char +1)))
           (delete-char +1))
          ((eq (1- (point)) (car start+end))
           ;; If it is the close-quote, delete only if we're also right
           ;; past the open-quote (i.e. it's empty), and then delete
           ;; both quotes.  Otherwise we refuse to delete it.
           (delete-char -1)
           (delete-char +1)))))

(defun visp-check-forward-delete-in-comment ()
  ;; Point is in a comment, possibly at eol.  We are about to delete
  ;; some characters forward; if we are at eol, we are about to delete
  ;; the line break.  Refuse to do so if if moving the next line into
  ;; the comment would break structure.
  (if (eolp)
      (let ((next-line-start (point-at-bol 2))
            (next-line-end (point-at-eol 2)))
        (visp-check-region next-line-start next-line-end))))

(defun visp-forward-delete-in-comment ()
  (visp-check-forward-delete-in-comment)
  (delete-char +1))

(defun visp-forward-delete-comment-start ()
  ;; Point precedes a comment start (not at eol).  Refuse to delete a
  ;; comment start if the comment contains unbalanced junk.
  (visp-check-region (+ (point) 1) (point-at-eol))
  (delete-char +1))

(defun visp-backward-delete (&optional argument)
  "Delete a character backward or move backward over a delimiter.
If on a closing S-expression delimiter, move backward into the
  S-expression.
If on an opening S-expression delimiter, refuse to delete unless the
  S-expression is empty, in which case delete the whole S-expression.
With a numeric prefix argument N, delete N characters backward.
With a `C-u' prefix argument, simply delete a character backward,
  without regard for delimiter balancing."
  (interactive "P")
  (cond ((or (consp argument) (bobp))
         ;++ Should this untabify?
         (delete-char -1))
        ((integerp argument)
         (if (< argument 0)
             (visp-forward-delete (- 0 argument))
             (while (> argument 0)
               (visp-backward-delete)
               (setq argument (- argument 1)))))
        ((visp-in-string-p)
         (visp-backward-delete-in-string))
        ((visp-in-comment-p)
         (visp-backward-delete-in-comment))
        ((visp-in-char-p)            ; Escape -- delete both chars.
         (delete-char -1)
         (delete-char +1))
        ((visp-in-char-p (1- (point)))
         (delete-char -2))              ; ditto
        ((let ((syn (char-syntax (char-before))))
           (or (eq syn ?\) )
               (eq syn ?\" )))
         (if (save-excursion
               (visp-handle-sexp-errors (progn (backward-sexp) t)
                 nil))
             (backward-char)
           (message "Deleting spurious closing delimiter.")
           (delete-char -1)))
        ((and (eq (char-syntax (char-before)) ?\( )
              (eq (char-after) (matching-paren (char-before))))
         (delete-char -1)               ; Empty list -- delete both
         (delete-char +1))              ;   delimiters.
        ((bolp)
         (visp-backward-delete-maybe-comment-end))
        ((eq (char-syntax (char-before)) ?\( )
         (if (visp-handle-sexp-errors
                 (save-excursion (backward-char) (forward-sexp) t)
               nil)
             (message "Beginning of list!")
             (progn
               (message "Deleting spurious closing delimiter.")
               (delete-char -1))))
        ;; Delete it, unless it's an opening delimiter.  The case of
        ;; character literals is already handled by now.
        (t
         ;; Turn off the @#&*&!^&(%^ botch in GNU Emacs 24 that changed
         ;; `backward-delete-char' and `backward-delete-char-untabify'
         ;; semantically so that they delete the region in transient
         ;; mark mode.
         (let ((delete-active-region nil))
           (backward-delete-char-untabify +1)))))

(defun visp-backward-delete-in-string ()
  (let ((start+end (visp-string-start+end-points)))
    (cond ((not (eq (1- (point)) (car start+end)))
           ;; If it's not the open-quote, it's safe to delete.
           (if (visp-in-string-escape-p)
               ;; If we're on a string escape, since we're about to
               ;; delete the backslash, we must first delete the
               ;; escaped char.
               (delete-char +1))
           (delete-char -1)
           (if (visp-in-string-escape-p)
               ;; If, after deleting a character, we find ourselves in
               ;; a string escape, we must have deleted the escaped
               ;; character, and the backslash is behind the point, so
               ;; backward delete it.
               (delete-char -1)))
          ((eq (point) (cdr start+end))
           ;; If it is the open-quote, delete only if we're also right
           ;; past the close-quote (i.e. it's empty), and then delete
           ;; both quotes.  Otherwise we refuse to delete it.
           (delete-char -1)
           (delete-char +1)))))

(defun visp-backward-delete-in-comment ()
  ;; Point is in a comment, possibly just after the comment start.
  ;; Refuse to delete a comment start if the comment contains
  ;; unbalanced junk.
  (if (save-excursion
        (backward-char)
        ;; Must call `visp-in-string-p' before
        ;; `visp-in-comment-p'.
        (not (or (visp-in-string-p) (visp-in-comment-p))))
      (visp-check-region (point) (point-at-eol)))
  (backward-delete-char-untabify +1))

(defun visp-backward-delete-maybe-comment-end ()
  ;; Point is at bol, possibly just after a comment end (i.e., the
  ;; previous line may have had a line comment).  Refuse to delete a
  ;; comment end if moving the current line into the previous line's
  ;; comment would break structure.
  (if (save-excursion
        (backward-char)
        (and (not (visp-in-string-p)) (visp-in-comment-p)))
      (visp-check-region (point-at-eol) (point-at-bol)))
  (delete-char -1))

;;;; Killing

(defun visp-kill (&optional argument)
  "Kill a line as if with `kill-line', but respecting delimiters.
In a string, act exactly as `kill-line' but do not kill past the
  closing string delimiter.
On a line with no S-expressions on it starting after the point or
  within a comment, act exactly as `kill-line'.
Otherwise, kill all S-expressions that start after the point.
With a `C-u' prefix argument, just do the standard `kill-line'.
With a numeric prefix argument N, do `kill-line' that many times."
  (interactive "P")
  (cond (argument
         (kill-line (if (integerp argument) argument 1)))
        ((visp-in-string-p)
         (visp-kill-line-in-string))
        ((visp-in-comment-p)
         (visp-kill-line-in-comment))
        ((save-excursion (visp-skip-whitespace t (point-at-eol))
                         (or (eolp) (eq (char-after) ?\; )))
         ;** Be careful about trailing backslashes.
         (if (visp-in-char-p)
             (backward-char))
         (kill-line))
        (t (visp-kill-sexps-on-line))))

(defun visp-kill-line-in-string ()
  (if (save-excursion (visp-skip-whitespace t (point-at-eol))
                      (eolp))
      (kill-line)
    (save-excursion
      ;; Be careful not to split an escape sequence.
      (if (visp-in-string-escape-p)
          (backward-char))
      (kill-region (point)
                   (min (point-at-eol)
                        (cdr (visp-string-start+end-points)))))))

(defun visp-kill-line-in-comment ()
  ;; The variable `kill-whole-line' is not relevant: the point is in a
  ;; comment, and hence not at the beginning of the line.
  (visp-check-forward-delete-in-comment)
  (kill-line))

(defun visp-kill-sexps-on-line ()
  (if (visp-in-char-p)               ; Move past the \ and prefix.
      (backward-char 2))                ; (# in Scheme/CL, ? in elisp)
  (let ((beginning (point))
        (eol (point-at-eol)))
    (let ((end-of-list-p (visp-forward-sexps-to-kill beginning eol)))
      ;; If we got to the end of the list and it's on the same line,
      ;; move backward past the closing delimiter before killing.  (This
      ;; allows something like killing the whitespace in (    ).)
      (if end-of-list-p (progn (up-list) (backward-char)))
      (if kill-whole-line
          (visp-kill-sexps-on-whole-line beginning)
        (kill-region beginning
                     ;; If all of the S-expressions were on one line,
                     ;; i.e. we're still on that line after moving past
                     ;; the last one, kill the whole line, including
                     ;; any comments; otherwise just kill to the end of
                     ;; the last S-expression we found.  Be sure,
                     ;; though, not to kill any closing parentheses.
                     (if (and (not end-of-list-p)
                              (eq (point-at-eol) eol))
                         eol
                         (point)))))))

;;; Please do not try to understand this code unless you have a VERY
;;; good reason to do so.  I gave up trying to figure it out well
;;; enough to explain it, long ago.

(defun visp-forward-sexps-to-kill (beginning eol)
  (let ((end-of-list-p nil)
        (firstp t))
    ;; Move to the end of the last S-expression that started on this
    ;; line, or to the closing delimiter if the last S-expression in
    ;; this list is on the line.
    (catch 'return
      (while t
        ;; This and the `kill-whole-line' business below fix a bug that
        ;; inhibited any S-expression at the very end of the buffer
        ;; (with no trailing newline) from being deleted.  It's a
        ;; bizarre fix that I ought to document at some point, but I am
        ;; too busy at the moment to do so.
        (if (and kill-whole-line (eobp)) (throw 'return nil))
        (save-excursion
          (visp-handle-sexp-errors (forward-sexp)
            (up-list)
            (setq end-of-list-p (eq (point-at-eol) eol))
            (throw 'return nil))
          (if (or (and (not firstp)
                       (not kill-whole-line)
                       (eobp))
                  (visp-handle-sexp-errors
                      (progn (backward-sexp) nil)
                    t)
                  (not (eq (point-at-eol) eol)))
              (throw 'return nil)))
        (forward-sexp)
        (if (and firstp
                 (not kill-whole-line)
                 (eobp))
            (throw 'return nil))
        (setq firstp nil)))
    end-of-list-p))

(defun visp-kill-sexps-on-whole-line (beginning)
  (kill-region beginning
               (or (save-excursion     ; Delete trailing indentation...
                     (visp-skip-whitespace t)
                     (and (not (eq (char-after) ?\; ))
                          (point)))
                   ;; ...or just use the point past the newline, if
                   ;; we encounter a comment.
                   (point-at-eol)))
  (cond ((save-excursion (visp-skip-whitespace nil (point-at-bol))
                         (bolp))
         ;; Nothing but indentation before the point, so indent it.
         (lisp-indent-line))
        ((eobp) nil)       ; Protect the CHAR-SYNTAX below against NIL.
        ;; Insert a space to avoid invalid joining if necessary.
        ((let ((syn-before (char-syntax (char-before)))
               (syn-after  (char-syntax (char-after))))
           (or (and (eq syn-before ?\) )            ; Separate opposing
                    (eq syn-after  ?\( ))           ;   parentheses,
               (and (eq syn-before ?\" )            ; string delimiter
                    (eq syn-after  ?\" ))           ;   pairs,
               (and (memq syn-before '(?_ ?w))      ; or word or symbol
                    (memq syn-after  '(?_ ?w)))))   ;   constituents.
         (insert " "))))

;;;;; Killing Words

;;; This is tricky and asymmetrical because backward parsing is
;;; extraordinarily difficult or impossible, so we have to implement
;;; killing in both directions by parsing forward.

(defun visp-forward-kill-word ()
  "Kill a word forward, skipping over intervening delimiters."
  (interactive)
  (let ((beginning (point)))
    (skip-syntax-forward " -")
    (let* ((parse-state (visp-current-parse-state))
           (state (visp-kill-word-state parse-state 'char-after)))
      (while (not (or (eobp)
                      (eq ?w (char-syntax (char-after)))))
        (setq parse-state
              (progn (forward-char 1) (visp-current-parse-state))
;;               (parse-partial-sexp (point) (1+ (point))
;;                                   nil nil parse-state)
              )
        (let* ((old-state state)
               (new-state
                (visp-kill-word-state parse-state 'char-after)))
          (cond ((not (eq old-state new-state))
                 (setq parse-state
                       (visp-kill-word-hack old-state
                                               new-state
                                               parse-state))
                 (setq state
                       (visp-kill-word-state parse-state
                                                'char-after))
                 (setq beginning (point)))))))
    (goto-char beginning)
    (kill-word 1)))

(defun visp-backward-kill-word ()
  "Kill a word backward, skipping over any intervening delimiters."
  (interactive)
  (if (not (or (bobp)
               (eq (char-syntax (char-before)) ?w)))
      (let ((end (point)))
        (backward-word 1)
        (forward-word 1)
        (goto-char (min end (point)))
        (let* ((parse-state (visp-current-parse-state))
               (state
                (visp-kill-word-state parse-state 'char-before)))
          (while (and (< (point) end)
                      (progn
                        (setq parse-state
                              (parse-partial-sexp (point) (1+ (point))
                                                  nil nil parse-state))
                        (or (eq state
                                (visp-kill-word-state parse-state
                                                         'char-before))
                            (progn (backward-char 1) nil)))))
          (if (and (eq state 'comment)
                   (eq ?\# (char-after (point)))
                   (eq ?\| (char-before (point))))
              (backward-char 1)))))
  (backward-kill-word 1))

;;;;;; Word-Killing Auxiliaries

(defun visp-kill-word-state (parse-state adjacent-char-fn)
  (cond ((visp-in-comment-p parse-state) 'comment)
        ((visp-in-string-p  parse-state) 'string)
        ((memq (char-syntax (funcall adjacent-char-fn))
               '(?\( ?\) ))
         'delimiter)
        (t 'other)))

;;; This optionally advances the point past any comment delimiters that
;;; should probably not be touched, based on the last state change and
;;; the characters around the point.  It returns a new parse state,
;;; starting from the PARSE-STATE parameter.

(defun visp-kill-word-hack (old-state new-state parse-state)
  (cond ((and (not (eq old-state 'comment))
              (not (eq new-state 'comment))
              (not (visp-in-string-escape-p))
              (eq ?\# (char-before))
              (eq ?\| (char-after)))
         (forward-char 1)
         (visp-current-parse-state)
;;          (parse-partial-sexp (point) (1+ (point))
;;                              nil nil parse-state)
         )
        ((and (not (eq old-state 'comment))
              (eq new-state 'comment)
              (eq ?\; (char-before)))
         (skip-chars-forward ";")
         (visp-current-parse-state)
;;          (parse-partial-sexp (point) (save-excursion
;;                                        (skip-chars-forward ";"))
;;                              nil nil parse-state)
         )
        (t parse-state)))

(defun visp-copy-as-kill ()
  "Save in the kill ring the region that `visp-kill' would kill."
  (interactive)
  (cond ((visp-in-string-p)
         (visp-copy-as-kill-in-string))
        ((visp-in-comment-p)
         (copy-region-as-kill (point) (point-at-eol)))
        ((save-excursion (visp-skip-whitespace t (point-at-eol))
                         (or (eolp) (eq (char-after) ?\; )))
         ;** Be careful about trailing backslashes.
         (save-excursion
           (if (visp-in-char-p)
               (backward-char))
           (copy-region-as-kill (point) (point-at-eol))))
        (t (visp-copy-sexps-as-kill))))

(defun visp-copy-as-kill-in-string ()
  (save-excursion
    (if (visp-in-string-escape-p)
        (backward-char))
    (copy-region-as-kill (point)
                         (min (point-at-eol)
                              (cdr (visp-string-start+end-points))))))

(defun visp-copy-sexps-as-kill ()
  (save-excursion
    (if (visp-in-char-p)
        (backward-char 2))
    (let ((beginning (point))
          (eol (point-at-eol)))
      (let ((end-of-list-p (visp-forward-sexps-to-kill beginning eol)))
        (if end-of-list-p (progn (up-list) (backward-char)))
        (copy-region-as-kill beginning
                             (cond (kill-whole-line
                                    (or (save-excursion
                                          (visp-skip-whitespace t)
                                          (and (not (eq (char-after) ?\; ))
                                               (point)))
                                        (point-at-eol)))
                                   ((and (not end-of-list-p)
                                         (eq (point-at-eol) eol))
                                    eol)
                                   (t
                                    (point))))))))

;;;; Deleting Regions

(defun visp-delete-region (start end)
  "Delete the text between point and mark, like `delete-region'.
If that text is unbalanced, signal an error instead.
With a prefix argument, skip the balance check."
  (interactive "r")
  (if (and start end (not current-prefix-arg))
      (visp-check-region-for-delete start end))
  (setq this-command 'delete-region)
  (delete-region start end))

(defun visp-kill-region (start end)
  "Kill the text between point and mark, like `kill-region'.
If that text is unbalanced, signal an error instead.
With a prefix argument, skip the balance check."
  (interactive "r")
  (if (and start end (not current-prefix-arg))
      (visp-check-region-for-delete start end))
  (setq this-command 'kill-region)
  (kill-region start end))

(defun visp-check-region-for-delete (start end)
  "Signal an error deleting text between START and END is unsafe."
  (save-excursion
    (goto-char start)
    (let* ((start-state (visp-current-parse-state))
           (end-state (parse-partial-sexp start end nil nil start-state)))
      (visp-check-region-for-delete:depth start start-state end end-state)
      (visp-check-region-for-delete:string start start-state end end-state)
      (visp-check-region-for-delete:comment start start-state end end-state)
      (visp-check-region-for-delete:char-quote start start-state
                                                  end end-state))))

(defun visp-check-region-for-delete:depth (start start-state end end-state)
  (let ((start-depth (nth 0 start-state))
        (end-depth (nth 0 end-state)))
    (if (not (= start-depth end-depth))
        (error "Mismatched parenthesis depth: %S at start, %S at end."
               start-depth
               end-depth))))

(defun visp-check-region-for-delete:string (start start-state end end-state)
  (let ((start-string-p (nth 3 start-state))
        (end-string-p (nth 3 end-state)))
    (if (not (eq start-string-p end-string-p))
        (error "Mismatched string state: start %sin string, end %sin string."
               (if start-string-p "" "not ")
               (if end-string-p "" "not ")))))

(defun visp-check-region-for-delete:comment
    (start start-state end end-state)
  (let ((start-comment-state (nth 4 start-state))
        (end-comment-state (nth 4 end-state)))
    (if (not (or (eq start-comment-state end-comment-state)
                 ;; If we are moving text into or out of a line
                 ;; comment, make sure that the text is balanced.  (The
                 ;; comment state may be a number, not t or nil at all,
                 ;; for nestable comments, which are not handled by
                 ;; this heuristic (or any of visp, really).)
                 (and (or (and (eq start-comment-state nil)
                               (eq end-comment-state t))
                          (and (eq start-comment-state t)
                               (eq end-comment-state nil)))
                      (save-excursion
                        (goto-char end)
                        (visp-region-ok-p (point) (point-at-eol))))))
        (error "Mismatched comment state: %s"
               (cond ((and (integerp start-comment-state)
                           (integerp end-comment-state))
                      (format "depth %S at start, depth %S at end."
                              start-comment-state
                              end-comment-state))
                     ((integerp start-comment-state)
                      "start in nested comment, end otherwise.")
                     ((integerp end-comment-state)
                      "end in nested comment, start otherwise.")
                     (start-comment-state
                      "start in comment, end not in comment.")
                     (end-comment-state
                      "end in comment, start not in comment.")
                     (t
                      (format "start %S, end %S."
                              start-comment-state
                              end-comment-state)))))))

(defun visp-check-region-for-delete:char-quote
    (start start-state end end-state)
  (let ((start-char-quote (nth 5 start-state))
        (end-char-quote (nth 5 end-state)))
    (if (not (eq start-char-quote end-char-quote))
        (let ((phrase "character quotation"))
          (error "Mismatched %s: start %sin %s, end %sin %s."
                 phrase
                 (if start-char-quote "" "not ")
                 phrase
                 (if end-char-quote "" "not ")
                 phrase)))))

;;;; Point Motion

(eval-and-compile
  (defmacro defun-motion (name bvl doc &rest body)
    `(defun ,name ,bvl
       ,doc
       ,(xcond ((visp-xemacs-p)
                '(interactive "_"))
               ((visp-gnu-emacs-p)
                ;++ Not sure this is sufficient for the `^'.
                (if (fboundp 'handle-shift-selection)
                    '(interactive "^p")
                    '(interactive "p"))))
       ,@body)))

(defun-motion visp-forward (&optional arg)
  "Move forward an S-expression, or up an S-expression forward.
If there are no more S-expressions in this one before the closing
  delimiter, move past that closing delimiter; otherwise, move forward
  past the S-expression following the point."
  (let ((n (or arg 1)))
    (cond ((< 0 n) (dotimes (i n)       (visp-move-forward)))
          ((< n 0) (dotimes (i (- n))   (visp-move-backward))))))

(defun-motion visp-backward (&optional arg)
  "Move backward an S-expression, or up an S-expression backward.
If there are no more S-expressions in this one before the opening
  delimiter, move past that opening delimiter backward; otherwise, move
  move backward past the S-expression preceding the point."
  (let ((n (or arg 1)))
    (cond ((< 0 n) (dotimes (i n)       (visp-move-backward)))
          ((< n 0) (dotimes (i (- n))   (visp-move-forward))))))

(defun visp-move-forward ()
  (cond ((visp-in-string-p)
         (let ((end (visp-enclosing-string-end)))
           ;; `forward-sexp' and `up-list' may move into the next string
           ;; in the buffer.  Don't do that; move out of the current one.
           (if (visp-handle-sexp-errors
                   (progn (visp-handle-sexp-errors (forward-sexp)
                            (up-list))
                          (<= end (point)))
                 t)
               (goto-char end))))
        ((visp-in-char-p)
         (forward-char))
        (t
         (visp-handle-sexp-errors (forward-sexp)
           (up-list)))))

(defun visp-move-backward ()
  (cond ((visp-in-string-p)
         (let ((start (visp-enclosing-string-start)))
           (if (visp-handle-sexp-errors
                   (progn (visp-handle-sexp-errors (backward-sexp)
                            (backward-up-list))
                          (<= (point) start))
                 t)
               (goto-char start))))
        ((visp-in-char-p)
         ;++ Corner case: a buffer of `\|x'.  What to do?
         (backward-char 2))
        (t
         (visp-handle-sexp-errors (backward-sexp)
           (backward-up-list)))))

;;;; Window Positioning

(defalias 'visp-recentre-on-sexp 'visp-recenter-on-sexp)

(defun visp-recenter-on-sexp (&optional n)
  "Recenter the screen on the S-expression following the point.
With a prefix argument N, encompass all N S-expressions forward."
  (interactive "P")
  (let* ((p (point))
         (end-point (progn (forward-sexp n) (point)))
         (start-point (progn (goto-char end-point) (backward-sexp n) (point))))
    ;; Point is at beginning of first S-expression.
    (let ((p-visible nil) (start-visible nil))
      (save-excursion
        (forward-line (/ (count-lines start-point end-point) 2))
        (recenter)
        (setq p-visible (pos-visible-in-window-p p))
        (setq start-visible (pos-visible-in-window-p start-point)))
      (cond ((not start-visible)
             ;; Implies (not p-visible).  Put the start at the top of
             ;; the screen.
             (recenter 0))
            (p-visible
             ;; Go back to p if we can.
             (goto-char p))))))

(defun visp-recenter-on-defun ()
  "Recenter the screen on the definition at point."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (visp-recenter-on-sexp)))

(defun visp-focus-on-defun ()
  "Moves display to the top of the definition at point."
  (interactive)
  (beginning-of-defun)
  (recenter 0))

;;;; Generalized Upward/Downward Motion

(defun visp-up/down (n vertical-direction)
  (let ((horizontal-direction (if (< 0 n) +1 -1)))
    (while (/= n 0)
      (goto-char
       (visp-next-up/down-point horizontal-direction vertical-direction))
      (setq n (- n horizontal-direction)))))

(defun visp-next-up/down-point (horizontal-direction vertical-direction)
  (let ((state (visp-current-parse-state))
        (scan-lists
         (lambda ()
           (scan-lists (point) horizontal-direction vertical-direction))))
    (cond ((visp-in-string-p state)
           (let ((start+end (visp-string-start+end-points state)))
             (if (< 0 vertical-direction)
                 (if (< 0 horizontal-direction)
                     (+ 1 (cdr start+end))
                     (car start+end))
                 ;; We could let the user try to descend into lists
                 ;; within the string, but that would be asymmetric
                 ;; with the up case, which rises out of the whole
                 ;; string and not just out of a list within the
                 ;; string, so this case will just be an error.
                 (error "Can't descend further into string."))))
          ((< 0 vertical-direction)
           ;; When moving up, just try to rise up out of the list.
           (or (funcall scan-lists)
               (buffer-end horizontal-direction)))
          ((< vertical-direction 0)
           ;; When moving down, look for a string closer than a list,
           ;; and use that if we find it.
           (let* ((list-start
                   (visp-handle-sexp-errors (funcall scan-lists) nil))
                  (string-start
                   (visp-find-next-string-start horizontal-direction
                                                   list-start)))
             (if (and string-start list-start)
                 (if (< 0 horizontal-direction)
                     (min string-start list-start)
                     (max string-start list-start))
                 (or string-start
                     ;; Scan again: this is a kludgey way to report the
                     ;; error if there really was one.
                     (funcall scan-lists)
                     (buffer-end horizontal-direction)))))
          (t
           (error "Vertical direction must be nonzero in `%s'."
                  'visp-up/down)))))

(defun visp-find-next-string-start (horizontal-direction limit)
  (let ((buffer-limit-p (if (< 0 horizontal-direction) 'eobp 'bobp))
        (next-char (if (< 0 horizontal-direction) 'char-after 'char-before))
        (pastp (if (< 0 horizontal-direction) '> '<)))
    (visp-handle-sexp-errors
        (save-excursion
          (catch 'exit
            (while t
              (if (or (funcall buffer-limit-p)
                      (and limit (funcall pastp (point) limit)))
                  (throw 'exit nil))
              (forward-sexp horizontal-direction)
              (save-excursion
                (backward-sexp horizontal-direction)
                (if (eq ?\" (char-syntax (funcall next-char)))
                    (throw 'exit (+ (point) horizontal-direction)))))))
      nil)))

(defun-motion visp-forward-down (&optional argument)
  "Move forward down into a list.
With a positive argument, move forward down that many levels.
With a negative argument, move backward down that many levels."
  (visp-up/down (or argument +1) -1))

(defun-motion visp-backward-up (&optional argument)
  "Move backward up out of the enclosing list.
With a positive argument, move backward up that many levels.
With a negative argument, move forward up that many levels.
If in a string initially, that counts as one level."
  (visp-up/down (- 0 (or argument +1)) +1))

(defun-motion visp-forward-up (&optional argument)
  "Move forward up out of the enclosing list.
With a positive argument, move forward up that many levels.
With a negative argument, move backward up that many levels.
If in a string initially, that counts as one level."
  (visp-up/down (or argument +1) +1))

(defun-motion visp-backward-down (&optional argument)
  "Move backward down into a list.
With a positive argument, move backward down that many levels.
With a negative argument, move forward down that many levels."
  (visp-up/down (- 0 (or argument +1)) -1))

;;;; Depth-Changing Commands:  Wrapping, Splicing, & Raising

(defun visp-wrap-sexp (&optional argument open close)
  "Wrap the following S-expression.
If a `C-u' prefix argument is given, wrap all S-expressions following
  the point until the end of the buffer or of the enclosing list.
If a numeric prefix argument N is given, wrap N S-expressions.
Automatically indent the newly wrapped S-expression.
As a special case, if the point is at the end of a list, simply insert
  a parenthesis pair, rather than inserting a lone opening delimiter
  and then signalling an error, in the interest of preserving
  structure.
By default OPEN and CLOSE are round delimiters."
  (interactive "P")
  (visp-lose-if-not-in-sexp 'visp-wrap-sexp)
  (let ((open (or open ?\( ))
        (close (or close ?\) )))
    (visp-handle-sexp-errors
        ((lambda (n) (visp-insert-pair n open close 'goto-char))
         (cond ((integerp argument) argument)
               ((consp argument) (visp-count-sexps-forward))
               ((visp-region-active-p) nil)
               (t 1)))
      (insert close)
      (backward-char)))
  (save-excursion (backward-up-list) (indent-sexp)))

(defun visp-yank-pop (&optional argument)
  "Replace just-yanked text with the next item in the kill ring.
If this command follows a `yank', just run `yank-pop'.
If this command follows a `visp-wrap-sexp', or any other visp
  wrapping command (see `visp-wrap-commands'), run `yank' and
  reindent the enclosing S-expression.
If this command is repeated, run `yank-pop' and reindent the enclosing
  S-expression.

The argument is passed on to `yank' or `yank-pop'; see their
  documentation for details."
  (interactive "*p")
  (cond ((eq last-command 'yank)
         (yank-pop argument))
        ((memq last-command visp-wrap-commands)
         (yank argument)
         ;; `yank' futzes with `this-command'.
         (setq this-command 'visp-yank-pop)
         (save-excursion (backward-up-list) (indent-sexp)))
        ((eq last-command 'visp-yank-pop)
         ;; Pretend we just did a `yank', so that we can use
         ;; `yank-pop' without duplicating its definition.
         (setq last-command 'yank)
         (yank-pop argument)
         ;; Return to our original state.
         (setq last-command 'visp-yank-pop)
         (setq this-command 'visp-yank-pop)
         (save-excursion (backward-up-list) (indent-sexp)))
        (t (error "Last command was not a yank or a wrap: %s" last-command))))

(defun visp-splice-sexp (&optional argument)
  "Splice the list that the point is on by removing its delimiters.
With a prefix argument as in `C-u', kill all S-expressions backward in
  the current list before splicing all S-expressions forward into the
  enclosing list.
With two prefix arguments as in `C-u C-u', kill all S-expressions
  forward in the current list before splicing all S-expressions
  backward into the enclosing list.
With a numerical prefix argument N, kill N S-expressions backward in
  the current list before splicing the remaining S-expressions into the
  enclosing list.  If N is negative, kill forward.
Inside a string, unescape all backslashes, or signal an error if doing
  so would invalidate the buffer's structure."
  (interactive "P")
  (if (visp-in-string-p)
      (visp-splice-string argument)
    (if (visp-in-comment-p)
        (error "Can't splice comment."))
    (visp-handle-sexp-errors (visp-enclosing-list-start)
      (error "Can't splice top level."))
    (visp-kill-surrounding-sexps-for-splice argument)
    (let ((delete-start (visp-enclosing-list-start))
          (delete-end
           (let ((limit
                  (save-excursion
                    (visp-ignore-sexp-errors (forward-sexp) (backward-sexp))
                    (point))))
             (save-excursion
               (backward-up-list)
               (forward-char +1)
               (visp-skip-whitespace t limit)
               (point)))))
      (let ((end-marker (make-marker)))
        (save-excursion
          (up-list)
          (delete-char -1)
          (set-marker end-marker (point)))
        (delete-region delete-start delete-end)
        (visp-splice-reindent delete-start (marker-position end-marker))))))

(defun visp-splice-reindent (start end)
  (visp-preserving-column
    ;; If we changed the first subform of the enclosing list, we must
    ;; reindent the whole enclosing list.
    (if (visp-handle-sexp-errors
            (save-excursion
              (backward-up-list)
              (down-list)
              (visp-ignore-sexp-errors (forward-sexp))
              (< start (point)))
          nil)
        (save-excursion (backward-up-list) (indent-sexp))
        (visp-indent-region start end))))

(defun visp-kill-surrounding-sexps-for-splice (argument)
  (cond ((or (visp-in-string-p)
             (visp-in-comment-p))
         (error "Invalid context for splicing S-expressions."))
        ((or (not argument) (eq argument 0)) nil)
        ((or (numberp argument) (eq argument '-))
         ;; Kill S-expressions before/after the point by saving the
         ;; point, moving across them, and killing the region.
         (let* ((argument (if (eq argument '-) -1 argument))
                (saved (visp-point-at-sexp-boundary (- argument))))
           (goto-char saved)
           (visp-ignore-sexp-errors (backward-sexp argument))
           (visp-hack-kill-region saved (point))))
        ((consp argument)
         (let ((v (car argument)))
           (if (= v 4)                  ;One `C-u'.
               ;; Move backward until we hit the open paren; then
               ;; kill that selected region.
               (let ((end (point)))
                 (visp-ignore-sexp-errors
                   (while (not (bobp))
                     (backward-sexp)))
                 (visp-hack-kill-region (point) end))
               ;; Move forward until we hit the close paren; then
               ;; kill that selected region.
               (let ((beginning (point)))
                 (visp-ignore-sexp-errors
                   (while (not (eobp))
                     (forward-sexp)))
                 (visp-hack-kill-region beginning (point))))))
        (t (error "Bizarre prefix argument `%s'." argument))))

(defun visp-splice-sexp-killing-backward (&optional n)
  "Splice the list the point is on by removing its delimiters, and
  also kill all S-expressions before the point in the current list.
With a prefix argument N, kill only the preceding N S-expressions."
  (interactive "P")
  (visp-splice-sexp (if n
                           (prefix-numeric-value n)
                           '(4))))

(defun visp-splice-sexp-killing-forward (&optional n)
  "Splice the list the point is on by removing its delimiters, and
  also kill all S-expressions after the point in the current list.
With a prefix argument N, kill only the following N S-expressions."
  (interactive "P")
  (visp-splice-sexp (if n
                           (- (prefix-numeric-value n))
                           '(16))))

(defun visp-raise-sexp (&optional argument)
  "Raise the following S-expression in a tree, deleting its siblings.
With a prefix argument N, raise the following N S-expressions.  If N
  is negative, raise the preceding N S-expressions.
If the point is on an S-expression, such as a string or a symbol, not
  between them, that S-expression is considered to follow the point."
  (interactive "P")
  (save-excursion
    (cond ((visp-in-string-p)
           (goto-char (car (visp-string-start+end-points))))
          ((visp-in-char-p)
           (backward-sexp))
          ((visp-in-comment-p)
           (error "No S-expression to raise in comment.")))
    ;; Select the S-expressions we want to raise in a buffer substring.
    (let* ((n (prefix-numeric-value argument))
           (bound (scan-sexps (point) n))
           (sexps
            (if (< n 0)
                (buffer-substring bound (visp-point-at-sexp-end))
                (buffer-substring (visp-point-at-sexp-start) bound))))
      ;; Move up to the list we're raising those S-expressions out of and
      ;; delete it.
      (backward-up-list)
      (delete-region (point) (scan-sexps (point) 1))
      (let* ((indent-start (point))
             (indent-end (save-excursion (insert sexps) (point))))
        ;; If the expression spans multiple lines, its indentation is
        ;; probably broken, so reindent it -- but don't reindent
        ;; anything that we didn't touch outside the expression.
        ;;
        ;; XXX What if the *column* of the starting point was preserved
        ;; too?  Should we avoid reindenting in that case?
        (if (not (eq (save-excursion (goto-char indent-start) (point-at-eol))
                     (save-excursion (goto-char indent-end) (point-at-eol))))
            (indent-region indent-start indent-end nil))))))

;;; The effects of convolution on the surrounding whitespace are pretty
;;; random.  If you have better suggestions, please let me know.

(defun visp-convolute-sexp (&optional n)
  "Convolute S-expressions.
Save the S-expressions preceding point and delete them.
Splice the S-expressions following point.
Wrap the enclosing list in a new list prefixed by the saved text.
With a prefix argument N, move up N lists before wrapping."
  (interactive "p")
  (visp-lose-if-not-in-sexp 'visp-convolute-sexp)
  ;; Make sure we can move up before destroying anything.
  (save-excursion (backward-up-list n) (backward-up-list))
  (let (open close)                     ;++ Is this a good idea?
    (let ((prefix
           (let ((end (point)))
             (visp-ignore-sexp-errors
               (while (not (bobp)) (backward-sexp)))
             (prog1 (buffer-substring (point) end)
               (backward-up-list)
               (save-excursion (forward-sexp)
                               (setq close (char-before))
                               (delete-char -1))
               (setq open (char-after))
               (delete-region (point) end)
               ;; I'm not sure this makes sense...
               (if (not (eolp)) (just-one-space))))))
      (backward-up-list n)
      (visp-insert-pair 1 open close 'goto-char)
      (insert prefix)
      ;; I'm not sure this makes sense either...
      (if (not (eolp)) (just-one-space))
      (save-excursion
        (backward-up-list)
        (visp-ignore-sexp-errors (indent-sexp))))))

(defun visp-splice-string (argument)
  (let ((original-point (point))
        (start+end (visp-string-start+end-points)))
    (let ((start (car start+end))
          (end (cdr start+end)))
      ;; START and END both lie before the respective quote
      ;; characters, which we want to delete; thus we increment START
      ;; by one to extract the string, and we increment END by one to
      ;; delete the string.
      (let* ((escaped-string
              (cond ((not (consp argument))
                     (buffer-substring (1+ start) end))
                    ((= 4 (car argument))
                     (buffer-substring original-point end))
                    (t
                     (buffer-substring (1+ start) original-point))))
             (unescaped-string
              (visp-unescape-string escaped-string)))
        (if (not unescaped-string)
            (error "Unspliceable string.")
          (save-excursion
            (goto-char start)
            (delete-region start (1+ end))
            (insert unescaped-string))
          (if (not (and (consp argument)
                        (= 4 (car argument))))
              (goto-char (- original-point 1))))))))

(defun visp-unescape-string (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (and (not (eobp))
                ;; nil -> no bound; t -> no errors.
                (search-forward "\\" nil t))
      (delete-char -1)
      (forward-char))
    (visp-handle-sexp-errors
        (progn (scan-sexps (point-min) (point-max))
               (buffer-string))
      nil)))

;;;; Slurpage & Barfage

(defun visp-forward-slurp-sexp (&optional argument)
  "Add the S-expression following the current list into that list
  by moving the closing delimiter.
Automatically reindent the newly slurped S-expression with respect to
  its new enclosing form.
If in a string, move the opening double-quote forward by one
  S-expression and escape any intervening characters as necessary,
  without altering any indentation or formatting."
  (interactive "P")
  (save-excursion
    (cond ((visp-in-comment-p)
           (error "Invalid context for slurping S-expressions."))
          ((numberp argument)
           (if (< argument 0)
               (visp-forward-barf-sexp (- 0 argument))
               (while (< 0 argument)
                 (visp-forward-slurp-sexp)
                 (setq argument (- argument 1)))))
          ((visp-in-string-p)
           ;; If there is anything to slurp into the string, take that.
           ;; Otherwise, try to slurp into the enclosing list.
           (if (save-excursion
                 (goto-char (visp-enclosing-string-end))
                 (visp-handle-sexp-errors (progn (forward-sexp) nil)
                   t))
               (progn
                 (goto-char (visp-enclosing-string-end))
                 (visp-forward-slurp-into-list argument))
               (visp-forward-slurp-into-string argument)))
          (t
           (visp-forward-slurp-into-list argument)))))

(defun visp-forward-slurp-into-list (&optional argument)
  (let ((nestedp nil))
    (save-excursion
      (up-list)                            ; Up to the end of the list to
      (let ((close (char-before)))         ;   save and delete the closing
        (delete-char -1)                   ;   delimiter.
        (let ((start (point)))
          (catch 'return                   ; Go to the end of the desired
            (while t                       ;   S-expression, going up a
              (visp-handle-sexp-errors  ;   list if it's not in this,
                  (progn (forward-sexp)
                         (if argument
                             (visp-ignore-sexp-errors
                               (while (not (eobp))
                                 (forward-sexp))))
                         (throw 'return nil))
                (setq nestedp t)
                (up-list)
                (setq close                ; adjusting for mixed
                      (prog1 (char-before) ;   delimiters as necessary,
                        (delete-char -1)
                        (insert close))))))
          (insert close)                   ;  to insert that delimiter.
          (indent-region start (point) nil))))
    (if (and (not nestedp)
             (eq (save-excursion (visp-skip-whitespace nil) (point))
                 (save-excursion (backward-up-list) (forward-char) (point)))
             (eq (save-excursion (forward-sexp) (backward-sexp) (point))
                 (save-excursion (visp-skip-whitespace t) (point))))
        (delete-region (save-excursion (visp-skip-whitespace nil) (point))
                       (save-excursion (visp-skip-whitespace t) (point))))))

(defun visp-forward-slurp-into-string (&optional argument)
  (let ((start (visp-enclosing-string-start))
        (end (visp-enclosing-string-end)))
    (goto-char end)
    ;; Signal any errors that we might get first, before mucking with
    ;; the buffer's contents.
    (save-excursion (forward-sexp))
    (let ((close (char-before)))
      ;; Skip intervening whitespace if we're slurping into an empty
      ;; string.  XXX What about nonempty strings?
      (if (and (= (+ start 2) end)
               (eq (save-excursion (visp-skip-whitespace t) (point))
                   (save-excursion (forward-sexp) (backward-sexp) (point))))
          (delete-region (- (point) 1)
                         (save-excursion (visp-skip-whitespace t) (point)))
          (delete-char -1))
      (visp-forward-for-quote
       (save-excursion
         (forward-sexp)
         (if argument
             (while (visp-handle-sexp-errors (progn (forward-sexp) t) nil)))
         (point)))
      (insert close))))

(defun visp-forward-barf-sexp (&optional argument)
  "Remove the last S-expression in the current list from that list
  by moving the closing delimiter.
Automatically reindent the newly barfed S-expression with respect to
  its new enclosing form."
  (interactive "P")
  (visp-lose-if-not-in-sexp 'visp-forward-barf-sexp)
  (if (and (numberp argument) (< argument 0))
      (visp-forward-slurp-sexp (- 0 argument))
    (let ((start (point)) (end nil))
      (save-excursion
        (up-list)                       ; Up to the end of the list to
        (let ((close (char-before)))    ;   save and delete the closing
          (delete-char -1)              ;   delimiter.
          (setq end (point))
          (visp-ignore-sexp-errors   ; Go back to where we want to
            (if (or (not argument)      ;   insert the delimiter.
                    (numberp argument))
                (backward-sexp argument)
                (while (visp-handle-sexp-errors
                           (save-excursion (backward-sexp) (<= start (point)))
                         nil)
                  (backward-sexp))))
          (visp-skip-whitespace nil) ; Skip leading whitespace.
          (cond ((bobp)
                 ;++ We'll have deleted the close, but there's no open.
                 ;++ Is that OK?
                 (error "Barfing all subexpressions with no open-paren?"))
                ((visp-in-comment-p) ; Don't put the close-paren in
                 (newline)))            ;   a comment.
          (insert close))
        ;; Reindent all of the newly barfed S-expressions.  Start at the
        ;; start of the first barfed S-expression, not at the close we
        ;; just inserted.
        (forward-sexp)
        (backward-sexp)
        (if (or (not argument) (numberp argument))
            (visp-forward-and-indent argument)
            (indent-region (point) end))))))

(defun visp-backward-slurp-sexp (&optional argument)
  "Add the S-expression preceding the current list into that list
  by moving the closing delimiter.
Automatically reindent the whole form into which new S-expression was
  slurped.
If in a string, move the opening double-quote backward by one
  S-expression and escape any intervening characters as necessary,
  without altering any indentation or formatting."
  (interactive "P")
  (save-excursion
    (cond ((visp-in-comment-p)
           (error "Invalid context for slurping S-expressions."))
          ((numberp argument)
           (if (< argument 0)
               (visp-backward-barf-sexp (- 0 argument))
               (while (< 0 argument)
                 (visp-backward-slurp-sexp)
                 (setq argument (- argument 1)))))
          ((visp-in-string-p)
           ;; If there is anything to slurp into the string, take that.
           ;; Otherwise, try to slurp into the enclosing list.
           (if (save-excursion
                 (goto-char (visp-enclosing-string-start))
                 (visp-handle-sexp-errors (progn (backward-sexp) nil)
                   t))
               (progn
                 (goto-char (visp-enclosing-string-start))
                 (visp-backward-slurp-into-list argument))
               (visp-backward-slurp-into-string argument)))
          (t
           (visp-backward-slurp-into-list argument)))))

(defun visp-backward-slurp-into-list (&optional argument)
  (let ((nestedp nil))
    (save-excursion
      (backward-up-list)
      (let ((open (char-after)))
        (delete-char +1)
        (catch 'return
          (while t
            (visp-handle-sexp-errors
                (progn (backward-sexp)
                       (if argument
                           (visp-ignore-sexp-errors
                             (while (not (bobp))
                               (backward-sexp))))
                       (throw 'return nil))
              (setq nestedp t)
              (backward-up-list)
              (setq open
                    (prog1 (char-after)
                      (save-excursion (insert open) (delete-char +1)))))))
        (insert open))
      ;; Reindent the line at the beginning of wherever we inserted the
      ;; opening delimiter, and then indent the whole S-expression.
      (backward-up-list)
      (lisp-indent-line)
      (indent-sexp))
    ;; If we slurped into an empty list, don't leave dangling space:
    ;; (foo |).
    (if (and (not nestedp)
             (eq (save-excursion (visp-skip-whitespace nil) (point))
                 (save-excursion (backward-sexp) (forward-sexp) (point)))
             (eq (save-excursion (up-list) (backward-char) (point))
                 (save-excursion (visp-skip-whitespace t) (point))))
        (delete-region (save-excursion (visp-skip-whitespace nil) (point))
                       (save-excursion (visp-skip-whitespace t) (point))))))

(defun visp-backward-slurp-into-string (&optional argument)
  (let ((start (visp-enclosing-string-start))
        (end (visp-enclosing-string-end)))
    (goto-char start)
    ;; Signal any errors that we might get first, before mucking with
    ;; the buffer's contents.
    (save-excursion (backward-sexp))
    (let ((open (char-after))
          (target (point)))
      ;; Skip intervening whitespace if we're slurping into an empty
      ;; string.  XXX What about nonempty strings?
      (if (and (= (+ start 2) end)
               (eq (save-excursion (visp-skip-whitespace nil) (point))
                   (save-excursion (backward-sexp) (forward-sexp) (point))))
          (delete-region (save-excursion (visp-skip-whitespace nil) (point))
                         (+ (point) 1))
          (delete-char +1))
      (backward-sexp)
      (if argument
          (visp-ignore-sexp-errors
            (while (not (bobp))
              (backward-sexp))))
      (insert open)
      (visp-forward-for-quote target))))

(defun visp-backward-barf-sexp (&optional argument)
  "Remove the first S-expression in the current list from that list
  by moving the closing delimiter.
Automatically reindent the barfed S-expression and the form from which
  it was barfed."
  (interactive "P")
  (visp-lose-if-not-in-sexp 'visp-backward-barf-sexp)
  (if (and (numberp argument) (< argument 0))
      (visp-backward-slurp-sexp (- 0 argument))
    (let ((end (make-marker)))
      (set-marker end (point))
      (save-excursion
        (backward-up-list)
        (let ((open (char-after)))
          (delete-char +1)
          (visp-ignore-sexp-errors
            (visp-forward-and-indent
             (if (or (not argument) (numberp argument))
                 argument
                 (let ((n 0))
                   (save-excursion
                     (while (visp-handle-sexp-errors
                                (save-excursion
                                  (forward-sexp)
                                  (<= (point) end))
                              nil)
                       (forward-sexp)
                       (setq n (+ n 1))))
                   n))))
          (while (progn (visp-skip-whitespace t) (eq (char-after) ?\; ))
            (forward-line 1))
          (if (eobp)
              ;++ We'll have deleted the close, but there's no open.
              ;++ Is that OK?
              (error "Barfing all subexpressions with no close-paren?"))
          ;** Don't use `insert' here.  Consider, e.g., barfing from
          ;**   (foo|)
          ;** and how `save-excursion' works.
          (insert-before-markers open))
        (backward-up-list)
        (lisp-indent-line)
        (indent-sexp)))))

;;;; Splitting & Joining

(defun visp-split-sexp ()
  "Split the list or string the point is on into two."
  (interactive)
  (cond ((visp-in-string-p)
         (insert "\"")
         (save-excursion (insert " \"")))
        ((or (visp-in-comment-p)
             (visp-in-char-p))
         (error "Invalid context for splitting S-expression."))
        (t
         (let ((open (save-excursion (backward-up-list) (char-after)))
               (close (save-excursion (up-list) (char-before))))
           (delete-horizontal-space)
           (insert close)
           (save-excursion
             (insert ?\ )
             (insert open)
             (backward-char)
             (indent-sexp))))))

(defun visp-join-sexps ()
  "Join the S-expressions adjacent on either side of the point.
Both must be lists, strings, or atoms; error if there is a mismatch."
  (interactive)
  (cond ((visp-in-comment-p) (error "Can't join S-expressions in comment."))
        ((visp-in-string-p) (error "Nothing to join in a string."))
        ((visp-in-char-p) (error "Can't join characters.")))
  (let ((left-point (visp-point-at-sexp-end))
        (right-point (visp-point-at-sexp-start)))
    (let ((left-char (char-before left-point))
          (right-char (char-after right-point)))
      (let ((left-syntax (char-syntax left-char))
            (right-syntax (char-syntax right-char)))
        (cond ((< right-point left-point)
               (error "Can't join a datum with itself."))
              ((and (eq left-syntax ?\) )
                    (eq right-syntax ?\( )
                    (eq left-char (matching-paren right-char))
                    (eq right-char (matching-paren left-char)))
               (visp-join-lists-internal left-point right-point)
               (visp-preserving-column
                 (save-excursion
                   (backward-up-list)
                   (indent-sexp))))
              ((and (eq left-syntax ?\" )
                    (eq right-syntax ?\" ))
               ;; Delete any intermediate formatting.
               (delete-region (1- left-point) (1+ right-point)))
              ((and (memq left-syntax '(?w ?_)) ; Word or symbol
                    (memq right-syntax '(?w ?_)))
               (delete-region left-point right-point))
              (t (error "Mismatched S-expressions to join.")))))))

(defun visp-join-lists-internal (left-point right-point)
  (save-excursion
    ;; Leave intermediate formatting alone.
    (goto-char right-point)
    (delete-char +1)
    (goto-char left-point)
    (delete-char -1)
    ;; Kludge: Add an extra space in several conditions.
    (if (or
         ;; (foo)| ;x\n(bar) => (foo | ;x\nbar), not (foo|  ;x\nbar).
         (and (not (eolp))
              (save-excursion
                (visp-skip-whitespace t (point-at-eol))
                (eq (char-after) ?\;)))
         ;; (foo)|(bar) => (foo| bar), not (foo|bar).
         (and (= left-point right-point)
              (not (or (eq ?\  (char-syntax (char-before)))
                       (eq ?\  (char-syntax (char-after)))))))
        (insert ?\  ))))

;++ How ought visp-join to handle comments intervening symbols or strings?
;++ Idea:
;++
;++   "foo"   |        ;bar
;++   "baz"      ;quux
;++
;++ =>
;++
;++   "foo|baz"       ;bar
;++              ;quux
;++
;++ The point should stay where it is relative to the comments, and the
;++ the comments' columns should all be preserved, perhaps.  Hmmmm...
;++ What about this?
;++
;++   "foo"           ;bar
;++       |           ;baz
;++   "quux"          ;zot

;++ Should rename:
;++     visp-point-at-sexp-start     -> visp-start-of-sexp-after-point
;++     visp-point-at-sexp-end       -> visp-end-of-sexp-before-point

;;;; Variations on the Lurid Theme

;;; I haven't the imagination to concoct clever names for these.

(defun visp-add-to-previous-list ()
  "Add the S-expression following point to the list preceding point."
  (interactive)
  (visp-lose-if-not-in-sexp 'visp-add-to-previous-list)
  (save-excursion
    (down-list -1)                      ;++ backward-down-list...
    (visp-forward-slurp-sexp)))

(defun visp-add-to-next-list ()
  "Add the S-expression preceding point to the list following point.
If no S-expression precedes point, move up the tree until one does."
  (interactive)
  (visp-lose-if-not-in-sexp 'visp-add-to-next-list)
  (save-excursion
    (down-list)
    (visp-backward-slurp-sexp)))

(defun visp-join-with-previous-list ()
  "Join the list the point is on with the previous list in the buffer."
  (interactive)
  (visp-lose-if-not-in-sexp 'visp-join-with-previous-list)
  (save-excursion
    (while (visp-handle-sexp-errors (save-excursion (backward-sexp) nil)
             (backward-up-list)
             t))
    (visp-join-sexps)))

(defun visp-join-with-next-list ()
  "Join the list the point is on with the next list in the buffer."
  (interactive)
  (visp-lose-if-not-in-sexp 'visp-join-with-next-list)
  (save-excursion
    (while (visp-handle-sexp-errors (save-excursion (forward-sexp) nil)
             (up-list)
             t))
    (visp-join-sexps)))

;;;; Utilities

(defun visp-in-string-escape-p ()
  "True if the point is on a character escape of a string.
This is true only if the character is preceded by an odd number of
  backslashes.
This assumes that `visp-in-string-p' has already returned true."
  (let ((oddp nil))
    (save-excursion
      (while (eq (char-before) ?\\ )
        (setq oddp (not oddp))
        (backward-char)))
    oddp))

(defun visp-in-char-p (&optional position)
  "True if point is on a character escape outside a string."
  (save-excursion
    (goto-char (or position (point)))
    (visp-in-string-escape-p)))

(defun visp-skip-whitespace (trailing-p &optional limit)
  "Skip past any whitespace, or until the point LIMIT is reached.
If TRAILING-P is nil, skip leading whitespace; otherwise, skip trailing
  whitespace."
  (funcall (if trailing-p 'skip-chars-forward 'skip-chars-backward)
           " \t\n"  ; This should skip using the syntax table, but LF
           limit))    ; is a comment end, not newline, in Lisp mode.

(defalias 'visp-region-active-p
  (xcond ((visp-xemacs-p) 'region-active-p)
         ((visp-gnu-emacs-p)
          (lambda ()
            (and mark-active transient-mark-mode)))))

(defun visp-hack-kill-region (start end)
  "Kill the region between START and END.
Do not append to any current kill, and
 do not let the next kill append to this one."
  (interactive "r")                     ;Eh, why not?
  ;; KILL-REGION sets THIS-COMMAND to tell the next kill that the last
  ;; command was a kill.  It also checks LAST-COMMAND to see whether it
  ;; should append.  If we bind these locally, any modifications to
  ;; THIS-COMMAND will be masked, and it will not see LAST-COMMAND to
  ;; indicate that it should append.
  (let ((this-command nil)
        (last-command nil))
    (kill-region start end)))

;;;;; Reindentation utilities

;++ Should `visp-indent-sexps' and `visp-forward-and-indent' use
;++ `visp-indent-region' rather than `indent-region'?

(defun visp-indent-sexps ()
  "If in a list, indent all following S-expressions in the list."
  (let* ((start (point))
         (end (visp-handle-sexp-errors (progn (up-list) (point)) nil)))
    (if end
        (indent-region start end nil))))

(defun visp-forward-and-indent (&optional n)
  "Move forward by N S-expressions, indenting them with `indent-region'."
  (let ((start (point)))
    (forward-sexp n)
    (indent-region start (point) nil)))

(defun visp-indent-region (start end)
  "Indent the region from START to END.
Don't reindent the line starting at START, however."
  (if (not (<= start end))
      (error "Incorrectly related points: %S, %S" start end))
  (save-excursion
    (goto-char start)
    (let ((bol (point-at-bol)))
      ;; Skip all S-expressions that end on the starting line, but
      ;; don't go past `end'.
      (if (and (save-excursion (goto-char end) (not (eq bol (point-at-bol))))
               (visp-handle-sexp-errors
                   (catch 'exit
                     (while t
                       (save-excursion
                         (forward-sexp)
                         (if (not (eq bol (point-at-bol)))
                             (throw 'exit t))
                         (if (not (< (point) end))
                             (throw 'exit nil)))
                       (forward-sexp)))
                 nil))
          (progn
            ;; Point is still on the same line, but precedes an
            ;; S-expression that ends on a different line.
            (if (not (eq bol (point-at-bol)))
                (error "Internal error -- we moved forward a line!"))
            (goto-char (+ 1 (point-at-eol)))
            (if (not (<= (point) end))
                (error "Internal error -- we frobnitzed the garfnut!"))
            (indent-region (point) end nil))))))

;;;;; S-expression Parsing Utilities

;++ These routines redundantly traverse S-expressions a great deal.
;++ If performance issues arise, this whole section will probably have
;++ to be refactored to preserve the state longer, like visp.scm
;++ does, rather than to traverse the definition N times for every key
;++ stroke as it presently does.

(defun visp-current-parse-state ()
  "Return parse state of point from beginning of defun."
  (let ((point (point)))
    (beginning-of-defun)
    ;; Calling PARSE-PARTIAL-SEXP will advance the point to its second
    ;; argument (unless parsing stops due to an error, but we assume it
    ;; won't in visp-mode).
    (parse-partial-sexp (point) point)))

(defun visp-in-string-p (&optional state)
  "True if the parse state is within a double-quote-delimited string.
If no parse state is supplied, compute one from the beginning of the
  defun to the point."
  ;; 3. non-nil if inside a string (the terminator character, really)
  (and (nth 3 (or state (visp-current-parse-state)))
       t))

(defun visp-string-start+end-points (&optional state)
  "Return a cons of the points of open and close quotes of the string.
The string is determined from the parse state STATE, or the parse state
  from the beginning of the defun to the point.
This assumes that `visp-in-string-p' has already returned true, i.e.
  that the point is already within a string."
  (save-excursion
    ;; 8. character address of start of comment or string; nil if not
    ;;    in one
    (let ((start (nth 8 (or state (visp-current-parse-state)))))
      (goto-char start)
      (forward-sexp 1)
      (cons start (1- (point))))))

(defun visp-enclosing-string-start ()
  (car (visp-string-start+end-points)))

(defun visp-enclosing-string-end ()
  (+ 1 (cdr (visp-string-start+end-points))))

(defun visp-enclosing-list-start ()
  (save-excursion
    (backward-up-list)
    (point)))

(defun visp-enclosing-list-end ()
  (save-excursion
    (up-list)
    (point)))

(defun visp-in-comment-p (&optional state)
  "True if parse state STATE is within a comment.
If no parse state is supplied, compute one from the beginning of the
  defun to the point."
  ;; 4. nil if outside a comment, t if inside a non-nestable comment,
  ;;    else an integer (the current comment nesting)
  (and (nth 4 (or state (visp-current-parse-state)))
       t))

(defun visp-prefix-numeric-value (argument)
  ;++ Kludgerific.
  (cond ((integerp argument) argument)
        ((eq argument '-) -1)
        ((consp argument)
         (cond ((equal argument '(4)) (visp-count-sexps-forward))   ;C-u
               ((equal argument '(16)) (visp-count-sexps-backward)) ;C-u C-u
               (t (error "Invalid prefix argument: %S" argument))))
        ((visp-region-active-p)
         (save-excursion
           (save-restriction
             (narrow-to-region (region-beginning) (region-end))
             (cond ((= (point) (point-min)) (visp-count-sexps-forward))
                   ((= (point) (point-max)) (visp-count-sexps-backward))
                   (t
                    (error "Point %S is not start or end of region: %S..%S"
                           (point) (region-beginning) (region-end)))))))
        (t 1)))

(defun visp-count-sexps-forward ()
  (save-excursion
    (let ((n 0) (p nil))                ;hurk
      (visp-ignore-sexp-errors
        (while (setq p (scan-sexps (point) +1))
          (goto-char p)
          (setq n (+ n 1))))
      n)))

(defun visp-count-sexps-backward ()
  (save-excursion
    (let ((n 0) (p nil))                ;hurk
      (visp-ignore-sexp-errors
        (while (setq p (scan-sexps (point) -1))
          (goto-char p)
          (setq n (+ n 1))))
      n)))

(defun visp-point-at-sexp-boundary (n)
  (cond ((< n 0) (visp-point-at-sexp-start))
        ((= n 0) (point))
        ((> n 0) (visp-point-at-sexp-end))))

(defun visp-point-at-sexp-start ()
  (save-excursion
    (forward-sexp)
    (backward-sexp)
    (point)))

(defun visp-point-at-sexp-end ()
  (save-excursion
    (backward-sexp)
    (forward-sexp)
    (point)))

(defun visp-lose-if-not-in-sexp (command)
  (if (or (visp-in-string-p)
          (visp-in-comment-p)
          (visp-in-char-p))
      (error "Invalid context for command `%s'." command)))

(defun visp-check-region (start end)
  "Signal an error if text between `start' and `end' is unbalanced."
  ;; `narrow-to-region' will move the point, so avoid calling it if we
  ;; don't need to.  We don't want to use `save-excursion' because we
  ;; want the point to move if `check-parens' reports an error.
  (if (not (visp-region-ok-p start end))
      (save-restriction
        (narrow-to-region start end)
        (check-parens))))

(defun visp-region-ok-p (start end)
  "Return true iff the region between `start' and `end' is balanced.
This is independent of context -- it doesn't check what state the
  text at `start' is in."
  (save-excursion
    (visp-handle-sexp-errors
        (progn
          (save-restriction
            (narrow-to-region start end)
            (scan-sexps (point-min) (point-max)))
          t)
      nil)))

(defun visp-current-indentation ()
  (save-excursion
    (back-to-indentation)
    (current-column)))

(defun visp-restore-column (column indentation)
  ;; Preserve the point's position either in the indentation or in the
  ;; code: if on code, move with the code; if in indentation, leave it
  ;; in the indentation, either where it was (if still on indentation)
  ;; or at the end of the indentation (if the code moved far enough
  ;; left).
  (let ((indentation* (visp-current-indentation)))
    (goto-char
     (+ (point-at-bol)
        (cond ((not (< column indentation))
               (+ column (- indentation* indentation)))
              ((<= indentation* column) indentation*)
              (t column))))))

;;;; Initialization

(visp-define-keys)
;; (visp-annotate-mode-with-examples)
;; (visp-annotate-functions-with-examples)

(provide 'visp)

;;; Local Variables:
;;; outline-regexp: "\n;;;;+"
;;; End:

;;; visp.el ends here
