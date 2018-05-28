(require 'lispyville)

(setq lispyville-key-theme '(operators
                             s-operators
                             prettify
                             c-w
                             additional-movement
                             slurp/barf-cp
                             additional
                             additional-insert
                             mark-toggle))
(lispyville-set-key-theme)

(setq evil-move-cursor-back nil)

(defmacro lispyville-with (in &rest body)
  "This is `lispy-with' modified for lispyville.
Note that | is considered to be \"on\" a character in normal/visual state,
meaning that it is included in a visual selection. ~ on the other hand is not
considered to be on a character, so when it represents the region end, the
character after it is not considered as part of the region."
  (declare (indent 1))
  `(let ((temp-buffer (generate-new-buffer " *temp*")))
     (save-window-excursion
       (unwind-protect
           (progn
             (switch-to-buffer temp-buffer)
             (emacs-lisp-mode)
             (transient-mark-mode 1)
             (evil-mode)
             (lispy-mode)
             (lispyville-mode)
             (insert ,in)
             (goto-char (point-min))
             (when (search-forward "~" nil t)
               (backward-delete-char 1)
               (set-mark (point)))
             (goto-char (point-max))
             (search-backward "|")
             (delete-char 1)
             (setq current-prefix-arg nil)
             ,@(mapcar (lambda (x)
                         (if (stringp x)
                             `(evil-execute-macro 1 (kbd ,x))
                           x))
                       body)
             (insert "|")
             (when (region-active-p)
               (exchange-point-and-mark)
               ;; because not considering ~ as "on" like |
               (when (and (evil-visual-state-p)
                          (= (point) (region-end)))
                 (forward-char))
               (insert "~"))
             (buffer-substring-no-properties
              (point-min)
              (point-max)))
         (and (buffer-name temp-buffer)
              (kill-buffer temp-buffer))))))

(defun lispyville-replace-with-last-kill ()
  "Replace buffer with last kill."
  (delete-region (point-min) (point-max))
  (yank)
  (goto-char (point-max)))

;;; Operators
(ert-deftest lispyville-yank ()
  (should (string= (lispyville-with "(|a)"
                     "yW"
                     (lispyville-replace-with-last-kill))
                   "a|"))
  ;; visual mode
  (should (string= (lispyville-with "(~a { b [ c \"tes|ting\"]})"
                     "y"
                     (lispyville-replace-with-last-kill))
                   "a  b  c test|"))
  (should (string= (lispyville-with "(~a { b [ c \"testing\"]})|"
                     "y"
                     (lispyville-replace-with-last-kill))
                   "a { b [ c \"testing\"]}|"))
  ;; linewise (always ends with newline)
  (should (string= (lispyville-with "((\n  |(a b)))"
                     "yy"
                     (lispyville-replace-with-last-kill))
                   "  (a b)\n|"))
  (should (string= (lispyville-with "(\n  |(a b)\n  (c d))"
                     "2yy"
                     (lispyville-replace-with-last-kill))
                   "  (a b)\n  (c d)\n|"))
  ;; test that works at end of buffer
  (should (string= (lispyville-with "|(a b)"
                     "yy"
                     (lispyville-replace-with-last-kill))
                   "(a b)\n|"))
  ;; test yank handler/pasting after linewise yank
  (should (string= (lispyville-with "((\n  |(a b)))"
                     "yyp")
                   "((\n  (a b)))\n  |(a b)"))
  (should (string= (lispyville-with "((\n  |(a b)))"
                     "yyP")
                   "((\n  |(a b)\n  (a b)))"))
  ;; visual block mode
  (should (string= (lispyville-with "((~a b)\n (c d|))"
                     "C-v y"
                     (lispyville-replace-with-last-kill))
                   "a b\nc d|"))
  (should (string= (lispyville-with "~(a b)\n(|c d)"
                     "C-v y"
                     (lispyville-replace-with-last-kill))
                   "a\nc|"))
  ;; test yank handler/pasting after yanking block
  (should (string= (lispyville-with "~(a b)\n(|c d)"
                     "C-v y p")
                   "(|aa b)\n(cc d)")))

(ert-deftest lispyville-yank-line ()
  (should (string= (lispyville-with "(|a (b c) d)"
                     "Y"
                     (lispyville-replace-with-last-kill))
                   "a (b c) d|"))
  ;; counts; 2Y doesn't work normally with evil actually
  ;; probably not that useful either
  ;; (should (string= (lispyville-with "(\n  |(a b)\n  (c d))"
  ;;                    "2Y"
  ;;                    (lispyville-replace-with-last-kill))
  ;;                  "  (a b)\n  (c d)|"))
  ;; visual
  (should (string= (lispyville-with "((\n  ~|(a b)))"
                     "Yp")
                   "((\n  (a b)))\n  |(a b)"))
  (should (string= (lispyville-with "((\n  ~|(a b)))"
                     "YP")
                   "((\n  |(a b)\n  (a b)))"))
  ;; visual block
  (should (string= (lispyville-with "((~a b)\n (c d|))"
                     "C-v Y"
                     (lispyville-replace-with-last-kill))
                   "a b\nc d|"))
  (should (string= (lispyville-with "~(a b)\n(|c d)"
                     "C-v Y"
                     (lispyville-replace-with-last-kill))
                   "a\nc|")))

(ert-deftest lispyville-delete ()
  (should (string= (lispyville-with "(|a)"
                     "dW")
                   "(|)"))
  ;; visual mode
  (should (string= (lispyville-with "(~a { b [ c \"tes|ting\"]})"
                     "d")
                   "(|{[\"ing\"]})"))
  (should (string= (lispyville-with "(~a { b [ c \"testing\"]})|"
                     "d")
                   "(|)"))
  ;; linewise
  ;; after deletion, at closing delimiter(s)
  (should (string= (lispyville-with "((\n  |(a b)))"
                     "dd")
                   "|(())"))
  (should (string= (lispyville-with "((\n  |(a b)))\n\nfoo"
                     "dd")
                   ;; shouldn't delete more than one newline
                   "(())\n|\nfoo"))
  (should (string= (lispyville-with "(\n |(a b)\n (c d))"
                     "2dd")
                   "|()"))
  (should (string= (lispyville-with "(\n |(a b)\n (c d))\n\nfoo"
                     ;; shouldn't delete extra newline
                     "2dd")
                   "()\n|\nfoo"))
  ;; closing delimiter(s) but comment before
  (should (string= (lispyville-with "((;; comment\n  |(a b)))"
                     "dd")
                   "((;; comment\n  |))"))
  (should (string= (lispyville-with "((;; comment\n  |(a b)))\n\nfoo"
                     "dd")
                   ;; should still delete a newline
                   "((;; comment\n  |))\nfoo"))
  (should (string= (lispyville-with "((;; comment\n  |(a b)))\nfoo"
                     "dd")
                   ;; shouldn't delete a newline if it will pull a sexp up
                   "((;; comment\n  |))\nfoo"))
  ;; sexp after closing
  (should (string= (lispyville-with "(let ((a 1)\n      |(b 2))\n  (foo a b))"
                     "dd")
                   "(let ((a 1))\n  |(foo a b))"))
  ;; after deletion, at opening delimiter(s)
  (should (string= (lispyville-with "|(a b\n   c)"
                     "dd")
                   ;; should remove whitespace
                   "|(c)"))
  ;; should delete final newline
  (should (string= (lispyville-with "a\n|"
                     "dd")
                   "|a"))
  ;; test that works at end of buffer
  (should (string= (lispyville-with "|(a b)"
                     "dd")
                   "|"))
  (should (string= (lispyville-with "a\n|b"
                     "dd")
                   "|a"))
  (should (string= (lispyville-with "(a\n|b)"
                     "dd")
                   "|(a)"))
  ;; testing re-positioning
  (should (string= (lispyville-with "\"multi-line\n|string\""
                     "dd")
                   "|\"multi-line\""))
  (should (string= (lispyville-with "(a\n |(b c)\n (d e))"
                     "dd")
                   "(a\n |(d e))"))
  ;; visual block mode
  (should (string= (lispyville-with "((~a b)\n (c d|))"
                     "C-v d")
                   "((|)\n ())"))
  (should (string= (lispyville-with "~(a b)\n(|c d)"
                     "C-v d")
                   "|( b)\n( d)"))
  ;; test whether delimiters are pulled into comments
  (should (string= (lispyville-with "(a\n ;; b\n |c)"
                     "dd")
                   "(a\n ;; b\n |)"))
  (should (string= (lispyville-with "(a\n ;;~ b\n c)|"
                     "d")
                   "(a\n ;;|\n)"))
  (should (string= (lispyville-with ";; ~a\n(b\n ;;| c\n d)"
                     "d")
                   ";; |\n(c\n d)"))
  ;; should pull into comment here since no unmatched delimiters
  (should (string= (lispyville-with ";; ~a\nb| c"
                     "d")
                   ";; |c")))

(ert-deftest lispyville-delete-line ()
  (should (string= (lispyville-with "(|a (b c) d)"
                     "D")
                   "(|)"))
  ;; visual (act like dd)
  (should (string= (lispyville-with "((\n  ~|(a b)))"
                     "D")
                   "|(())"))
  (should (string= (lispyville-with "((\n  ~|(a b)))\n"
                     "D")
                   "(())\n|"))
  ;; visual block
  (should (string= (lispyville-with "((~a b)\n (c d|))"
                     "C-v D")
                   "((|)\n ())"))
  (should (string= (lispyville-with "~(a b)\n(|c d)"
                     "C-v D")
                   "|( b)\n( d)")))

(ert-deftest lispyville-delete-char-or-splice ()
  (should (string= (lispyville-with "(|a)"
                     "x")
                   "(|)"))
  (should (string= (lispyville-with "|(a)"
                     "x")
                   "|a"))
  ;; errors without space for some reason
  ;; doesn't happen in my emacs
  (should (string= (lispyville-with "(a|) "
                     "x")
                   "a| "))
  ;; visual mode
  (should (string= (lispyville-with "~(|a\n (b c))"
                     "x")
                   "|\n (b c)"))
  ;; visual block mode
  ;; should have same behavior as d in this case
  (should (string= (lispyville-with "((~a b)\n (c |d))"
                     "C-v x")
                   "((|)\n ())"))
  ;; splice here
  (should (string= (lispyville-with "~(a b)\n(|c d)"
                     "C-v x")
                   "| b\n d")))

(ert-deftest lispyville-delete-char-or-splice-backwards ()
  (should (string= (lispyville-with "(a|)"
                     "X")
                   "(|)"))
  (should (string= (lispyville-with "(|a)"
                     "X")
                   "|a"))
  (should (string= (lispyville-with "(a)| "
                     "X")
                   "a| "))
  ;; visual mode
  ;; should behave the same as x here
  (should (string= (lispyville-with "~(|a\n (b c))"
                     "X")
                   "|\n (b c)"))
  ;; visual block mode
  ;; should have same behavior as d in this case
  (should (string= (lispyville-with "((~a b)\n (c |d))"
                     "C-v X")
                   "((|)\n ())"))
  ;; splice here; should behave the same as x
  (should (string= (lispyville-with "~(a b)\n(|c d)"
                     "C-v X")
                   "| b\n d")))

(ert-deftest lispyville-delete-backward-word ()
  (should (string= (lispyville-with "(foo bar|)"
                     "i C-w")
                   "(foo |)"))
  (should (string= (lispyville-with "(|a)"
                     "i C-w")
                   "|"))
  (should (string= (lispyville-with "(a)|"
                     "i C-w")
                   "|"))
  (should (string= (lispyville-with "\"a\"|"
                     "i C-w")
                   "|")))

(ert-deftest lispyville-change ()
  ;; linewise; unlike dd, cc should not delete newlines
  (should (string= (lispyville-with "|foo\n"
                     "cc")
                   "|\n"))
  (should (string= (lispyville-with "(((a b)\n  |(c d)\n  (e f)))"
                     "cc")
                   "(((a b)\n  |\n  (e f)))"))
  ;; before closing unmatched delimiter(s)
  (should (string= (lispyville-with "((\n  |(a b)))\n"
                     "cc")
                   "((\n  |))\n"))
  (should (string= (lispyville-with "(let ((a 1)\n      |(b 2))\n  (foo a b))\n"
                     "cc")
                   "(let ((a 1)\n      |)\n  (foo a b))\n"))
  ;; 2cc should delete one newline
  (should (string= (lispyville-with "(\n  |(a b)\n  (c d))\n"
                     "2cc")
                   "(\n |)\n"))
  ;; before opening unmatched delimiters(s)
  (should (string= (lispyville-with "a\n|(b\n c)\n"
                     "cc")
                   "a\n(|\n c)\n"))
  (should (string= (lispyville-with "a\n(b|\n c)\n"
                     "cc")
                   "a\n(|\n c)\n"))
  ;; test that works at end of buffer
  (should (string= (lispyville-with "|(a b)"
                     "cc")
                   "|\n"))
  ;; should not pull up opening delimiter
  (should (string= (lispyville-with "a|\n(b)\n"
                     "cc")
                   "|\n(b)\n"))
  ;; test that undo is one step (evil-want-fine-undo nil)
  (should (string= (lispyville-with "(defvar foo\n  bar baz)|"
                     (concat "cchello" "ESC" "u"))
                   "(defvar foo\n  bar baz)|")))

;; pretty much already tested
;; (ert-deftest lispyville-change-line ()
;;   )

(ert-deftest lispyville-change-whole-line ()
  (should (string= (lispyville-with "((\n  |(a b)))\n"
                     "S")
                   "((\n  |))\n")))

(ert-deftest lispyville-substitute ()
  (should (string= (lispyville-with "|(a)"
                     "s")
                   "|(a)"))
  (should (string= (lispyville-with "(|a)"
                     "s")
                   "(|)")))

(ert-deftest lispyville-prettify ()
  ;; should work in the basic case
  (should (string= (lispyville-with "(foo\n |a\n )"
                     "=G")
                   "(foo\n |a)")))

;;; Commands/Motions
(ert-deftest lispyville-first-non-blank ()
  (should (string= (lispyville-with "|   a"
                     (lispyville-first-non-blank))
                   "   |a"))
  ;; should not go past the ending quote
  (should (string= (lispyville-with "\"a string with a newline \n|  \""
                     (lispyville-first-non-blank))
                   "\"a string with a newline \n  |\""))
  (should (string= (lispyville-with "|   ([{\"a string\"}])"
                     (lispyville-first-non-blank))
                   "   ([{\"|a string\"}])")))

(ert-deftest lispyville-forward-sexp ()
  (should (string= (lispyville-with "(|a b c)"
                     "L")
                   "(a| b c)"))
  (should (string= (lispyville-with "(|a b c)"
                     "2L")
                   "(a b| c)"))
  (should (string= (lispyville-with "(|a b c)"
                     "3L")
                   "(a b c|)"))
  (should (string= (lispyville-with "(~|a b c)"
                     "3L")
                   "(~a b c|)"))
  (should (string= (lispyville-with "(|a b (c e d))"
                     "3L")
                   "(a b (c e d)|)")))

(ert-deftest lispyville-backward-sexp ()
  (should (string= (lispyville-with "(a b c|)"
                     "H")
                   "(a b |c)"))
  (should (string= (lispyville-with "(a b c|)"
                     "2H")
                   "(a |b c)"))
  (should (string= (lispyville-with "(a b c|)"
                     "3H")
                   "(|a b c)"))
  (should (string= (lispyville-with "(a b |c d~)"
                     "2H")
                   "(|a b c d~)"))
  (should (string= (lispyville-with "(a b (c e d)|)"
                     "3H")
                   "(|a b (c e d))")))

(ert-deftest lispyville-beginning-of-defun ()
  (should (string= (lispyville-with "(a |b c)"
                     "M-h")
                   "|(a b c)")))

(ert-deftest lispyville-end-of-defun ()
  (should (string= (lispyville-with "(a |b c)"
                     "M-l")
                   "(a b c|)"))
  (let ((lispyville-motions-put-into-special t))
    (should (string= (lispyville-with "(a |b c)"
                       "M-l")
                     "(a b c)|"))))

(ert-deftest lispyville-next-opening ()
  (should (string= (lispyville-with "|(((a b c)))"
                     "{")
                   "(|((a b c)))"))
  (should (string= (lispyville-with "(|((a b c)))"
                     "{")
                   "((|(a b c)))"))
  (should (string= (lispyville-with "|(((a b c)))"
                     "2{")
                   "((|(a b c)))"))
  (should (string= (lispyville-with "~|(((a b c)))"
                     "{")
                   "~(|((a b c)))"))
  (should (string= (lispyville-with "~(|((a b c)))"
                     "{")
                   "~((|(a b c)))"))
  (should (string= (lispyville-with "~|(((a b c)))"
                     "2{")
                   "~((|(a b c)))"))
  (should (string= (lispyville-with "|a\nb\n(c)"
                     "{")
                   "a\nb\n|(c)"))
  (should (string= (lispyville-with "\"|\\\"{[(a)]}\\\"\" (b)"
                     "{")
                   "\"\\\"{[(a)]}\\\"\" |(b)"))
  (should (string= (lispyville-with "|;; ([{\n(a)"
                     "{")
                   ";; ([{\n|(a)"))
  (should (string= (lispyville-with "|a\n;; ("
                     "{")
                   "|a\n;; (")))

(ert-deftest lispyville-previous-opening ()
  (should (string= (lispyville-with "(((a |b c)))"
                     "[")
                   "((|(a b c)))"))
  (should (string= (lispyville-with "((|(a b c)))"
                     "[")
                   "(|((a b c)))"))
  (should (string= (lispyville-with "(|((a b c)))"
                     "[")
                   "|(((a b c)))"))
  (should (string= (lispyville-with "(((a |b c)))"
                     "2[")
                   "(|((a b c)))"))
  (should (string= (lispyville-with "(((a |b c)))"
                     "3[")
                   "|(((a b c)))"))
  (should (string= (lispyville-with "(a)\nb\nc|"
                     "[")
                   "|(a)\nb\nc"))
  (should (string= (lispyville-with "\"\\\"{[(a)]}\\\"\" |(b)"
                     "[")
                   "|\"\\\"{[(a)]}\\\"\" (b)"))
  (should (string= (lispyville-with "(a)\n;; ([{|"
                     "[")
                   "|(a)\n;; ([{"))
  (should (string= (lispyville-with ";; (\n|a"
                     "[")
                   ";; (\n|a")))

(ert-deftest lispyville-next-closing ()
  (should (string= (lispyville-with "|(((a b c)))"
                     "]")
                   "(((a b c|)))"))
  (should (string= (lispyville-with "(((a b c|)))"
                     "]")
                   "(((a b c)|))"))
  (should (string= (lispyville-with "(((a b c|)))"
                     "]")
                   "(((a b c)|))"))
  (should (string= (lispyville-with "(((a b c)|))"
                     "]")
                   "(((a b c))|)"))
  (should (string= (lispyville-with "|(((a b c)))"
                     "3]")
                   "(((a b c))|)"))
  (should (string= (lispyville-with "|a\nb\n(c)"
                     "]")
                   "a\nb\n(c|)"))
  (should (string= (lispyville-with "\"|\\\"{[(a)]}\\\"\""
                     "]")
                   "\"\\\"{[(a)]}\\\"|\""))
  (should (string= (lispyville-with "|;; )]}\n(a)"
                     "]")
                   ";; )]}\n(a|)"))
  (should (string= (lispyville-with "|a\n;; )"
                     "]")
                   "|a\n;; )"))
  (let ((lispyville-motions-put-into-special t))
    (should (string= (lispyville-with "(a |b c)"
                       "]")
                     "(a b c)|"))
    (should (string= (lispyville-with "\"|a b\""
                       "]")
                     "\"a b|\""))))

(ert-deftest lispyville-previous-closing ()
  (should (string= (lispyville-with "(((a b c)))|"
                     "}")
                   "(((a b c))|)"))
  (should (string= (lispyville-with "(((a b c))|)"
                     "}")
                   "(((a b c)|))"))
  (should (string= (lispyville-with "(((a b c)|))"
                     "}")
                   "(((a b c|)))"))
  (should (string= (lispyville-with "(((a b c)))|"
                     "3}")
                   "(((a b c|)))"))
  (should (string= (lispyville-with "(a)\nb\nc|"
                     "}")
                   "(a|)\nb\nc"))
  (should (string= (lispyville-with "(a) \"\\\"{[(b)]}\\\"|\""
                     "}")
                   "(a|) \"\\\"{[(b)]}\\\"\""))
  (should (string= (lispyville-with "(a)\n;; }])|"
                     "}")
                   "(a|)\n;; }])"))
  (should (string= (lispyville-with ";; )\n|a"
                     "}")
                   ";; )\n|a"))
  (let ((lispyville-motions-put-into-special t))
    (should (string= (lispyville-with "(a b c) (d|)"
                       "}")
                     "(a b c)| (d)"))
    (should (string= (lispyville-with "\"a b\" (a|)"
                       "}")
                     "\"a b|\" (a)"))))

;; tests in visual mode for up-list
(ert-deftest lispyville-backward-up-list ()
  (should (string= (lispyville-with
                       "(cond ((a)\n       (b))\n      ((c)\n       |(d)))"
                     "(")
                   "(cond ((a)\n       (b))\n      |((c)\n       (d)))"))
  (should (string= (lispyville-with
                       "(cond ((a)\n       (b))\n      |((c)\n       (d)))"
                     "(")
                   "|(cond ((a)\n       (b))\n      ((c)\n       (d)))")))

(ert-deftest lispyville-up-list ()
  (should (string= (lispyville-with
                       "(cond ((a)|\n       (b))\n      ((c)\n       (d)))"
                     ")")
                   "(cond ((a)\n       (b)|)\n      ((c)\n       (d)))"))
  (should (string= (lispyville-with
                       "(cond ((a)\n       (b))|\n      ((c)\n       (d)))"
                     ")")
                   "(cond ((a)\n       (b))\n      ((c)\n       (d))|)"))
  (should (string= (lispyville-with "(((a |b c)))"
                     ")")
                   "(((a b c|)))"))
  (should (string= (lispyville-with "(((a b c|)))"
                     ")")
                   "(((a b c)|))"))
  (should (string= (lispyville-with "(((a b c)|))"
                     ")")
                   "(((a b c))|)"))
  (let ((lispyville-motions-put-into-special t))
    (should (string= (lispyville-with "(((a |b c)))"
                       ")")
                     "(((a b c)|))"))
    (should (string= (lispyville-with "(((a b c|)))"
                       ")")
                     "(((a b c))|)"))
    (should (string= (lispyville-with "(((a b c))|)"
                       ")")
                     "(((a b c)))|"))))

(ert-deftest lispyville-> ()
  (should (string= (lispyville-with "((a|) (b c))"
                     ">")
                   "((a (b c)|))"))
  (should (string= (lispyville-with "((a) |(b c))"
                     ">")
                   "((a) b |(c))"))
  (should (string= (lispyville-with "((|a) (b c))"
                     ">")
                   "((|a (b c)))"))
  (let ((lispyville-commands-put-into-special t))
    (should (string= (lispyville-with "((a|) (b c))"
                       ">")
                     "((a (b c))|)"))
    (should (string= (lispyville-with "((|a) (b c))"
                       ">")
                     "((a (b c))|)"))))

(ert-deftest lispyville-< ()
  (should (string= (lispyville-with "((a b) |(c))"
                     "<")
                   "(|((a b) c))"))
  (should (string= (lispyville-with "((a b|) (c))"
                     "<")
                   "((a|) b (c))"))
  (should (string= (lispyville-with "((a |b) (c))"
                     "<")
                   "((a) |b (c))"))
  (should (string= (lispyville-with "((|a b) (c))"
                     "<")
                   "((|a) b (c))"))
  (let ((lispyville-commands-put-into-special t))
    (should (string= (lispyville-with "((a b|) (c))"
                       "<")
                     "((a)| b (c))"))
    (should (string= (lispyville-with "((a |b) (c))"
                       "<")
                     "((a)| b (c))"))
    (should (string= (lispyville-with "((|a b) (c))"
                       "<")
                     "((a)| b (c))"))))

(ert-deftest lispyville-slurp ()
  (lispyville-set-key-theme '(slurp/barf-lispy))
  (should (string= (lispyville-with "((a|) (b c))"
                     ">")
                   "((a (b c)|))"))
  (should (string= (lispyville-with "((a) |(b c))"
                     ">")
                   "(|((a) b c))"))
  (should (string= (lispyville-with "((|a) (b c))"
                     ">")
                   "((|a (b c)))"))
  (let ((lispyville-commands-put-into-special t))
    (should (string= (lispyville-with "((a|) (b c))"
                       ">")
                     "((a (b c))|)"))
    (should (string= (lispyville-with "((|a) (b c))"
                       ">")
                     "((a (b c))|)")))
  (lispyville-set-key-theme))

(ert-deftest lispyville-barf ()
  (lispyville-set-key-theme '(slurp/barf-lispy))
  (should (string= (lispyville-with "(|(a b) (c))"
                     "<")
                   "(a |(b) (c))"))
  (should (string= (lispyville-with "((a b|) (c))"
                     "<")
                   "((a|) b (c))"))
  (should (string= (lispyville-with "((a |b) (c))"
                     "<")
                   "((a) |b (c))"))
  (should (string= (lispyville-with "((|a b) (c))"
                     "<")
                   "((|a) b (c))"))
  (let ((lispyville-commands-put-into-special t))
    (should (string= (lispyville-with "((a b|) (c))"
                       "<")
                     "((a)| b (c))"))
    (should (string= (lispyville-with "((a |b) (c))"
                       "<")
                     "((a)| b (c))"))
    (should (string= (lispyville-with "((|a b) (c))"
                       "<")
                     "((a)| b (c))")))
  (lispyville-set-key-theme))

(ert-deftest lispyville-wrap-with-round ()
  (lispyville-set-key-theme '(wrap))
  (should (string= (lispyville-with "|foo bar"
                     "M-( i w")
                   "|(foo) bar"))
  (should (string= (lispyville-with "|foo bar"
                     "M-( $")
                   "|(foo bar)")))

(ert-deftest lispyville-drag-forward ()
  ;; region
  (should (string= (lispyville-with "(~a |b c)"
                     "M-j")
                   "(c ~a |b)"))
  (should (string= (lispyville-with "(|a b~ c)"
                     "M-j")
                   "(c |a b~)"))
  ;; on a paren
  (should (string= (lispyville-with "|(a) (b)"
                     "M-j")
                   "(b) |(a)"))
  (should (string= (lispyville-with "(a|) (b)"
                     "M-j")
                   "(b) (a|)"))
  (should (string= (lispyville-with "((a)|) (b)"
                     "M-j")
                   "(b) ((a)|)"))
  ;; atoms
  (should (string= (lispyville-with "((|a b c) d)"
                     "M-j")
                   "((b |a c) d)"))
  (should (string= (lispyville-with "((a |b c) d)"
                     "M-j")
                   "((a c |b) d)"))
  ;; atom but can't move back further
  ;; TODO for some reason tests put | one char back
  ;; which doesn't happen when I run the test in emacs myself
  ;; (should (string= (lispyville-with "((a b |c) d)" (kbd "M-j"))
  ;;                  "(d (a b |c))"))
  )

(ert-deftest lispyville-drag-backward ()
  ;; region
  (should (string= (lispyville-with "(a ~b |c)"
                     "M-k")
                   "(~b |c a)"))
  (should (string= (lispyville-with "(a |b c~)"
                     "M-k")
                   "(|b c~ a)"))
  ;; on a paren
  (should (string= (lispyville-with "(a) |(b)"
                     "M-k")
                   "|(b) (a)"))
  (should (string= (lispyville-with "(a) (b|)"
                     "M-k")
                   "(b|) (a)"))
  (should (string= (lispyville-with "(a) ((b)|)"
                     "M-k")
                   "((b)|) (a)"))
  ;; atoms
  (should (string= (lispyville-with "(a (b c |d))"
                     "M-k")
                   "(a (b |d c))"))
  (should (string= (lispyville-with "(a (b |c d))"
                     "M-k")
                   "(a (|c b d))"))
  ;; atom but can't move back further
  ;; same problem as above
  ;; (should (string= (lispyville-with "(a (|b c d))" (kbd "M-k"))
  ;;                  "((|b c d) a)"))
  )

(ert-deftest lispyville-raise-list ()
  ;; should work in the basic case
  (should (string= (lispyville-with "((|a))"
                     "M-R")
                   "(|a)"))
  ;; should work in a string
  (should (string= (lispyville-with "((\"|a\"))"
                     "M-R")
                   "(\"|a\")"))
  ;; should work with a count
  (should (string= (lispyville-with "(((|a)))"
                     "2 M-R")
                   "(|a)"))
  ;; should work with a count larger than the max possible count
  (should (string= (lispyville-with "(((|a)))"
                     "3 M-R")
                   "(|a)")))

(ert-deftest lispyville-insert-at-beginning-of-list ()
  ;; should work in the basic case
  (should (string= (lispyville-with "(a |b c)"
                     "M-i")
                   "(|a b c)"))
  ;; should work in a string
  (should (string= (lispyville-with "(a \"|b\" c)"
                     "M-i")
                   "(|a \"b\" c)"))
  ;; should work with a count
  (should (string= (lispyville-with "((a |b c))"
                     "2 M-i")
                   "(|(a b c))"))
  ;; should work with a count larger than the max possible count
  (should (string= (lispyville-with "((a |b c))"
                     "3 M-i")
                   "(|(a b c))")))

(ert-deftest lispyville-insert-at-end-of-list ()
  ;; should work in the basic case
  (should (string= (lispyville-with "(a |b c)"
                     "M-a")
                   "(a b c|)"))
  ;; should work in a string
  (should (string= (lispyville-with "(a \"|b\" c)"
                     "M-a")
                   "(a \"b\" c|)"))
  ;; should work with a count
  (should (string= (lispyville-with "((a |b c))"
                     "2 M-a")
                   "((a b c)|)"))
  ;; should work with a count larger than the max possible count
  (should (string= (lispyville-with "((a |b c))"
                     "3 M-a")
                   "((a b c)|)")))

(ert-deftest lispyville-open-below-list ()
  ;; should work in the basic case
  (should (string= (lispyville-with "((|a)\n b)"
                     "M-o")
                   "((a)\n |\n b)"))
  ;; should work with trailing sexps
  (should (string= (lispyville-with "((|a) b)"
                     "M-o")
                   "((a)\n |b)"))
  ;; should insert an extra newline at the top-level
  (should (string= (lispyville-with "(|a)"
                     "M-o")
                   "(a)\n\n|"))
  ;; should work with a count
  (should (string= (lispyville-with "((|a))"
                     "2 M-o")
                   "((a))\n\n|"))
  ;; should work with a count larger than the max possible count
  (should (string= (lispyville-with "((|a))"
                     "3 M-o")
                   "((a))\n\n|")))

(ert-deftest lispyville-open-above-list ()
  ;; should work in the basic case
  (should (string= (lispyville-with "(a\n (|b))"
                     "M-O")
                   "(a\n |\n (b))"))
  ;; should work with leading sexps
  (should (string= (lispyville-with "(a (|b))"
                     "M-O")
                   "(a |\n (b))"))
  ;; should insert an extra newline at the top-level
  (should (string= (lispyville-with "(|a)"
                     "M-O")
                   "|\n\n(a)"))
  ;; should work with a count
  (should (string= (lispyville-with "((|a))"
                     "2 M-O")
                   "|\n\n((a))"))
  ;; should work with a count larger than the max possible count
  (should (string= (lispyville-with "((|a))"
                     "3 M-O")
                   "|\n\n((a))")))

(ert-deftest lispyville-wrap-round ()
  ;; TODO why are these failing?
  ;; (lispyville-set-key-theme '((additional-wrap insert normal)))
  ;; should never insert a space when in normal state
  ;; (should (string= (lispyville-with "a |b"
  ;;                    "M-(")
  ;;                  "a (|b)"))
  ;; should insert a space by default when in an insert state
  ;; (should (string= (lispyville-with "a |b"
  ;;                    "i M-(")
  ;;                  "a (| b)"))
  )

;; * Insert Key Theme
(ert-deftest lispyville-insert ()
  (lispyville-space-after-insert)
  ;; should not error at bob
  (should (lispyville-with "|(foo)" "i ESC"))
  (lispyville-space-after-insert t))

;; * Visual and Special Mark Integration
(ert-deftest lispyville-toggle-mark-type ()
  (lispy-define-key lispy-mode-map "m" #'lispyville-toggle-mark-type)
  ;; test that it behaves as `lispy-mark-list' without region
  (should (string= (lispyville-with "|(a b c) d"
                     "im")
                   "~(a b c)| d"))
  (should (string= (lispyville-with "|(a b c) d"
                     "i2m")
                   "(a ~b| c) d"))
  (should (string= (lispyville-with "|(a b c) d"
                     "i2m")
                   "(a ~b| c) d"))
  ;; test that it behaves as `lispy-mark-list' with a count
  (should (string= (lispyville-with "|(a b c) d"
                     "im2m")
                   "(a ~b| c) d"))
  ;; test toggling in special with region
  (should (string= (lispyville-with "|(a b c) d"
                     "imm")
                   "~(a b c|) d"))
  (should (string= (lispyville-with "|(a b c) d"
                     "i2mm")
                   "(a ~|b c) d"))
  (should (string= (lispyville-with "|(a b c) d"
                     "imdm")
                   "|(a b c)~ d"))
  (should (string= (lispyville-with "|(a baz c) d"
                     "i2mdm")
                   "(a |baz~ c) d"))
  ;; test toggling in visual state
  (should (string= (lispyville-with "~(a b c|) d"
                     "v")
                   "~(a b c)| d"))
  (should (string= (lispyville-with "(a ~|b c) d"
                     "v")
                   "(a ~b| c) d"))
  (should (string= (lispyville-with "|(a b c)~ d"
                     "v")
                   "|(a b c)~ d"))
  (should (string= (lispyville-with "(a |baz~ c) d"
                     "v")
                   "(a |baz~ c) d"))
  (let ((lispyville-preferred-lispy-state 'emacs))
    (should (string= (lispyville-with "~(a b c|) d"
                       "v")
                     "~(a b c)| d"))
    (should (string= (lispyville-with "(a ~|b c) d"
                       "v")
                     "(a ~b| c) d"))
    (should (string= (lispyville-with "|(a b c)~ d"
                       "v")
                     "|(a b c)~ d"))
    (should (string= (lispyville-with "(a |baz~ c) d"
                       "v")
                     "(a |baz~ c) d"))))

(ert-deftest lispyville-escape ()
  (should (string= (lispyville-with "|(a b c) d"
                     "i m ESC")
                   "(a b c)| d"))
  (should (string= (lispyville-with "|(a b c) d"
                     "i m 2 ESC")
                   "(a ~b| c) d")))

(ert-deftest lispyville-always-visual ()
  (lispyville-enter-visual-when-marking)
  (should (string= (lispyville-with "|(a b c) d"
                     "im")
                   "~(a b c|) d"))
  (lispyville-remove-marking-hooks))

(ert-deftest lispyville-always-special ()
  (lispyville-enter-special-when-marking)
  (should (string= (lispyville-with "|(a b c) d" (lispy-mark-list 1))
                   "~(a b c)| d"))
  (lispyville-remove-marking-hooks))

;; test wrapping
(ert-deftest lispyville-wrap-lispy-mark-symbol ()
  (lispyville--define-key '(normal visual) "v"
                          (lispyville-wrap-command lispy-mark-symbol visual))
  (should (string= (lispyville-with "(|foo bar) baz qux" "v")
                   "(~fo|o bar) baz qux"))
  (should (string= (lispyville-with "(~fo|o bar) baz qux" "v")
                   "(~foo ba|r) baz qux"))
  (should (string= (lispyville-with "(~foo ba|r) baz qux" "v")
                   "(foo bar) ~ba|z qux"))
  (lispyville--define-key '(normal visual) "v"
                          (lispyville-wrap-command lispy-mark-symbol special))
  (should (string= (lispyville-with "(|foo bar) baz qux" "v")
                   "(~foo| bar) baz qux"))
  (should (string= (lispyville-with "(~fo|o bar) baz qux" "v")
                   "(~foo bar|) baz qux"))
  (should (string= (lispyville-with "(~foo ba|r) baz qux" "v")
                   "(foo bar) ~baz| qux"))
  (should (string= (lispyville-with "(foo |bar) baz qux" "v")
                   "(foo ~bar|) baz qux"))
  (lispyville--define-key 'normal "v" nil)
  (lispyville-set-key-theme '(mark-togle)))
