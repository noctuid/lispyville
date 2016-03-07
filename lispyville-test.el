(defmacro lispyville-with (in &rest body)
  "This is `lispy-with' modified for lispyville."
  (declare (indent 1))
  `(let ((temp-buffer (generate-new-buffer " *temp*")))
     (save-window-excursion
       (unwind-protect
            (progn
              (switch-to-buffer temp-buffer)
              (emacs-lisp-mode)
              (transient-mark-mode 1)
              (evil-mode)
              ;; (lispy-mode)
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
                          (if (or (stringp x)
                                  (and (listp x)
                                       (eq (car x) 'kbd)))
                              `(evil-execute-macro 1 ,x)
                            x))
                        body)
              (insert "|")
              (when (region-active-p)
                (exchange-point-and-mark)
                (insert "~"))
              (buffer-substring-no-properties
               (point-min)
               (point-max)))
         (and (buffer-name temp-buffer)
              (kill-buffer temp-buffer))))))

(defun lispyville-replace-with-last-kill ()
  "Replace buffer with last kill."
  (delete-region (point-min) (point-max))
  (yank))

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
  ;; linewise
  (should (string= (lispyville-with "((\n  |(a b)))"
                     "yy"
                     (lispyville-replace-with-last-kill))
                   "  (a b)|"))
  (should (string= (lispyville-with "(\n  |(a b)\n  (c d))"
                     "2yy"
                     (lispyville-replace-with-last-kill))
                   "  (a b)\n  (c d)|"))
  ;; test yank handler (pasting after linewise yank)
  (should (string= (lispyville-with "((\n  |(a b)))"
                     "yyp")
                   "((\n  (a b)))\n  |(a b)"))
  (should (string= (lispyville-with "((\n  |(a b)))"
                     "yyP")
                   "((\n  |(a b)\n  (a b)))"))
  ;; visual block mode
  (should (string= (lispyville-with "((~a b)\n (c d|))"
                     (kbd "C-v y")
                     (lispyville-replace-with-last-kill))
                   "a b\nc d|"))
  (should (string= (lispyville-with "~(a b)\n(|c d)"
                     (kbd "C-v y")
                     (lispyville-replace-with-last-kill))
                   "a\nc|")))

(ert-deftest lispyville-yank-line ()
  (should (string= (lispyville-with "(|a (b c) d)"
                     "Y"
                     (lispyville-replace-with-last-kill))
                   "a (b c) d|"))
  ;; counts; 2Y doesn't work normally with evil actually
  ;; probably not that useful either
  ;; (should (string= (lispyville-with "((\n  |(a b)\n  (c d))"
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
                     (kbd "C-v Y")
                     (lispyville-replace-with-last-kill))
                   "a b\nc d|"))
  (should (string= (lispyville-with "~(a b)\n(|c d)"
                     (kbd "C-v Y")
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
  (should (string= (lispyville-with "((\n  |(a b)))"
                     "dd")
                   ;; TODO why does the point end up here in testing
                   "(()|)"))
  (should (string= (lispyville-with "(\n  |(a b)\n  (c d))"
                     "2dd")
                   "(|)"))
  ;; testing re-positioning
  (should (string= (lispyville-with "\"multi-line\n|string\""
                     "dd")
                   "\"multi-line|\""))
  (should (string= (lispyville-with "(a\n |(b c)\n (d e))"
                     "dd")
                   "(a\n |(d e))"))
  ;; visual block mode
  (should (string= (lispyville-with "((~a b)\n (c d|))"
                     (kbd "C-v d"))
                   "((|)\n ())"))
  (should (string= (lispyville-with "~(a b)\n(|c d)"
                     (kbd "C-v d"))
                   "|( b)\n( d)")))

(ert-deftest lispyville-delete-line ()
  (should (string= (lispyville-with "(|a (b c) d)"
                     "D")
                   "(|)"))
  ;; visual
  (should (string= (lispyville-with "((\n  ~|(a b)))"
                     "D")
                   "(()|)"))
  ;; visual block
  (should (string= (lispyville-with "((~a b)\n (c d|))"
                     (kbd "C-v D"))
                   "((|)\n ())"))
  (should (string= (lispyville-with "~(a b)\n(|c d)"
                     (kbd "C-v D"))
                   "|( b)\n( d)")))

(ert-deftest lispyville-change ()
  ;; linewise; unlike dd, cc shouldn't bring parens up to previous line
  (should (string= (lispyville-with "((\n  |(a b)))"
                     "cc")
                   "((\n  |))"))
  (should (string= (lispyville-with "(\n  |(a b)\n  (c d))"
                     "2cc")
                   "(\n |)")))

;; pretty much already tested
;; (ert-deftest lispyville-change-line ()
;;   )

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
                     (kbd "C-v x"))
                   "((|)\n ())"))
  ;; splice here
  (should (string= (lispyville-with "~(a b)\n(|c d)"
                     (kbd "C-v x"))
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
                     (kbd "C-v X"))
                   "((|)\n ())"))
  ;; splice here; should behave the same as x
  (should (string= (lispyville-with "~(a b)\n(|c d)"
                     (kbd "C-v X"))
                   "| b\n d")))

(ert-deftest lispyville-substitute ()
  (should (string= (lispyville-with "|(a)"
                     "s")
                   "|(a)"))
  (should (string= (lispyville-with "(|a)"
                     "s")
                   "(|)")))

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
