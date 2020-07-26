;;; test-lispyville.el --- Tests for lispyville.el -*- lexical-binding: t; -*-

;; Author: Fox Kiester <http://github/noctuid>
;; Maintainer: Fox Kiester <noct@posteo.net>
;; Homepage: https://github.com/noctuid/lispyville

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;  Tests for lispyville.

;;; Code:
;; TODO rewrite all tests with real newlines instead of \n

;; * Setup
(when (require 'undercover nil t)
  (undercover "*.el"
              (:exclude "test-*.el")
              (:report-type :codecov)
              (:send-report nil)))

(require 'buttercup)
(require 'lispyville)

(setq lispyville-key-theme '(operators
                             s-operators
                             prettify
                             c-w
                             c-u
                             additional-movement
                             slurp/barf-cp
                             additional
                             additional-insert
                             commentary
                             mark-toggle
                             wrap))
(lispyville-set-key-theme)

(setq evil-move-cursor-back nil
      evil-move-beyond-eol t)

;; * Test Helpers
;; TODO use gensym
(defmacro lispyville-with (in &rest body)
  "This is `lispy-with' modified for lispyville.
Note that | is considered to be \"on\" a character in normal/visual state,
meaning that it is included in a visual selection. ~ on the other hand is not
considered to be on a character, so when it represents the region end, the
character after it is not considered as part of the region."
  (declare (indent 1))
  `(let ((temp-buffer (generate-new-buffer " *temp*")))
     ;; (when (string= (substring in 0 1) "\n")
     ;;   (setq in (substring in 1)))
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

(defmacro lispyville-expect-equal (before after &rest body)
  "Expect BEFORE to transform to AFTER after running BODY.
BODY is the same as the body for `lispyville-with': a list of strings or
commands to run in the buffer."
  (declare (indent 2))
  `(expect (lispyville-with ,before ,@body)
           :to-equal ,after))

;; * Operators
;; ** Yank
(describe "lispyville-yank"
  (describe "should yank safely"
    (it "with a motion"
      (lispyville-expect-equal
          "(|a)"
          "a|"
        "yW"
        (lispyville-replace-with-last-kill)))
    (it "with a visual selection"
      (lispyville-expect-equal
          "(~a { b [ c \"tes|ting\"]})"
          "a  b  c test|"
        "y"
        (lispyville-replace-with-last-kill))
      (lispyville-expect-equal
          "(~a { b [ c \"testing\"]})|"
          "a { b [ c \"testing\"]}|"
        "y"
        (lispyville-replace-with-last-kill)))
    (describe "linewise"
      ;; always ends with newline
      (it ""
        (lispyville-expect-equal
            "((\n  |(a b)))"
            "  (a b)\n|"
          "yy"
          (lispyville-replace-with-last-kill))
        (lispyville-expect-equal
            "(\n  |(a b)\n  (c d))"
            "  (a b)\n  (c d)\n|"
          "2yy"
          (lispyville-replace-with-last-kill)))
      (it "at the buffer end"
        (lispyville-expect-equal
            "|(a b)"
            "(a b)\n|"
          "yy"
          (lispyville-replace-with-last-kill)))
      (it "allowing correct pasting"
        (lispyville-expect-equal
            "((\n  |(a b)))"
            "((\n  (a b)))\n  |(a b)"
          "yyp")
        (lispyville-expect-equal
            "((\n  |(a b)))"
            "((\n  |(a b)\n  (a b)))"
          "yyP")))
    (describe "with a visual block selection"
      (it ""
        (lispyville-expect-equal
            "((~a b)\n (c d|))"
            "a b\nc d|"
          "C-v y"
          (lispyville-replace-with-last-kill))
        (lispyville-expect-equal
            "~(a b)\n(|c d)"
            "a\nc|"
          "C-v y"
          (lispyville-replace-with-last-kill)))
      (it "allowing correct pasting"
        (lispyville-expect-equal
            "~(a b)\n(|c d)"
            "(|aa b)\n(cc d)"
          "C-v y p")))))

(describe "lispyville-yank-line"
  (describe "should yank safely"
    (it "in normal state"
      (lispyville-expect-equal
          "(|a (b c) d)"
          "a (b c) d|"
        "Y"
        (lispyville-replace-with-last-kill)))
    (xit "with a count"
      ;; counts; 2Y doesn't work normally with evil actually
      ;; probably not that useful either
      (lispyville-expect-equal
          "(\n  |(a b)\n  (c d))"
          "  (a b)\n  (c d)|"
        "2Y"
        (lispyville-replace-with-last-kill)))
    (it "with a visual selection"
      (lispyville-expect-equal
          "((\n  ~|(a b)))"
          "((\n  (a b)))\n  |(a b)"
        "Yp")
      (lispyville-expect-equal
          "((\n  ~|(a b)))"
          "((\n  |(a b)\n  (a b)))"
        "YP"))
    (it "with a visual block selection"
      (lispyville-expect-equal
          "((~a b)\n (c d|))"
          "a b\nc d|"
        "C-v Y"
        (lispyville-replace-with-last-kill))
      (lispyville-expect-equal
          "~(a b)\n(|c d)"
          "a\nc|"
        "C-v Y"
        (lispyville-replace-with-last-kill)))))

;; ** Delete
;; TODO reorganize/rephrase
(describe "lispyville-delete"
  (describe "should delete safely"
    (it "with a motion"
      (lispyville-expect-equal
          "(|a)"
          "(|)"
        "dW"))
    (describe "linewise"
      (it ""
        (lispyville-expect-equal
            "(a\n |(b c)\n (d e))"
            "(a\n |(d e))"
          "dd"))
      (it "moving unmatched closing delimiters up to the previous line"
        (lispyville-expect-equal
            "
((
  |(a b)))

foo"
            "
(())
|
foo"
          "dd")
        (lispyville-expect-equal
            "(let ((a 1)\n      |(b 2))\n  (foo a b))"
            "(let ((a 1))\n  |(foo a b))"
          "dd"))
      (it "moving up at the buffer end"
        (lispyville-expect-equal
            "a\n|b"
            "|a"
          "dd")
        (lispyville-expect-equal
            "
((
  |(a b)))"
            "
|(())"
          "dd")
        (lispyville-expect-equal
            "(a\n|b)"
            "|(a)"
          "dd")
        (lispyville-expect-equal
            "\"multi-line\n|string\""
            "|\"multi-line\""
          "dd"))
      (describe "at closing delimiters with comment on previous line"
        (it ""
          (lispyville-expect-equal
              "((;; comment\n  |(a b)))"
              "((;; comment\n  |))"
            "dd"))
        (it "deleting the following newline unless it will pull a sexp up"
          (lispyville-expect-equal
              ;; should still delete a newline
              "((;; comment\n  |(a b)))\n\nfoo"
              "((;; comment\n  |))\nfoo"
            "dd")
          (lispyville-expect-equal
              ;; shouldn't delete a newline if it will pull a sexp up
              "((;; comment\n  |(a b)))\nfoo"
              "((;; comment\n  |))\nfoo"
            "dd"))
        (it "deleting the following newline unless at the buffer end"
          (lispyville-expect-equal
              "(a\n ;; b\n |c)"
              "(a\n ;; b\n |)"
            "dd")))
      (it "at an opening delimiter deleting whitespace"
        (lispyville-expect-equal
            "|(a b\n   c)"
            "|(c)"
          "dd"))
      (it "deleting the final newline"
        (lispyville-expect-equal
            "a\n|"
            "|a"
          "dd"))
      (it "at the buffer end"
        (lispyville-expect-equal
            "|(a b)"
            "|"
          "dd"))
      (describe "with a count"
        (it ""
          (lispyville-expect-equal
              "(\n |(a b)\n (c d))"
              "|()"
            "2dd"))
        (it "without deleting an extra newline"
          (lispyville-expect-equal
              "
(
 |(a b)
 (c d))

foo"
              "
()
|
foo"
            ;; shouldn't delete extra newline
            "2dd"))))
    (describe "with a visual selection"
      (it ""
        (lispyville-expect-equal
            "(~a { b [ c \"tes|ting\"]})"
            "(|{[\"ing\"]})"
          "d")
        (lispyville-expect-equal
            "(~a { b [ c \"testing\"]})|"
            "(|)"
          "d"))
      (it "pulling atoms into comments"
        (lispyville-expect-equal
            ";; ~a\nb| c"
            ";; |c"
          "d"))
      (it "without pulling delimiters into comments"
        (lispyville-expect-equal
            "(a\n ;;~ b\n c)|"
            "(a\n ;;|\n)"
          "d")
        (lispyville-expect-equal
            ";; ~a\n(b\n ;;| c\n d)"
            ";; |\n(c\n d)"
          "d")))
    (it "with a visual block selection"
      (lispyville-expect-equal
          "((~a b)\n (c d|))"
          "((|)\n ())"
        "C-v d")
      (lispyville-expect-equal
          "~(a b)\n(|c d)"
          "|( b)\n( d)"
        "C-v d"))))

(describe "lispyville-delete-line"
  (describe "should delete safely"
    (it "in normal state"
      (lispyville-expect-equal
          "(|a (b c) d)"
          "(|)"
        "D"))
    (it "with a visual selection (acting like dd)"
      (lispyville-expect-equal
          "((\n  ~|(a b)))"
          "|(())"
        "D")
      (lispyville-expect-equal
          "((\n  ~|(a b)))\n"
          "(())\n|"
        "D"))
    (it "with a visual block selection"
      (lispyville-expect-equal
          "((~a b)\n (c d|))"
          "((|)\n ())"
        "C-v D")
      (lispyville-expect-equal
          "~(a b)\n(|c d)"
          "|( b)\n( d)"
        "C-v D"))))

(describe "lispyville-delete-char-or-splice"
  (it "should work in the basic case"
    (lispyville-expect-equal
        "(|a)"
        "(|)"
      "x")
    (lispyville-expect-equal
        "|(a)"
        "|a"
      "x")
    (lispyville-expect-equal
        "(a|)"
        "a|"
      "x"))
  (it "should work by nested side-by-side delimiters"
    (lispyville-expect-equal
        "(foo (bar|))"
        "(foo bar|)"
      "x")
    (lispyville-expect-equal
        "(foo (bar)|)"
        "foo (bar)|"
      "x"))
  (it "should work with a visual selection (acting as x)"
    (lispyville-expect-equal
        "~(|a\n (b c))"
        "|\n (b c)"
      "x")
    (lispyville-expect-equal
        "~(foo)|"
        "(foo)|"
      "x"
      (lispyville-replace-with-last-kill))
    (lispyville-expect-equal
        "(fo~o)|"
        "o|"
      "x"
      (lispyville-replace-with-last-kill))
    (lispyville-expect-equal
        "~(|foo)"
        "f|"
      "x"
      (lispyville-replace-with-last-kill)))
  (it "should work with a visual block selection"
    ;; should have same behavior as d in this case
    (lispyville-expect-equal
        "((~a b)\n (c |d))"
        "((|)\n ())"
      "C-v x")
    ;; splice here
    (lispyville-expect-equal
        "~(a b)\n(|c d)"
        "| b\n d"
      "C-v x")))

(describe "lispyville-delete-char-or-splice-backwards"
  (it "should work in the basic case"
    (lispyville-expect-equal
        "(a|)"
        "(|)"
      "X")
    (lispyville-expect-equal
        "(|a)"
        "|a"
      "X")
    (lispyville-expect-equal
        "(a)|"
        "a|"
      "X"))
  (it "should work by nested side-by-side delimiters"
    (lispyville-expect-equal
        "(foo (bar)|)"
        "(foo bar|)"
      "X")
    (lispyville-expect-equal
        "(foo (bar))|"
        "foo (bar)|"
      "X"))
  (it "should work with a visual selection (acting as x)"
    (lispyville-expect-equal
        "~(|a\n (b c))"
        "|\n (b c)"
      "X")
    (lispyville-expect-equal
        "~(foo)|"
        "(foo)|"
      "X"
      (lispyville-replace-with-last-kill))
    (lispyville-expect-equal
        "(fo~o)|"
        "o|"
      "X"
      (lispyville-replace-with-last-kill))
    (lispyville-expect-equal
        "~(|foo)"
        "f|"
      "X"
      (lispyville-replace-with-last-kill)))
  (it "should work with a visual block selection"
    ;; same as d
    (lispyville-expect-equal
        "((~a b)\n (c |d))"
        "((|)\n ())"
      "C-v X")
    ;; splice
    (lispyville-expect-equal
        "~(a b)\n(|c d)"
        "| b\n d"
      "C-v X")))

(describe "lispyville-delete-backward-word"
  (it "should work after an atom"
    (lispyville-expect-equal
        "(foo bar|)"
        "(foo |)"
      "i C-w"))
  (it "should delete the full sexp after the opening delimiter"
    (lispyville-expect-equal
        "(|a)"
        "|"
      "i C-w"))
  (it "should delete the full sexp after the closing delimiter"
    (lispyville-expect-equal
        "(a)|"
        "|"
      "i C-w"))
  (it "should delete the full string string after a string"
    (lispyville-expect-equal
        "\"a\"|"
        "|"
      "i C-w")))

(describe "lispyville-delete-back-to-indentation"
  (before-all
    (defvar lispyville-evil-want-C-u-original-value evil-want-C-u-delete)
    (custom-set-variables '(evil-want-C-u-delete t)))
  (after-all
    (custom-set-variables (list 'evil-want-C-u-delete
                                lispyville-evil-want-C-u-original-value)))
  (it "should act as a safe `evil-delete-back-to-indentation'"
    (lispyville-expect-equal
        "(foo bar\n     foobar|)"
        "(foo bar\n     |)"
      "i C-u")
    (lispyville-expect-equal
        "(foo bar\n     |)"
        "(foo bar\n|)"
      "i C-u")
    (lispyville-expect-equal
        "(foo bar\n|)"
        "(foo bar|)"
      "i C-u")
    (lispyville-expect-equal
        "(foo bar|)"
        "(|)"
      "i C-u")
    (lispyville-expect-equal
        "(|a)"
        "|"
      "i C-u")
    (lispyville-expect-equal
        "(a)|"
        "|"
      "i C-u")
    (lispyville-expect-equal
        "\"a\"|"
        "|"
      "i C-u")))

;; ** Change
(describe "lispyville-change"
  (it "should delete linewise without deleting newlines"
    (lispyville-expect-equal
        "|foo\n"
        "|\n"
      "cc")
    (lispyville-expect-equal
        "(((a b)\n  |(c d)\n  (e f)))"
        "(((a b)\n  |\n  (e f)))"
      "cc"))
  (it "should work before closing unmatched delimiter(s)"
    (lispyville-expect-equal
        "((\n  |(a b)))\n"
        "((\n  |))\n"
      "cc")
    (lispyville-expect-equal
        "(let ((a 1)\n      |(b 2))\n  (foo a b))\n"
        "(let ((a 1)\n      |)\n  (foo a b))\n"
      "cc"))
  (it "should delete one newline with a count of 2"
    (lispyville-expect-equal
        "(\n  |(a b)\n  (c d))\n"
        "(\n |)\n"
      "2cc"))
  (it "should work before opening unmatched delimiters"
    (lispyville-expect-equal
        "a\n|(b\n c)\n"
        "a\n(|\n c)\n"
      "cc")
    (lispyville-expect-equal
        "a\n(b|\n c)\n"
        "a\n(|\n c)\n"
      "cc"))
  (it "should work at the buffer end"
    (lispyville-expect-equal
        "|(a b)"
        "|\n"
      "cc"))
  (it "should not pull up opening delimiters"
    (lispyville-expect-equal
        "a|\n(b)\n"
        "|\n(b)\n"
      "cc"))
  (it "should be undoable with one undo (with evil-want-fine-undo nil)"
    (lispyville-expect-equal
        "(defvar foo\n  bar baz)|"
        "(defvar foo\n  bar baz)|"
      (concat "cchello" "ESC" "u"))))

;; pretty much already tested
;; (describe "lispyville-change-line"
;;   )

(describe "lispyville-change-whole-line"
  (it "should work as a safe S"
    (lispyville-expect-equal
        "((\n  |(a b)))\n"
        "((\n  |))\n"
      "S")))

(describe "lispyville-substitute"
  (it "should work as a safe s"
    (lispyville-expect-equal
        "|(a)"
        "|(a)"
      "s")
    (lispyville-expect-equal
        "(|a)"
        "(|)"
      "s")))

;; ** Prettify
(describe "lispyville-prettify"
  (it "should work with a motion"
    (lispyville-expect-equal
        "(foo\n |a\n )"
        "(foo\n |a)"
      "=G")))

;; ** Comment/Uncomment
(describe "lispyville-comment-or-uncomment"
  (describe "should work linewise"
    (it ""
      (lispyville-expect-equal
          "|foo"
          "|;; foo"
        "gcc")
      (lispyville-expect-equal
          "|;; foo"
          "|foo"
        "gcc"))
    (it "before unbalanced opening delimiters"
      (lispyville-expect-equal
          "|(foo\n bar baz)"
          "|(;; foo\n bar baz)"
        "gcc"))
    (it "before unbalanced closing delimiters"
      (lispyville-expect-equal
          "(foo\n bar |baz)"
          "(foo\n ;; bar |baz\n )"
        "gcc"))
    (it "after unbalanced opening delimiters"
      ;; TODO maybe auto indent/stripping indentation should be built into the
      ;; macro?
      (lispyville-expect-equal
          "(foo (bar (baz|\n quux)))"
          "(;; foo\n(;; bar\n(;; baz|\nquux)))"
        "gcc"
        (indent-rigidly (point-min) (point-max) -10)))
    (it "with a multiline comment"
      (lispyville-expect-equal
          "/* |a\nb\nc*/ d"
          "|a\nb\nc d"
        (c-mode)
        (lispyville-mode)
        "gcc")))
  (it "should work with text objects"
    (lispyville-expect-equal
        "foo b|ar baz"
        "foo ;; b|ar\nbaz"
      "gciw"))
  (it "should work with a visual selection"
    (lispyville-expect-equal
        "(foo (ba~r (baz\n qu|ux)))"
        "(foo (ba|;; r\n(;; baz\n;; quu\nx)))"
      "gc"
      (indent-rigidly (point-min) (point-max) -10))))

(describe "lispyville-comment-and-clone-dwim"
  (describe "should work linewise"
    (it ""
      ;; cursor stays on the uncommented region
      (lispyville-expect-equal
          "fo|o"
          ";; foo\nfo|o"
        "gyy"))
    ;; TODO why is indentation messed up when running tests?
    (xit "on a line with unmatched opening delimiters"
      ;; only the rightmost balanced region
      (lispyville-expect-equal
          "
(lorem (|ipsum (dolor
                (sit)))
       (amet))"
          "
(lorem (|ipsum (;; dolor
                dolor
                (sit)))
       (amet))"
        "gyy")))
  (it "should work with text objects"
    (lispyville-expect-equal
        "foo b|ar baz"
        "foo ;; bar\nb|ar baz"
      "gyiw"))
  ;; TODO why is indentation messed up?
  (xit "should work with a visual selection"
    (lispyville-expect-equal
        "
~(lorem (ipsum (dolor|
                (sit)))
        (amet))"
        "
|(;; lorem
  lorem (;; ipsum
         ipsum (;; dolor
                dolor
                (sit)))
        (amet))"
      "gy")
    (lispyville-expect-equal
        "
(lorem (ipsum (~dolor
               (sit)))
       (amet))|"
        "
(lorem (ipsum (|;; dolor
               ;; (sit)
               dolor
               (sit)))
       ;; (amet)
       (amet))"
      "gy")))

;; * Commands/Motions
;; ** Navigation
(describe "lispyville-first-non-blank"
  (it "should go to the first non-blank character skipping opening delimiters"
    (lispyville-expect-equal
        "|   a"
        "   |a"
      (lispyville-first-non-blank))
    ;; should not go past the ending quote
    (lispyville-expect-equal
        "\"a string with a newline \n|  \""
        "\"a string with a newline \n  |\""
      (lispyville-first-non-blank))
    (lispyville-expect-equal
        "|   ([{\"a string\"}])"
        "   ([{\"|a string\"}])"
      (lispyville-first-non-blank))))

(describe "lispyville-forward-sexp"
  (it "should act as `forward-sexp'"
    (lispyville-expect-equal
        "(|a b c)"
        "(a| b c)"
      "L"))
  (it "should work with a count"
    (lispyville-expect-equal
        "(|a b c)"
        "(a b| c)"
      "2L")
    (lispyville-expect-equal
        "(|a b c)"
        "(a b c|)"
      "3L")
    (lispyville-expect-equal
        "(~|a b c)"
        "(~a b c|)"
      "3L")
    (lispyville-expect-equal
        "(|a b (c e d))"
        "(a b (c e d)|)"
      "3L")))

(describe "lispyville-backward-sexp"
  (it "should act as `backward-sexp'"
    (lispyville-expect-equal
        "(a b c|)"
        "(a b |c)"
      "H"))
  (it "should work with a count"
    (lispyville-expect-equal
        "(a b c|)"
        "(a |b c)"
      "2H")
    (lispyville-expect-equal
        "(a b c|)"
        "(|a b c)"
      "3H")
    (lispyville-expect-equal
        "(a b |c d~)"
        "(|a b c d~)"
      "2H")
    (lispyville-expect-equal
        "(a b (c e d)|)"
        "(|a b (c e d))"
      "3H")))

;; TODO test maybe enter special behavior
(describe "lispyville-beginning-of-defun"
  (it "should act as `beginning-of-defun'"
    (lispyville-expect-equal
        "(a |b c)"
        "|(a b c)"
      "M-h")))

(describe "lispyville-end-of-defun"
  (it "should act as `end-of-defun'"
    (lispyville-expect-equal
        "(a |b c)"
        "(a b c|)"
      "M-l"))
  (it "should go to after the closing delimiter when \
`lispyville-motions-put-into-special' is non-nil"
    (let ((lispyville-motions-put-into-special t))
      (lispyville-expect-equal
          "(a |b c)"
          "(a b c)|"
        "M-l"))))

(describe "lispyville-next-opening"
  (it "should go to the next opening delimiter"
    (lispyville-expect-equal
        "|(((a b c)))"
        "(|((a b c)))"
      "{")
    (lispyville-expect-equal
        "(|((a b c)))"
        "((|(a b c)))"
      "{")
    (lispyville-expect-equal
        "|a\nb\n(c)"
        "a\nb\n|(c)"
      "{"))
  (it "should work with a count"
    (lispyville-expect-equal
        "|(((a b c)))"
        "((|(a b c)))"
      "2{"))
  (it "should work with a visual selection"
    (lispyville-expect-equal
        "~|(((a b c)))"
        "~(|((a b c)))"
      "{")
    (lispyville-expect-equal
        "~(|((a b c)))"
        "~((|(a b c)))"
      "{"))
  (it "should work with a visual selection and a count"
    (lispyville-expect-equal
        "~|(((a b c)))"
        "~((|(a b c)))"
      "2{"))
  (it "should not jump to delimiters in strings"
    (lispyville-expect-equal
        "\"|\\\"{[(a)]}\\\"\" (b)"
        "\"\\\"{[(a)]}\\\"\" |(b)"
      "{"))
  (it "should not jump to delimiters in comments"
    (lispyville-expect-equal
        "|;; ([{\n(a)"
        ";; ([{\n|(a)"
      "{")
    (lispyville-expect-equal
        "|a\n;; ("
        "|a\n;; ("
      "{")))

(describe "lispyville-previous-opening"
  (it "should go to the previous opening delimiter"
    (lispyville-expect-equal
        "(((a |b c)))"
        "((|(a b c)))"
      "[")
    (lispyville-expect-equal
        "((|(a b c)))"
        "(|((a b c)))"
      "[")
    (lispyville-expect-equal
        "(|((a b c)))"
        "|(((a b c)))"
      "[")
    (lispyville-expect-equal
        "(a)\nb\nc|"
        "|(a)\nb\nc"
      "["))
  (it "should work with a count"
    (lispyville-expect-equal
        "(((a |b c)))"
        "(|((a b c)))"
      "2[")
    (lispyville-expect-equal
        "(((a |b c)))"
        "|(((a b c)))"
      "3["))
  (it "should not go to delimiters in strings"
    (lispyville-expect-equal
        "\"\\\"{[(a)]}\\\"\" |(b)"
        "|\"\\\"{[(a)]}\\\"\" (b)"
      "["))
  (it "should not go to delimiters in comments"
    (lispyville-expect-equal
        "(a)\n;; ([{|"
        "|(a)\n;; ([{"
      "[")
    (lispyville-expect-equal
        ";; (\n|a"
        ";; (\n|a"
      "[")))

(describe "lispyville-next-closing"
  (it "should go to the next closing delimiter"
    (lispyville-expect-equal
        "|(((a b c)))"
        "(((a b c|)))"
      "]")
    (lispyville-expect-equal
        "(((a b c|)))"
        "(((a b c)|))"
      "]")
    (lispyville-expect-equal
        "(((a b c|)))"
        "(((a b c)|))"
      "]")
    (lispyville-expect-equal
        "(((a b c)|))"
        "(((a b c))|)"
      "]")
    (lispyville-expect-equal
        "|a\nb\n(c)"
        "a\nb\n(c|)"
      "]"))
  (it "should work with a count"
    (lispyville-expect-equal
        "|(((a b c)))"
        "(((a b c))|)"
      "3]"))
  (it "should not go to delimiters in strings"
    (lispyville-expect-equal
        "\"|\\\"{[(a)]}\\\"\""
        "\"\\\"{[(a)]}\\\"|\""
      "]"))
  (it "should not go to delimiters in comments"
    (lispyville-expect-equal
        "|;; )]}\n(a)"
        ";; )]}\n(a|)"
      "]")
    (lispyville-expect-equal
        "|a\n;; )"
        "|a\n;; )"
      "]"))
  (it "should go to after the closing delimiter when \
`lispyville-motions-put-into-special' is non-nil"
    (let ((lispyville-motions-put-into-special t))
      (lispyville-expect-equal
          "(a |b c)"
          "(a b c)|"
        "]")
      (lispyville-expect-equal
          "\"|a b\""
          "\"a b|\""
        "]"))))

(describe "lispyville-previous-closing"
  (it "should go to the previous closing delimiter"
    (lispyville-expect-equal
        "(((a b c)))|"
        "(((a b c))|)"
      "}")
    (lispyville-expect-equal
        "(((a b c))|)"
        "(((a b c)|))"
      "}")
    (lispyville-expect-equal
        "(((a b c)|))"
        "(((a b c|)))"
      "}")
    (lispyville-expect-equal
        "(a)\nb\nc|"
        "(a|)\nb\nc"
      "}"))
  (it "should work with a count"
    (lispyville-expect-equal
        "(((a b c)))|"
        "(((a b c|)))"
      "3}"))
  (it "should not go to a delimiter inside a string"
    (lispyville-expect-equal
        "(a) \"\\\"{[(b)]}\\\"|\""
        "(a|) \"\\\"{[(b)]}\\\"\""
      "}"))
  (it "should not go to a delimiter inside a comment"
    (lispyville-expect-equal
        "(a)\n;; }])|"
        "(a|)\n;; }])"
      "}")
    (lispyville-expect-equal
        ";; )\n|a"
        ";; )\n|a"
      "}"))
  (it "should go to after the closing delimiter when \
`lispyville-motions-put-into-special' is non-nil"
    (let ((lispyville-motions-put-into-special t))
      (lispyville-expect-equal
          "(a b c) (d|)"
          "(a b c)| (d)"
        "}")
      (lispyville-expect-equal
          "\"a b\" (a|)"
          "\"a b|\" (a)"
        "}"))))

(describe "lispyville-backward-up-list"
  (it "should act as `lispy-left'"
    (lispyville-expect-equal
        "(cond ((a)\n       (b))\n      ((c)\n       |(d)))"
        "(cond ((a)\n       (b))\n      |((c)\n       (d)))"
      "(")
    (lispyville-expect-equal
        "(cond ((a)\n       (b))\n      |((c)\n       (d)))"
        "|(cond ((a)\n       (b))\n      ((c)\n       (d)))"
      "(")))

(describe "lispyville-up-list"
  (it "should act as `lispy-right'"
    (lispyville-expect-equal
        "(cond ((a)|\n       (b))\n      ((c)\n       (d)))"
        "(cond ((a)\n       (b)|)\n      ((c)\n       (d)))"
      ")")
    (lispyville-expect-equal
        "(cond ((a)\n       (b))|\n      ((c)\n       (d)))"
        "(cond ((a)\n       (b))\n      ((c)\n       (d))|)"
      ")")
    (lispyville-expect-equal
        "(((a |b c)))"
        "(((a b c|)))"
      ")")
    (lispyville-expect-equal
        "(((a b c|)))"
        "(((a b c)|))"
      ")")
    (lispyville-expect-equal
        "(((a b c)|))"
        "(((a b c))|)"
      ")"))
  (it "should go to after the closing delimiter when \
`lispyville-motions-put-into-special' is non-nil"
    (let ((lispyville-motions-put-into-special t))
      (lispyville-expect-equal
          "(((a |b c)))"
          "(((a b c)|))"
        ")")
      (lispyville-expect-equal
          "(((a b c|)))"
          "(((a b c))|)"
        ")")
      (lispyville-expect-equal
          "(((a b c))|)"
          "(((a b c)))|"
        ")"))))

(describe "lispyville-forward-atom-begin"
  (before-all
    (lispyville-set-key-theme '(atom-movement)))
  (after-all
    (lispyville-set-key-theme))
  (it "should go to the next atom or comment beginning"
    (lispyville-expect-equal
        "(|foo-bar baz-qux)"
        "(foo-bar |baz-qux)"
      "w"))
  (it "should work with an operator"
    (lispyville-expect-equal
        "(|foo-bar baz-qux)"
        "(| baz-qux)"
      "cw"))
  (it "should respect `lispyville-want-change-atom-to-end' or \
`evil-want-change-word-to-end'"
    (let ((lispyville-want-change-atom-to-end t)
          evil-want-change-word-to-end)
      (lispyville-expect-equal
          "(|foo-bar baz-qux)"
          "(| baz-qux)"
        "cw"))
    (let (evil-want-change-word-to-end)
      (lispyville-expect-equal
          "(|foo-bar baz-qux)"
          "(|baz-qux)"
        "cw"))
    (let (lispyville-want-change-atom-to-end)
      (lispyville-expect-equal
          "(|foo-bar baz-qux)"
          "(|baz-qux)"
        "cw"))))

;; ** Slurping
;; TODO test counts for all of these

(describe "lispyville->"
  (it "should slurp on a closing delimiter"
    (lispyville-expect-equal
        "((a|) (b c))"
        "((a (b c)|))"
      ">"))
  (it "should barf on an opening delimiter"
    (lispyville-expect-equal
        "((a) |(b c))"
        "((a) b |(c))"
      ">"))
  (describe "should slurp on the right if not on a delimiter"
    (it ""
      (lispyville-expect-equal
          "((|a) (b c))"
          "((|a (b c)))"
        ">"))
    (it "and move with the moved delimiter if \
`lispyville-commands-put-into-special' is non-nil"
      (let ((lispyville-commands-put-into-special t))
        (lispyville-expect-equal
            "((a|) (b c))"
            "((a (b c))|)"
          ">")
        (lispyville-expect-equal
            "((|a) (b c))"
            "((a (b c))|)"
          ">")))))

(describe "lispyville-<"
  (it "should slurp on an opening delimiter"
    (lispyville-expect-equal
        "((a b) |(c))"
        "(|((a b) c))"
      "<"))
  (it "should barf on a closing delimiter"
    (lispyville-expect-equal
        "((a b|) (c))"
        "((a|) b (c))"
      "<"))
  (describe "should barf on the right if not on a delimiter"
    (it ""
      (lispyville-expect-equal
          "((a |b) (c))"
          "((a) |b (c))"
        "<")
      (lispyville-expect-equal
          "((|a b) (c))"
          "((|a) b (c))"
        "<"))
    (it "and move with the moved delimiter if \
`lispyville-commands-put-into-special' is non-nil"
      (let ((lispyville-commands-put-into-special t))
        (lispyville-expect-equal
            "((a b|) (c))"
            "((a)| b (c))"
          "<")
        (lispyville-expect-equal
            "((a |b) (c))"
            "((a)| b (c))"
          "<")
        (lispyville-expect-equal
            "((|a b) (c))"
            "((a)| b (c))"
          "<")))))

(describe "lispyville-slurp"
  (before-all
    (lispyville-set-key-theme '(slurp/barf-lispy)))
  (after-all
    (lispyville-set-key-theme))
  (describe "should slurp"
    (it ""
      (lispyville-expect-equal
          "((a|) (b c))"
          "((a (b c)|))"
        ">")
      (lispyville-expect-equal
          "((a) |(b c))"
          "(|((a) b c))"
        ">")
      (lispyville-expect-equal
          "((|a) (b c))"
          "((|a (b c)))"
        ">"))
    (it "and move with the moved delimiter if \
`lispyville-commands-put-into-special' is non-nil"
      (let ((lispyville-commands-put-into-special t))
        (lispyville-expect-equal
            "((a|) (b c))"
            "((a (b c))|)"
          ">")
        (lispyville-expect-equal
            "((|a) (b c))"
            "((a (b c))|)"
          ">")))))

(describe "lispyville-barf"
  (before-all
    (lispyville-set-key-theme '(slurp/barf-lispy)))
  (after-all
    (lispyville-set-key-theme))
  (describe "should barf"
    (it ""
      (lispyville-expect-equal
          "(|(a b) (c))"
          "(a |(b) (c))"
        "<")
      (lispyville-expect-equal
          "((a b|) (c))"
          "((a|) b (c))"
        "<")
      (lispyville-expect-equal
          "((a |b) (c))"
          "((a) |b (c))"
        "<")
      (lispyville-expect-equal
          "((|a b) (c))"
          "((|a) b (c))"
        "<"))
    (it "and move with the moved delimiter if \
`lispyville-commands-put-into-special' is non-nil"
      (let ((lispyville-commands-put-into-special t))
        (lispyville-expect-equal
            "((a b|) (c))"
            "((a)| b (c))"
          "<")
        (lispyville-expect-equal
            "((a |b) (c))"
            "((a)| b (c))"
          "<")
        (lispyville-expect-equal
            "((|a b) (c))"
            "((a)| b (c))"
          "<")))))

;; ** Wrapping
(describe "lispyville-wrap-with-round"
  (it "should wrap parens around an area using a motion"
    (lispyville-expect-equal
        "|foo bar"
        "|(foo bar)"
      "M-( $"))
  (it "should wrap parens around an area using a text object"
    (lispyville-expect-equal
        "|foo bar"
        "|(foo) bar"
      "M-( i w")))


;; TODO why are these failing?
(describe "lispyville-wrap-round"
  (before-all
    (lispyville-set-key-theme '((additional-wrap insert normal))))
  (after-all
    (lispyville-set-key-teme))
  (it "should never insert a space when in normal state"
    (lispyville-expect-equal
        "a |b"
        "a (|b)"
      "M-("))
  (it "should insert a space by default when in an insert state"
    (lispyville-expect-equal
        "a |b"
        "a (| b)"
      "i M-(")))

;; ** Dragging
;; TODO test counts

(describe "lispyville-drag-forward"
  (it "should drag a visually selected region down"
    (lispyville-expect-equal
        "(~a |b c)"
        "(c ~a |b)"
      "M-j")
    (lispyville-expect-equal
        "(|a b~ c)"
        "(c |a b~)"
      "M-j"))
  (it "should move the current sexp down when on a delimiter"
    (lispyville-expect-equal
        "|(a) (b)"
        "(b) |(a)"
      "M-j")
    (lispyville-expect-equal
        "(a|) (b)"
        "(b) (a|)"
      "M-j")
    (lispyville-expect-equal
        "((a)|) (b)"
        "(b) ((a)|)"
      "M-j"))
  (it "should drag the current atom down"
    (lispyville-expect-equal
        "((|a b c) d)"
        "((b |a c) d)"
      "M-j")
    (lispyville-expect-equal
        "((a |b c) d)"
        "((a c |b) d)"
      "M-j"))
  (it "should drag the current sexp down if the current atom cannot be moved"
    (lispyville-expect-equal
        "((a b |c) d)"
        "(d (a b |c))"
      "M-j")))

(describe "lispyville-drag-backward"
  (it "should drag a visually selected region up"
    (lispyville-expect-equal
        "(a ~b |c)"
        "(~b |c a)"
      "M-k")
    (lispyville-expect-equal
        "(a |b c~)"
        "(|b c~ a)"
      "M-k"))
  (it "should drag the current sexp up when on a delimiter"
    (lispyville-expect-equal
        "(a) |(b)"
        "|(b) (a)"
      "M-k")
    (lispyville-expect-equal
        "(a) (b|)"
        "(b|) (a)"
      "M-k")
    (lispyville-expect-equal
        "(a) ((b)|)"
        "((b)|) (a)"
      "M-k"))
  (it "should drag the current atom up"
    (lispyville-expect-equal
        "(a (b c |d))"
        "(a (b |d c))"
      "M-k")
    (lispyville-expect-equal
        "(a (b |c d))"
        "(a (|c b d))"
      "M-k"))
  (it "should drag the current sexp up if the current atom cannot be moved"
    (lispyville-expect-equal
        "(a (|b c d))"
        "((|b c d) a)"
      "M-k")))

;; ** Raising
(describe "lispyville-raise-list"
  (it "should work in the basic case"
    (lispyville-expect-equal
        "((|a))"
        "(|a)"
      "M-R"))
  (it "should work in a string"
    (lispyville-expect-equal
        "((\"|a\"))"
        "(\"|a\")"
      "M-R"))
  (it "should work with a count"
    (lispyville-expect-equal
        "(((|a)))"
        "(|a)"
      "2 M-R"))
  (it "should work with a count larger than the max possible count"
    (lispyville-expect-equal
        "(((|a)))"
        "(|a)"
      "3 M-R")))

;; ** Insertion
(describe "lispyville-insert"
  (before-all
    (lispyville-space-after-insert))
  (after-all
    (lispyville-space-after-insert t))
  (it "should not error at the buffer start"
    (expect (lispyville-with "|(foo)"
              "i ESC")
            :not :to-throw)))

;; ** Additional Insertion
(describe "lispyville-insert-at-beginning-of-list"
  (it "should work in the basic case"
    (lispyville-expect-equal
        "(a |b c)"
        "(|a b c)"
      "M-i"))
  (it "should work in a string"
    (lispyville-expect-equal
        "(a \"|b\" c)"
        "(|a \"b\" c)"
      "M-i"))
  (it "should work with a count"
    (lispyville-expect-equal
        "((a |b c))"
        "(|(a b c))"
      "2 M-i"))
  (it "should work with a count larger than the max possible count"
    (lispyville-expect-equal
        "((a |b c))"
        "(|(a b c))"
      "3 M-i")))

(describe "lispyville-insert-at-end-of-list"
  (it "should work in the basic case"
    (lispyville-expect-equal
        "(a |b c)"
        "(a b c|)"
      "M-a"))
  (it "should work in a string"
    (lispyville-expect-equal
        "(a \"|b\" c)"
        "(a \"b\" c|)"
      "M-a"))
  (it "should work with a count"
    (lispyville-expect-equal
        "((a |b c))"
        "((a b c)|)"
      "2 M-a"))
  (it "should work with a count larger than the max possible count"
    (lispyville-expect-equal
        "((a |b c))"
        "((a b c)|)"
      "3 M-a")))

(describe "lispyville-open-below-list"
  (it "should work in the basic case"
    (lispyville-expect-equal
        "((|a)\n b)"
        "((a)\n |\n b)"
      "M-o"))
  (it "should work with trailing sexps"
    (lispyville-expect-equal
        "((|a) b)"
        "((a)\n |b)"
      "M-o"))
  (it "should insert an extra newline at the top-level"
    (lispyville-expect-equal
        "(|a)"
        "(a)\n\n|"
      "M-o"))
  (it "should work with a count"
    (lispyville-expect-equal
        "((|a))"
        "((a))\n\n|"
      "2 M-o"))
  (it "should work with a count larger than the max possible count"
    (lispyville-expect-equal
        "((|a))"
        "((a))\n\n|"
      "3 M-o")))

(describe "lispyville-open-above-list"
  (it "should work in the basic case"
    (lispyville-expect-equal
        "(a\n (|b))"
        "(a\n |\n (b))"
      "M-O"))
  (it "should work with leading sexps"
    (lispyville-expect-equal
        "(a (|b))"
        "(a |\n (b))"
      "M-O"))
  (it "should insert an extra newline at the top-level"
    (lispyville-expect-equal
        "(|a)"
        "|\n\n(a)"
      "M-O"))
  (it "should work with a count"
    (lispyville-expect-equal
        "((|a))"
        "|\n\n((a))"
      "2 M-O"))
  (it "should work with a count larger than the max possible count"
    (lispyville-expect-equal
        "((|a))"
        "|\n\n((a))"
      "3 M-O")))

;; ** Join
(describe "lispyville-join"
  (it "should work in the basic case"
    (lispyville-expect-equal
        "fo|o\n bar"
        "foo| bar"
      "J")
    (lispyville-expect-equal
        "|foo     \n   bar"
        "foo| bar"
      "J")
    (lispyville-expect-equal
        "(|foo \n     )"
        "(foo|)"
      "J"))
  (it "should do nothing at the buffer end"
    (lispyville-expect-equal
        "|foo"
        "foo|"
      "J"))
  (it "should work with a count"
    (lispyville-expect-equal
        "(a|\n b\n c)"
        "(a b| c)"
      "3J"))
  (it "should splice comments together"
    (lispyville-expect-equal
        ";; |foo\n;; bar"
        ";; foo| bar"
      "J")
    (lispyville-expect-equal
        ";; foo |  \n     ;    bar"
        ";; foo| bar"
      "J")
    (lispyville-expect-equal
        "a | ; b  \n;;  c"
        "a  ; b| c"
      "J"))
  (it "should insert between inline comments"
    (lispyville-expect-equal
        "|a ; b\nc"
        "a| c ; b"
      "J")
    (lispyville-expect-equal
        "a | ;; b\n c ; d"
        "a| c ;; b ; d"
      "J"))
  (it "should join normally with multiline comments"
    (lispyville-expect-equal
        "|/* foo\n   bar\n   baz */"
        "/* foo| bar\n   baz */"
      (c-mode)
      (lispyville-mode)
      "J")
    (lispyville-expect-equal
        "/* foo\n   b|ar */\nbaz"
        "/* foo\n   bar */| baz"
      (c-mode)
      (lispyville-mode)
      "J")
    (lispyville-expect-equal
        "/* foo\n   bar| */\n// baz"
        "/* foo\n   bar */| // baz"
      (c-mode)
      (lispyville-mode)
      "J")
    (lispyville-expect-equal
        "|foo\n/* bar\n   baz */"
        "foo| /* bar\n   baz */"
      (c-mode)
      (lispyville-mode)
      "J"))
  (xit "it should work in between a single and multiline comment"
    ;; TODO splicing a single line comment with following multiline comment is a
    ;; little broken, but easy enough to fix manually
    (lispyville-expect-equal
        "// |foo\n/* bar\n   baz */"
        "// foo| /* bar\n   baz */"
      (c-mode)
      (lispyville-mode)
      "J")))

;; ** Visual and Special Mark Integration
(describe "lispyville-toggle-mark-type"
  (before-all
    (lispy-define-key lispy-mode-map "m" #'lispyville-toggle-mark-type))
  (it "should behave as `lispy-mark-list' without region"
    (lispyville-expect-equal
        "|(a b c) d"
        "~(a b c)| d"
      "im")
    (lispyville-expect-equal
        "|(a b c) d"
        "(a ~b| c) d"
      "i2m")
    (lispyville-expect-equal
        "|(a b c) d"
        "(a ~b| c) d"
      "i2m"))
  (it "should behave as `lispy-mark-list' with a count"
    (lispyville-expect-equal
        "|(a b c) d"
        "(a ~b| c) d"
      "im2m"))
  (it "should toggle the mark type in special"
    (lispyville-expect-equal
        "|(a b c) d"
        "~(a b c|) d"
      "imm")
    (lispyville-expect-equal
        "|(a b c) d"
        "(a ~|b c) d"
      "i2mm")
    (lispyville-expect-equal
        "|(a b c) d"
        "|(a b c)~ d"
      "imdm")
    (lispyville-expect-equal
        "|(a baz c) d"
        "(a |baz~ c) d"
      "i2mdm"))
  (it "should toggle the mark type in visual state"
    (lispyville-expect-equal
        "~(a b c|) d"
        "~(a b c)| d"
      "v")
    (lispyville-expect-equal
        "(a ~|b c) d"
        "(a ~b| c) d"
      "v")
    (lispyville-expect-equal
        "|(a b c)~ d"
        "|(a b c)~ d"
      "v")
    (lispyville-expect-equal
        "(a |baz~ c) d"
        "(a |baz~ c) d"
      "v"))
  (it "should enter `lispyville-preferred-lispy-state'"
    (let ((lispyville-preferred-lispy-state 'emacs))
      (lispyville-expect-equal
          "~(a b c|) d"
          "~(a b c)| d"
        "v")
      (lispyville-expect-equal
          "(a ~|b c) d"
          "(a ~b| c) d"
        "v")
      (lispyville-expect-equal
          "|(a b c)~ d"
          "|(a b c)~ d"
        "v")
      (lispyville-expect-equal
          "(a |baz~ c) d"
          "(a |baz~ c) d"
        "v"))))

(describe "lispyville-escape"
  (it "should cancel the active region"
    (lispyville-expect-equal
        "|(a b c) d"
        "(a b c)| d"
      "i m ESC")
    (lispyville-expect-equal
        "|(a b c) d"
        "(a ~b| c) d"
      "i m 2 ESC")))

(describe "lispyville-enter-visual-when-marking"
  (before-all
    (lispyville-enter-visual-when-marking))
  (after-all
    (lispyville-remove-marking-hooks))
  (it "should cause marking to always enter visual state"
    (lispyville-expect-equal
        "|(a b c) d"
        "~(a b c|) d"
      "im")))

(describe "lispyville-enter-special-when-marking"
  (before-all
    (lispyville-enter-special-when-marking))
  (after-all
    (lispyville-remove-marking-hooks))
  (it "should cause marking to always enter special"
    (lispyville-expect-equal
        "|(a b c) d"
        "~(a b c)| d"
      (lispy-mark-list 1))))

(describe "lispyville-wrap-command should allow creating:"
  (describe "lispyville-wrap-lispy-mark-symbol-visual"
    (before-all
      (lispyville--define-key '(normal visual) "v"
                              (lispyville-wrap-command lispy-mark-symbol visual)))
    (after-all
      (lispyville--define-key 'normal "v" nil))
    (it "should call `lispy-mark-symbol' and then enter visual state"
      (lispyville-expect-equal
          "(|foo bar) baz qux"
          "(~fo|o bar) baz qux"
        "v")
      (lispyville-expect-equal
          "(~fo|o bar) baz qux"
          "(~foo ba|r) baz qux"
        "v")
      (lispyville-expect-equal
          "(~foo ba|r) baz qux"
          "(foo bar) ~ba|z qux"
        "v")))
  (describe "lispyville-wrap-lispy-mark-symbol-special"
    (before-all
      (lispyville--define-key '(normal visual) "v"
                              (lispyville-wrap-command lispy-mark-symbol special)))
    (after-all
      (lispyville--define-key 'normal "v" nil))
    (it  "should call `lispy-mark-symbol' and then enter special"
      (lispyville-expect-equal
          "(|foo bar) baz qux"
          "(~foo| bar) baz qux"
        "v")
      (lispyville-expect-equal
          "(~fo|o bar) baz qux"
          "(~foo bar|) baz qux"
        "v")
      (lispyville-expect-equal
          "(~foo ba|r) baz qux"
          "(foo bar) ~baz| qux"
        "v")
      (lispyville-expect-equal
          "(foo |bar) baz qux"
          "(foo ~bar|) baz qux"
        "v"))))

(provide 'test-lispyville)
;;; test-lispyville.el ends here
