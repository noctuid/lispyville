;;; lispyville.el --- A minor mode for integrating evil with lispy. -*- lexical-binding: t -*-

;; Author: Fox Kiester <noct@posteo.net>
;; URL: https://github.com/noctuid/lispyville
;; Created: March 03, 2016
;; Keywords: vim, evil, lispy, lisp, parentheses
;; Package-Requires: ((lispy "0") (evil "1.2.12") (cl-lib "0.5") (emacs "24.4"))
;; Version: 0.1

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
;; For more information see the README in the online repository.

;;; Code:
(require 'evil)
(require 'lispy)
(require 'cl-lib)

;; * Settings
(defgroup lispyville nil
  "Provides a minor mode to integrate evil with lispy."
  :group 'lispy
  :prefix "lispyville-")

(defcustom lispyville-key-theme '(operators c-w)
  "Determines the key theme initially set by lispyville.
Changing this variable will only have an effect by itself when done prior to
lispyville being loaded. Otherwise, `lispyville-set-key-theme' should be
called afterwards with no arguments. The user can also not set this variable
at all and simply use `lispyville-set-key-theme' with an argument after
lispyville has been loaded."
  :type
  '(repeat :tag "Key Themes"
     (choice
      (const
       :tag "Safe versions of evil operators."
       operators)
      (const
       :tag "Safe version of `evil-delete-backward-word'."
       c-w)
      (const
       :tag "Safe version of `evil-delete-back-to-indentation'."
       c-u)
      (const
       :tag "Alternative to `evil-indent' that acts like `lispy-tab'."
       prettify)
      (const
       :tag "Extra motions similar to those provided by cleverparens."
       additional-movement)
      (const
       :tag "Safe version of `evil-commentary' for toggling comments."
       commentary)
      (const
       :tag "Slurp/barf keybindings in the style of cleverparens."
       slurp/barf-cp)
      (const
       :tag "Slurp/barf keybindings in the style of lispy."
       slurp/barf-lispy)
      (const
       :tag "Extra commands similar to those provided by cleverparens."
       additional)
      (const
       :tag "Extra insert commands similar to those provided by cleverparens."
       additional-insert)
      (const
       :tag "Commands similar to those provided by \
vim-sexp-mappings-for-regular-people."
       arrows)
      (const
       :tag "Command to enter normal state and cancel an active region."
       escape)
      (const
       :tag "Lispy commands for marking."
       mark)
      (const
       :tag "Lispy commands for marking that enter special instead of visual \
state."
       mark-special)
      (const
       :tag "Commands for toggling between special and visual state and \
canceling a region."
       mark-toggle)
      (const
       :tag "Insert a space when inserting after an opening delimiter or \
before a closing one."
       insert))))

(defcustom lispyville-barf-stay-with-closing nil
  "When non-nil, stay with the closing delimiter when barfing.
Specifically, this applies for `lispyville-barf' and `lispyville-<'
when barfing would move the delimiter behind the point. This option
only has an effect if `lispyville-commands-put-into-special' is nil."
  :type 'boolean)

(defvaralias 'lispyville-preferred-state 'lispyville-preferred-lispy-state)

(defcustom lispyville-preferred-lispy-state 'insert
  "The preferred evil state for insertion and using lispy.
This is used by any command that should enter special to determine the correct
state."
  :type '(choice
          (const :tag "Use insert state to get into special." insert)
          (const :tag "Use emacs state to get into special." emacs)))

(defcustom lispyville-motions-put-into-special nil
  "Applicable motions will enter insert or emacs state.
This will only happen when they are not called with an operator or in visual
mode."
  :type 'boolean)

(defcustom lispyville-commands-put-into-special nil
  "Applicable commands will enter insert or emacs state."
  :type 'boolean)

(defcustom lispyville-no-alter-lispy-options nil
  "Whether to to change certain lispy options when entering `lispyville-mode'.
By default, lispyville will set `lispy-safe-delte', `lispy-safe-copy',
`lispy-safe-delete', and `lispy-safe-actions-no-pull-delimiters-into-comments'
to t. To prevent lispyville from changing any lispy options, set this variable
to a non-nil value."
  :type 'boolean)

(defcustom lispyville-insert-states '(insert emacs hybrid iedit-insert)
  "Insertion states that lispy special can be used from."
  :type 'list)

(defcustom lispyville-want-change-atom-to-end 'evil-want-change-word-to-end
  "Whether `lispyville-forward-atom-begin' should change to the end of atoms.
`evil-want-change-word-to-end' means use the value of
`evil-want-change-word-to-end'. "
  :type '(choice
          boolean
          (const :tag "Use evil-want-change-word-to-end"
            evil-want-change-word-to-end)))

(defface lispyville-special-face
  '((t :foreground "#aa4456"))
  "Face for lispyville special mode line indicator.")

(with-eval-after-load 'evil-surround
  (add-to-list 'evil-surround-operator-alist '(lispyville-change . change))
  (add-to-list 'evil-surround-operator-alist '(lispyville-delete . delete)))

;; https://github.com/noctuid/lispyville/pull/26
(when (boundp 'evil-change-commands)
  (add-to-list 'evil-change-commands #'lispyville-change))

;;;###autoload
(define-minor-mode lispyville-mode
  "A minor mode for integrating evil with lispy."
  :lighter " LYVLE"
  :keymap (make-sparse-keymap)
  (when lispyville-mode
    (evil-normalize-keymaps)
    (unless lispyville-no-alter-lispy-options
      (setq lispy-safe-delete t
            lispy-safe-copy t
            lispy-safe-paste t
            lispy-safe-actions-no-pull-delimiters-into-comments t))))

;; * Helpers
(defun lispyville--in-string-p ()
  "Return the bounds of the string at the point or nil.
Unlike `lispy--in-string-p', |\"\" is not considered to be inside the string."
  (let ((string-bounds (lispy--bounds-string)))
    (and string-bounds
         (not (= (car string-bounds) (point)))
         string-bounds)))

(defun lispyville--at-left-p ()
  "Return whether the point is before an opening delimiter.
Opening delimiters inside strings and comments are ignored."
  (and (not (lispy--in-comment-p))
       (not (lispyville--in-string-p))
       (or (looking-at lispy-left)
           (looking-at "\""))
       (not (looking-back "\\\\" (- (point) 2)))))

(defun lispyville--at-right-p ()
  "Return whether the point is before a closing delimiter.
Closing delimiters inside strings and comments are ignored."
  (and (not (lispy--in-comment-p))
       (or (and (looking-at lispy-right)
                (not (lispyville--in-string-p)))
           (and (looking-at "\"")
                (lispyville--in-string-p)))
       (not (looking-back "\\\\" (- (point) 2)))))

(defun lispyville--after-left-p ()
  "Return whether the point is after an opening delimiters.
Opening delimiters inside strings and comments are ignored."
  (unless (bobp)
    (save-excursion
      (backward-char)
      (lispyville--at-left-p))))

(defun lispyville--after-delimiter-p ()
  "Return whether the point is after an opening or closing delimiter."
  (unless (bobp)
    (let ((lispy-delimiters (concat (substring lispy-right 0 -1)
                                    "\""
                                    (substring lispy-left 1))))
      (and (not (lispy--in-string-or-comment-p))
           (lispy-looking-back lispy-delimiters)
           (save-excursion
             (backward-char)
             (not (looking-back "\\\\" (- (point) 2))))))))

(defun lispyville--yank-text (text &optional type register yank-handler)
  "Like `evil-yank-characters' but takes TEXT directly instead of a region.
Depending on TYPE, this will behave like `evil-yank-characters',
`evil-yank-lines', or `evil-yank-rectangle'. REGISTER and YANK-HANDLER have the
same effect."
  (let ((lines (split-string text "\n")))
    (setq yank-handler
          (cl-case type
            (block (list (or yank-handler #'evil-yank-block-handler)
                         lines
                         t
                         'evil-delete-yanked-rectangle))
            (line (list (or yank-handler
                            #'evil-yank-line-handler)
                        nil
                        t))
            (t (when yank-handler
                 (list yank-handler)))))
    ;; ensure newline at end of text for line type
    (when (and (eq type 'line)
               (or (zerop (length text))
                   (/= (aref text (1- (length text))) ?\n)))
      (setq text (concat text "\n")))
    (when yank-handler
      (setq text (propertize text 'yank-handler yank-handler)))
    (when register
      (evil-set-register register text))
    (when evil-was-yanked-without-register
      (evil-set-register ?0 text))
    (unless (eq register ?_)
      (kill-new text))))

(defun lispyville--safe-string (beg end &optional delete)
  "Return the text between from BEG to END excluding unmatched delimiters.
When DELETE is non-nil, delete the safe part of the region."
  (let ((safe-regions (lispy--find-safe-regions beg end))
        safe-strings)
    (dolist (safe-region safe-regions)
      ;; NOTE evil uses `filter-buffer-substring'
      (push (filter-buffer-substring (car safe-region) (cdr safe-region))
            safe-strings)
      (when delete
        (delete-region (car safe-region) (cdr safe-region))))
    (apply #'concat safe-strings)))

;; TODO splice strings that aren't double quotes or have multiple delimiters on
;; either side
(defun lispyville--splice-string ()
  "Splice a string at the point."
  (cond ((and (not (bobp))
              (save-excursion
                (backward-char)
                (lispy--in-string-p)))
         (let ((right-quote (point)))
           (lispy--exit-string)
           (save-excursion
             (goto-char right-quote)
             (delete-char 1))
           (delete-char 1)))
        (t
         (delete-char 1)
         (while (progn (re-search-forward "\"" nil t)
                       (looking-back "\\\\\"" (- (point) 2))))
         (delete-char -1))))

(defun lispyville--splice ()
  "Like `lispy-splice' but also splice strings."
  (save-excursion
    (if (looking-at "\"")
        (lispyville--splice-string)
      (when (looking-at lispy-right)
        (forward-char))
      (lispy-splice 1))))

(defun lispyville--safe-string-splice (beg end)
  "Like `lispyville--safe-string' except always deletes.
Instead of ignoring unmatched delimiters between BEG and END, this will splice
them."
  (let ((beg-marker (save-excursion
                      (goto-char beg)
                      (point-marker)))
        (end-marker (save-excursion
                      (goto-char end)
                      (point-marker)))
        (unmatched-positions (lispy--find-unmatched-delimiters beg end))
        safe-text)
    (evil-exit-visual-state)
    (save-excursion
      (dolist (pos unmatched-positions)
        (goto-char pos)
        (lispyville--splice))
      (setq safe-text (filter-buffer-substring beg-marker end-marker))
      (delete-region beg-marker end-marker)
      safe-text)))

(defvar lispyville--safe-strings nil
  "Temporary list of safe strings for `lispyville--safe-string-rectangle'.")

(defun lispyville--safe-string-rectangle
    (beg end &optional delete)
  "Like `lispyville--safe-string' but operates on a rectangle/block.
BEG and END correspond to the start and beginning of the rectangle. DELETE is
passed to `lispyville--safe-string'. If DELETE is the symbol 'splice,
`lispyville--safe-string-splice' will be used instead."
  (setq lispyville--safe-strings nil)
  (apply-on-rectangle
   (lambda (beg end delete)
     (setq beg (save-excursion (move-to-column beg) (point)))
     (setq end (save-excursion (move-to-column end) (point)))
     (push (if (eq delete 'splice)
               (lispyville--safe-string-splice beg end)
             (lispyville--safe-string beg end delete))
           lispyville--safe-strings))
   beg end delete)
  (apply #'concat
         (lispy-interleave
          "\n"
          (nreverse lispyville--safe-strings))))

(defun lispyville--safe-manipulate
    (beg end &optional type delete yank register yank-handler)
  "Return the text from BEG to END excluding unmatched delimiters.
When DELETE is non-nil, delete the safe part of the region. When DELETE is the
symbol 'splice, splice unmatched delimiters instead of ignoring them. When YANK
is non-nil, also copy the safe text to the kill ring. REGISTER and YANK-HANDLER
are passed to `lispyville--yank-text' when YANK is non-nil."
  ;; special 'splice value
  (let ((safe-string
         (cl-case type
           (block (lispyville--safe-string-rectangle beg end delete))
           (t (if (eq delete 'splice)
                  (lispyville--safe-string-splice beg end)
                (lispyville--safe-string beg end delete))))))
    (when yank
      (lispyville--yank-text safe-string type register yank-handler))
    safe-string))

(defun lispyville--state-transition (&optional to-special)
  "Transition from lispy special to evil normal state.
If the region is active, transition to visual state. If TO-SPECIAL is non-nil,
transition to lispy special instead. This function will ensure that the point is
correctly on or after a closing delimiter at the end of the transition."
  (let ((regionp (region-active-p))
        (mark (mark t)))
    (cond (to-special
           (cond (regionp)
                 ((looking-at lispy-right)
                  (forward-char))
                 ((not (or (lispy-left-p)
                           (lispy-right-p)))
                  (lispy-right 1)))
           (evil-change-state lispyville-preferred-lispy-state)
           (when regionp
             (set-mark mark)))
          (t
           (let* ((point-last-p (> (point) mark))
                  evil-move-cursor-back)
             ;; if region active, this prevents from entering insert after exiting
             ;; visual state
             (evil-normal-state nil)
             (when (or (and regionp point-last-p)
                       (lispy-right-p))
               (backward-char))
             (when regionp
               (if point-last-p
                   (evil-visual-char mark (point))
                 (evil-visual-char (1- mark) (point)))))))))

(defun lispyville--maybe-enter-special (&optional command)
  "Potentially enter insert or emacs state to get into special.
The behavior depends on the value of `lispyville-motions-put-into-special'. If
COMMAND is non-nil, the behavior depends on the value of
`lispyville-commands-put-into-special' instead.
`lispyville-preferred-lispy-state' is used to determine whether to enter emacs
or insert state."
  (when (and (if command
                 lispyville-commands-put-into-special
               lispyville-motions-put-into-special)
             (not (or (evil-operator-state-p)
                      (evil-visual-state-p))))
    (lispyville--state-transition t)))

;; * Operators
(evil-define-operator lispyville-yank (beg end type register yank-handler)
  "Like `evil-yank' but will not copy unmatched delimiters."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (let ((evil-was-yanked-without-register
         (and evil-was-yanked-without-register (not register))))
    (lispyville--safe-manipulate beg end type nil t register yank-handler)))

;; NOTE: Y, D, and C don't work with counts in evil by default already
(evil-define-operator lispyville-yank-line (beg end type register yank-handler)
  "Copy to the end of the line ignoring unmatched delimiters.
This is not like the default `evil-yank-line'."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  (let ((beg (or beg (point)))
        (end (or end beg)))
    ;; act linewise in Visual state
    (when (evil-visual-state-p)
      (unless (memq type '(line block))
        (let ((range (evil-expand beg end 'line)))
          (setq beg (evil-range-beginning range)
                end (evil-range-end range)
                type (evil-type range))))
      (evil-exit-visual-state))
    (cond ((eq type 'block)
           (let ((temporary-goal-column most-positive-fixnum)
                 (last-command 'next-line))
             (lispyville-yank beg end type register yank-handler)))
          ((eq type 'line)
           (lispyville-yank beg end type register yank-handler))
          (t
           (lispyville-yank beg (line-end-position)
                            type register yank-handler)))))

(defun lispyville--maybe-newline-and-indent ()
  "Skip closing delimiters and call `newline-and-indent' with a trailing sexp.
Only add a newline if there is a sexp after the closing delimiters. Don't move
the point."
  (save-excursion
    (while (progn (forward-char)
                  (lispyville--at-right-p)))
    (unless (looking-at "[[:space:]]*$")
      (newline-and-indent))))

(defvar lispyville--cc nil
  "Whether `lispyville-delete' has been called from `lispyville-change'.")

(evil-define-operator lispyville-delete (beg end type register yank-handler)
  "Like `evil-delete' but will not delete/copy unmatched delimiters."
  (interactive "<R><x><y>")
  (unless register
    (let ((text (filter-buffer-substring beg end)))
      (unless (string-match-p "\n" text)
        ;; set the small delete register
        (evil-set-register ?- (lispyville--safe-manipulate beg end)))))
  (let ((evil-was-yanked-without-register nil))
    (cond
     ((and (eq type 'line)
           (not lispyville--cc))
      ;; at end of buffer delete previous newline since no newline after
      ;; current line
      (when (and
             ;; check that there is no final newline
             (= end (point-max))
             (or (= beg end)
                 (/= (char-before end) ?\n))
             ;; there must be a previous newline
             (/= beg (point-min))
             (= (char-before beg) ?\n))
        (setq beg (1- beg)))
      (lispyville--safe-manipulate beg end type t t register yank-handler)
      (when (and (lispyville--at-right-p)
                 (lispy-looking-back "^[[:space:]]*"))
        (cond ((save-excursion
                 (forward-line -1)
                 (goto-char (line-end-position))
                 (lispy--in-comment-p))
               (lispy--indent-for-tab)
               ;; don't pull delimiter(s) into comment
               (lispyville--maybe-newline-and-indent))
              (t
               (join-line)
               (while (progn (forward-char)
                             (lispyville--at-right-p)))
               (newline-and-indent))))
      (when (lispyville--at-left-p)
        ;; remove any extra whitespace (e.g. unmatched opening delimeter;
        ;; pulling up indented code)
        (save-excursion
          (lispyville-first-non-blank)
          (delete-region (point)
                         (progn
                           (skip-chars-forward "[:space:]")
                           (point)))))
      (when (called-interactively-p 'any)
        (evil-first-non-blank)))
     (t
      (lispyville--safe-manipulate beg end type t t register yank-handler)))))

(evil-define-operator lispyville-delete-line
  (beg end type register yank-handler)
  "Like `evil-delete-line' but will not delete/copy unmatched delimiters."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  (let* ((beg (or beg (point)))
         (end (or end beg)))
    ;; act linewise in Visual state
    (when (evil-visual-state-p)
      (unless (memq type '(line block))
        (let ((range (evil-expand beg end 'line)))
          (setq beg (evil-range-beginning range)
                end (evil-range-end range)
                type (evil-type range))))
      (evil-exit-visual-state))
    (cond ((eq type 'block)
           (let ((temporary-goal-column most-positive-fixnum)
                 (last-command 'next-line))
             (lispyville-delete beg end 'block register yank-handler)))
          ((eq type 'line)
           (lispyville-delete beg end type register yank-handler)
           (when (called-interactively-p 'any)
             (evil-first-non-blank)))
          (t
           (lispyville-delete beg (line-end-position)
                              type register yank-handler)))))

(evil-define-operator lispyville-delete-whole-line
  (beg end type register yank-handler)
  "Like `evil-delete-whole-line' but will not delete/copy unmatched delimiters."
  :motion evil-line
  (interactive "<R><x>")
  (lispyville-delete beg end type register yank-handler))

(evil-define-operator lispyville-delete-char-or-splice
  (beg end type register yank-handler)
  "Deletes and copies the region by splicing unmatched delimiters."
  :motion evil-forward-char
  (interactive "<R><x>")
  (lispyville--safe-manipulate beg end type 'splice t register yank-handler))

(evil-define-operator lispyville-delete-char-or-splice-backwards
  (beg end type register yank-handler)
  "Like `lispyville-delete-char-or-splice' but acts on the preceding character."
  :motion evil-backward-char
  (interactive "<R><x>")
  (lispyville-delete-char-or-splice beg end type register yank-handler))

(evil-define-command lispyville-delete-backward-word ()
  "Like `evil-delete-backward-word' but will not delete unmatched delimiters.
This will also act as `lispy-delete-backward' after delimiters."
  (cond ((and (bolp) (not (bobp)))
         (unless evil-backspace-join-lines
           (user-error "Beginning of line"))
         (delete-char -1))
        ((lispyville--after-delimiter-p)
         (lispy-delete-backward 1))
        (t
         (lispyville-delete (max
                             (save-excursion
                               (evil-backward-word-begin)
                               (point))
                             (line-beginning-position))
                            (point)
                            'exclusive))))

(evil-define-command lispyville-delete-back-to-indentation ()
  "Like `evil-delete-back-to-indentation' but will not delete unmatched delimiters.
This will also act as `lispy-delete-backward' after delimiters."
  (cond ((bolp)
         (evil-delete-backward-char-and-join 1))
        ((lispyville--after-delimiter-p)
         (lispy-delete-backward 1))
        (t
         (lispyville-delete (if (<= (current-column) (current-indentation))
                                (line-beginning-position)
                              (save-excursion
                                (evil-first-non-blank)
                                (point)))
                            (point)
                            'exclusive))))

(defun lispyville--open-here (count)
  "Like `evil-open-above' except inserts at the point."
  (interactive "p")
  (unless (eq evil-want-fine-undo t)
    (evil-start-undo-step))
  (push (point) buffer-undo-list)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (evil-insert-state 1)
  (when evil-auto-indent
    (indent-according-to-mode)))

(evil-define-operator lispyville-change
  (beg end type register yank-handler delete-func)
  "Like `evil-change' but will not delete/copy unmatched delimiters."
  (interactive "<R><x><y>")
  ;; differences from `evil-change' are commented
  (let ((delete-func (or delete-func #'lispyville-delete))
        (nlines (1+ (evil-count-lines beg end)))
        (opoint (save-excursion
                  (goto-char beg)
                  (line-beginning-position)))
        (unmatched (lispy--find-unmatched-delimiters
                    (line-beginning-position)
                    (line-end-position)))
        (lispyville--cc t))
    (unless (eq evil-want-fine-undo t)
      (evil-start-undo-step))
    (funcall delete-func beg end type register yank-handler)
    (cl-case type
      (line
       (cond ((lispyville--at-right-p)
              (lispy--indent-for-tab)
              (save-excursion
                (while (progn (forward-char)
                              (lispyville--at-right-p)))
                (newline-and-indent))
              (evil-insert 1))
             ((and unmatched
                   (lispyville--at-left-p))
              (while (progn (forward-char)
                            (lispyville--at-left-p)))
              (save-excursion
                (newline-and-indent))
              (evil-insert 1))
             ((= opoint (point))
              (evil-open-above 1))
             (t
              (newline-and-indent)
              (evil-insert 1))))
      (block (evil-insert 1 nlines))
      (t
       (evil-insert 1)))))

(evil-define-operator lispyville-change-line
  (beg end type register yank-handler)
  "Like `evil-change-line' but will not delete/copy unmatched delimiters."
  :motion evil-end-of-line
  (interactive "<R><x><y>")
  (lispyville-change beg end type register yank-handler
                     #'lispyville-delete-line))

(evil-define-operator lispyville-change-whole-line
  (beg end type register yank-handler)
  "Change whole line while respecting parentheses."
  :motion evil-line
  (interactive "<R><x>")
  (lispyville-change beg end type register yank-handler
                     #'lispyville-delete-whole-line))

(evil-define-operator lispyville-substitute (beg end type register)
  "Acts like `lispyville-change' (cl when not in visual mode)."
  :motion evil-forward-char
  (interactive "<R><x>")
  (lispyville-change beg end type register))

(defun lispyville--forward-list (&optional arg)
  "Like `forward-list' but error if no more lists.
ARG has the same effect."
  (interactive "^p")
  (or arg (setq arg 1))
  (let ((pos (scan-lists (point) arg 0)))
    (if pos
        (goto-char pos)
      (error "No more lists"))))

(evil-define-operator lispyville-prettify (_beg end)
  "Prettify lists from BEG to END."
  :move-point nil
  (interactive "<r>")
  (let ((orig-pos (point)))
    (evil-exit-visual-state)
    (lispy--out-backward 1)
    (while (and (ignore-errors (lispyville--forward-list))
                (<= (save-excursion
                      (backward-list))
                    end))
      (lispy--normalize-1))
    (goto-char orig-pos)))

(evil-define-operator lispyville-comment-or-uncomment (beg end type)
  "Like `evil-commentary' or `evilnc-comment-operator' but handles unmatched delimiters.
May insert newlines where necessary in order to move delimiters outside of the
commented regions and preserve the balanced structure. "
  :move-point nil
  (interactive "<R>")
  (let ((end-marker (copy-marker (1+ end)))
        (comment-combine-change-calls nil)) ;; workaround for undo-tree corruption issue in Emacs 27
    (dolist (r (lispy--find-safe-regions beg end))
      (comment-or-uncomment-region (car r) (cdr r)))
    (delete-trailing-whitespace beg end-marker)))

(evil-define-operator lispyville-comment-or-uncomment-line (beg end type)
  "Comment or uncomment lines while preserving structure."
  :motion evil-line
  :move-point nil
  (interactive "<R>")
  (lispyville-comment-or-uncomment beg end type))

(evil-define-operator lispyville-comment-and-clone-dwim (beg end type)
  "Insert commented out copies of the right-most balanced portion within the specified region.
If the region is active (visual selection), apply to all subregions, not only the rightmost one."
  :move-point nil
  (interactive "<R>")
  (save-excursion
    (let ((end-marker (copy-marker (1+ end)))
          (reg-active (region-active-p))
          (comment-combine-change-calls nil)) ;; workaround for undo-tree corruption issue in Emacs 27
      (when reg-active
        (evil-normal-state))
      (cl-loop
       for (rb . re) in (lispy--find-safe-regions beg end) do
       (let ((str (buffer-substring-no-properties rb re)))
         (goto-char rb)
         (insert str)
         (when (> (length (string-trim str)) 0)
           (comment-region rb (+ rb (length str)))
           (unless reg-active
             (cl-return)))))
      (indent-region beg end-marker))))

(defun lispyville--join-between-comments ()
  "Insert the contents of the next line between the non-commented
and commented regions of the current line.
If both lines have inline comments, place them after another in their original order,
and do not strip the comment characters."
  (let ((inline-comment
         (delete-and-extract-region
          (save-excursion (forward-line 0)
                          (comment-search-forward (point-at-eol) 'no-error))
          (point-at-eol))))
    (join-line 1)
    (save-excursion
      (when (re-search-forward
             (concat "[[:blank:]]*" comment-start-skip)
             (point-at-eol) 'no-error)
        (goto-char (match-beginning 0)))
      (insert " " inline-comment)))
  (indent-according-to-mode))

(evil-define-operator lispyville-join (beg end)
  "Join the next line keeping inline comments out of the way.
If region is active, join all selected lines into one and splice all comments to the end."
  :motion evil-line
  (let ((count (count-lines beg end)))
    (when (> count 1)
      (setq count (1- count)))
    (goto-char beg)
    (dotimes (_ count)
      (if (save-excursion (end-of-line) (lispy--in-comment-p))
          ;; Current line is a comment or ends in an inline comment
          (cond
           ;; Edge case: end of buffer, do nothing
           ((= (point-at-eol) (point-max)) nil)
           ;; Empty line or interior of multi-line comment - join normally
           ((save-excursion
              (forward-line 1)
              (or (nth 4 (syntax-ppss))
                  (looking-at-p "[[:space:]]*$")))
            (join-line 1))
           ;; Entire next line is also a comment - splice them together.
           ((save-excursion
              (forward-line 1)
              (looking-at (concat "[[:space:]]*" comment-start-skip)))
            (end-of-line)
            (delete-region (point) (match-end 0))
            (fixup-whitespace))
           ;; Next line is non-empty and not a comment:
           ;; splice it between current line and the inline comment
           (t (lispyville--join-between-comments)))
        ;; Normal case, no comments involved
        (join-line 1)))))

;; ** Wrap Key Theme
(evil-define-operator lispyville-wrap-with-round (beg end)
  "Insert ( at BEG and ) at END."
  (interactive "<r>")
  (lispy--mark (cons beg end))
  (lispy-parens nil))

(evil-define-operator lispyville-wrap-with-brackets (beg end)
  "Insert [ at BEG and ] at END."
  (interactive "<r>")
  (lispy--mark (cons beg end))
  (lispy-brackets nil))

(evil-define-operator lispyville-wrap-with-braces (beg end)
  "Insert { at BEG and } at END."
  (interactive "<r>")
  (lispy--mark (cons beg end))
  (lispy-braces nil))

;; * Motions
;; ** Additional Movement Key Theme
(evil-define-motion lispyville-forward-sexp (count)
  "This is an evil motion equivalent of `forward-sexp'."
  ;; TODO maybe forward-sexp-or-comment would be more useful
  ;; TODO also consider having it jump to the next level for failure
  ;; like evil-cp motion does
  (forward-sexp (or count 1)))

(evil-define-motion lispyville-backward-sexp (count)
  "This is an evil motion equivalent of `backward-sexp'."
  (backward-sexp (or count 1)))

(evil-define-motion lispyville-beginning-of-defun (count)
  "This is the evil motion equivalent of `beginning-of-defun'.
This won't jump to the beginning of the buffer if there is no paren there."
  (beginning-of-defun (or count 1))
  (lispyville--maybe-enter-special))

(evil-define-motion lispyville-end-of-defun (count)
  "This is the evil motion equivalent of `end-of-defun'.
This won't jump to the end of the buffer if there is no paren there."
  (when (<= (- (line-end-position)
              (point))
           1)
    (forward-line))
  (end-of-defun (or count 1))
  (re-search-backward lispy-right nil t)
  (lispyville--maybe-enter-special))

(evil-define-motion lispyville-beginning-of-next-defun (count)
  "Goto the beginning of the next top-level sexp COUNT times."
  (end-of-defun (1+ (or count 1)))
  (beginning-of-defun)
  (lispyville--maybe-enter-special))

;; lispy-flow like (and reverse)
(defun lispyville--move-to-delimiter (count &optional type no-string)
  "Move COUNT times to the next TYPE delimiter (right by default).
Move backwards when COUNT is negative. Unlike `evil-cp-next-closing', this won't
jump into comments or strings. When NO-STRING is non-nil, don't jump to string
delimiters. Return nil and don't move the point on failure."
  (setq type (or type 'right))
  (let ((positive (> count 0))
        (regex (concat (if (eq type 'left)
                           (substring lispy-left 0 -1)
                         (substring  lispy-right 0 -1))
                       (if no-string
                           "]"
                         "\"]")))
        (initial-pos (point))
        success)
    (dotimes (_ (abs count))
      (when positive
        (forward-char))
      (while
          (and (if positive
                   (re-search-forward regex nil t)
                 (re-search-backward regex nil t))
               (setq success t)
               (save-excursion
                 (goto-char (match-beginning 0))
                 ;; TODO handle "(" in case with NO-STRING nil
                 (or
                  (and no-string (lispy--in-string-p))
                  (lispy--in-comment-p)
                  (not (if (eq type 'left)
                           (lispyville--at-left-p)
                         (lispyville--at-right-p))))))
        (setq success nil))
      (if success
          (goto-char (match-beginning 0))
        (goto-char initial-pos)))
    (unless (= (point) initial-pos)
      (point))))

(evil-define-motion lispyville-next-opening (count)
  "Move to the next opening delimiter COUNT times."
  (lispyville--move-to-delimiter (or count 1) 'left)
  (unless (looking-at "\"")
    (lispyville--maybe-enter-special)))

(evil-define-motion lispyville-previous-opening (count)
  "Move to the previous opening delimiter COUNT times."
  (lispyville--move-to-delimiter (- (or count 1)) 'left)
  (unless (looking-at "\"")
    (lispyville--maybe-enter-special)))

(evil-define-motion lispyville-next-closing (count)
  "Move to the next closing delimiter COUNT times."
  (lispyville--move-to-delimiter (or count 1))
  (unless (looking-at "\"")
    (lispyville--maybe-enter-special)))

(evil-define-motion lispyville-previous-closing (count)
  "Move to the previous closing delimiter COUNT times."
  (lispyville--move-to-delimiter (- (or count 1)))
  (unless (looking-at "\"")
    (lispyville--maybe-enter-special)))

(evil-define-motion lispyville-backward-up-list (count)
  "This is the evil motion equivalent of `lispy-left' (or `backward-up-list').
This is comparable to `evil-cp-backward-up-sexp'. It does not have lispy's
behavior on outlines."
  (lispy-left (or count 1))
  (lispyville--maybe-enter-special))

(defalias 'lispyville-left 'lispyville-backward-up-list)

(evil-define-motion lispyville-up-list (count)
  "This is the evil motion equivalent of `lispy-right' (or `up-list').
This is comparable to `evil-cp-up-sexp'. It does not have lispy's behavior
on outlines. Unlike `up-list', it will keep the point on the closing delimiter."
  (forward-char)
  (lispy-right (or count 1))
  (backward-char)
  (lispyville--maybe-enter-special))

(defalias 'lispyville-right 'lispyville-up-list)

;; * Text Objects Key Theme
;; ** Atom (non-nestable)
;; save-excursion was changed to save-mark-and-excursion in 25.1
(eval-when-compile
  (when (or (< emacs-major-version 25)
            ;; unlikely since 25.1 was the first official release
            (and (= emacs-major-version 25) (< emacs-minor-version 1)))
    (defmacro save-mark-and-excursion (&rest body)
      `(save-excursion ,@body))))

(defun lispyville--bounds-atom ()
  "Return the bounds of the atom at point or nil."
  (let* ((comment-bounds (lispy--bounds-comment))
         (string-bounds (lispyville--bounds-string))
         ;; includes quote unlike symbol
         (sexp-bounds (save-excursion
                        (when (looking-at lispy-right)
                          (backward-char))
                        (bounds-of-thing-at-point 'sexp)))
         (atom-bounds (when (and sexp-bounds
                                 (not (equal sexp-bounds
                                             (lispy--bounds-list))))
                        sexp-bounds)))
    (or comment-bounds string-bounds atom-bounds)))
(put 'lispyville-atom 'bounds-of-thing-at-point #'lispyville--bounds-atom)

(evil-define-motion lispyville-forward-atom-begin (count)
  "Go to the next atom or comment beginning COUNT times."
  (if (and (if (eq lispyville-want-change-atom-to-end 'evil-want-change-word-to-end)
               evil-want-change-word-to-end
             lispyville-want-change-atom-to-end)
           (memq evil-this-operator evil-change-commands))
      (lispyville-forward-atom-end count)
    (or count (setq count 1))
    (if (< count 0)
        (lispyville-backward-atom-begin (- count))
      (cl-dotimes (_ count)
        (let ((orig-pos (point)))
          ;; move past the current atom
          (ignore-errors (end-of-thing 'lispyville-atom))
          ;; move to next atom
          (forward-symbol 1)
          (unless (and (ignore-errors (beginning-of-thing 'lispyville-atom))
                       (not (<= (point) orig-pos)))
            (goto-char orig-pos)
            (cl-return)))))))

(evil-define-motion lispyville-backward-atom-begin (count)
  "Go to the previous atom or comment beginning COUNT times. "
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-forward-atom-begin (- count))
    (cl-dotimes (_ count)
      (let ((orig-pos (point)))
        (forward-symbol -1)
        (unless (and (ignore-errors (beginning-of-thing 'lispyville-atom))
                     (not (>= (point) orig-pos)))
          (goto-char orig-pos)
          (cl-return))))))

(evil-define-motion lispyville-forward-atom-end (count)
  "Go to the next atom or comment end COUNT times."
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-backward-atom-end (- count))
    (cl-dotimes (_ count)
      (let ((orig-pos (point)))
        (forward-symbol 1)
        (unless (and (ignore-errors (end-of-thing 'lispyville-atom))
                     (not (<= (point) orig-pos)))
          (goto-char orig-pos)
          (cl-return))))))

(evil-define-motion lispyville-backward-atom-end (count)
  "Go to the previous atom or comment end COUNT times."
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-forward-atom-end (- count))
    (cl-dotimes (_ count)
      (let ((orig-pos (point)))
        ;; move before the current atom
        (ignore-errors (beginning-of-thing 'lispyville-atom))
        ;; move to previous atom
        (forward-symbol -1)
        (unless (and (ignore-errors (end-of-thing 'lispyville-atom))
                     (not (>= (point) orig-pos)))
          (goto-char orig-pos)
          (cl-return))))))

(evil-define-motion lispyville-forward-atom (count)
  "Move forward across an atom COUNT times"
  (or count (setq count 1))
  (let ((orig-pos (point)))
    (if (< count 0)
        (lispyville-backward-atom-begin (- count))
      (lispyville-forward-atom-end count))
    (if (= (point) orig-pos)
        1
      ;; evil wants 0 on success
      0)))
(put 'lispyville-atom 'forward-op #'lispyville-forward-atom)

(evil-define-text-object lispyville-inner-atom (count &optional beg end type)
  "Select inner atom.
This is comparable to `evil-inner-symbol' except that it will select entire
strings and comments."
  (evil-select-inner-object 'lispyville-atom beg end type count))

(evil-define-text-object lispyville-a-atom (count &optional beg end type)
  "Select an atom.
This is comparable to `evil-a-symbol' except that it will select entire
strings and comments."
  (evil-select-an-object 'lispyville-atom beg end type count))

;; ** List (nestable, custom inner)
(defun lispyville--bounds-list ()
  "Return the bounds of the list at point."
  (save-excursion
    (lispy--exit-string)
    (when (looking-at lispy-left)
      (forward-char))
    ;; with )| the point is on the next character
    ;; (when (lispy-looking-back lispy-right)
    ;;   (backward-char))
    (ignore-errors
      (let (beg end)
        (up-list)
        (setq end (point))
        (backward-list)
        (setq beg (point))
        (cons beg end)))))
(put 'lispyville-list 'bounds-of-thing-at-point #'lispyville--bounds-list)

(defun lispyville--shrink-list (range)
  "Shrink evil RANGE to exclude delimiters."
  (save-excursion
    (goto-char (car range))
    (when (looking-at lispy-left)
      (forward-char))
    (setf (car range) (point))
    (goto-char (cadr range))
    ;; (skip-chars-backward "\n")
    (when (lispy-looking-back lispy-right)
      (backward-char))
    (setf (cadr range) (point))
    range))
(put 'lispyville-list 'targets-shrink-inner-op #'lispyville--shrink-list)

(evil-define-motion lispyville-forward-list-begin (count)
  "Go to the next list beginning at any level of nesting COUNT times."
  (lispyville--move-to-delimiter (or count 1) 'left t))

(evil-define-motion lispyville-backward-list-begin (count)
  "Go to the previous list beginning at any level of nesting COUNT times."
  (lispyville--move-to-delimiter (if count (- count) -1) 'left t))

(evil-define-motion lispyville-forward-list-end (count)
  "Go to the next list end at any level of nesting COUNT timse."
  (when (lispyville--move-to-delimiter (or count 1) nil t)
    (forward-char)))

(evil-define-motion lispyville-backward-list-end (count)
  "Go to the previous list end at any level of nesting COUNT times."
  (when (lispyville--move-to-delimiter (if count (- count) -1) nil t)
    (forward-char)))

(evil-define-motion lispyville-forward-list (count)
  "Move forward across a list COUNT times."
  (or count (setq count 1))
  (let ((orig-pos (point)))
    (if (< count 0)
        (lispyville-backward-list-begin (- count))
      (lispyville-forward-list-end count))
    (if (= (point) orig-pos)
        1
      ;; evil wants 0 on success
      0)))
(put 'lispyville-list 'forward-op #'lispyville-forward-list)

(evil-define-motion lispyville-forward-list-alt (count)
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-backward-list-begin (- count))
    (lispyville-forward-list-begin count)))
(put 'lispyville-list 'targets-seek-op #'lispyville-forward-list-alt)
(put 'lispyville-list 'targets-seeks-forward-begin t)

(defun lispyville--up-list ()
  "Like `up-list' but return non-nil on success."
  (when (region-beginning)
    (goto-char (region-beginning)))
  (let ((orig-pos (point)))
    (ignore-errors (up-list))
    (unless (= (point) orig-pos)
      (point))))
(put 'lispyville-list 'targets-extend-seek-op #'lispyville--up-list)

(evil-define-text-object lispyville-inner-list (count &optional beg end type)
  "Select inner list."
  (let ((range
         (evil-select-inner-object 'lispyville-list beg end type count)))
    (when range
      (lispyville--shrink-list range))))

(evil-define-text-object lispyville-a-list (count &optional beg end type)
  "Select a list."
  (evil-select-inner-object 'lispyville-list beg end type count))

;; ** Sexp (nestable, custom inner)
;; TODO implement as a composite text object
;; e.g. currently outer atom and inner atom are same
(defun lispyville--min-distance-pos (target &rest positions)
  "Return the position that is closest to TARGET from POSITIONS.
Delete any nils from POSITIONS first."
  (setq positions (delq nil positions))
  (when positions
    (cl-reduce (lambda (x y)
                 (if (< (abs (- x target))
                        (abs (- y target)))
                     x
                   y))
               positions)))

(defun lispyville--bounds-sexp ()
  "Return the bounds of the sexp at the point or nil"
  (let ((list-bounds (lispyville--bounds-list))
        (atom-bounds (lispyville--bounds-atom)))
    (if (and list-bounds
             (looking-at lispy-right))
        list-bounds
      (or atom-bounds list-bounds))))
(put 'lispyville-sexp 'bounds-of-thing-at-point #'lispyville--bounds-sexp)

(defun lispyville--shrink-sexp (range)
  (let ((altered-range (lispyville--shrink-list range)))
    (when (equal range altered-range)
      (save-excursion
        (goto-char (cadr range))
        (while (lispy-looking-back "[[:space:]]")
          (backward-char))
        (setf (cadr range) (point))))
    altered-range))
(put 'lispyville-sexp 'targets-shrink-inner-op #'lispyville--shrink-sexp)

(evil-define-motion lispyville-forward-sexp-begin (count)
  "Go to the next sexp beginning COUNT times"
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-backward-sexp-begin (- count))
    (cl-dotimes (_ count)
      (let* ((orig-pos (point))
             (atom-pos (save-excursion
                         (lispyville-forward-atom-begin 1)
                         (unless (= (point) orig-pos)
                           (point))))
             (list-pos (save-excursion
                         (lispyville-forward-list-begin 1)
                         (unless (= (point) orig-pos)
                           (point))))
             (sexp-pos (lispyville--min-distance-pos (point) atom-pos list-pos)))
        (if sexp-pos
            (goto-char sexp-pos)
          (cl-return))))))

(evil-define-motion lispyville-backward-sexp-begin (count)
  "Go to the previous sexp beginning COUNT times."
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-forward-sexp-begin (- count))
    (cl-dotimes (_ count)
      (let* ((orig-pos (point))
             (atom-pos (save-excursion
                         (lispyville-backward-atom-begin 1)
                         (unless (= (point) orig-pos)
                           (point))))
             (list-pos (save-excursion
                         (lispyville-backward-list-begin 1)
                         (unless (= (point) orig-pos)
                           (point))))
             (sexp-pos (lispyville--min-distance-pos (point) atom-pos list-pos)))
        (if sexp-pos
            (goto-char sexp-pos)
          (cl-return))))))

(evil-define-motion lispyville-forward-sexp-end (count)
  "Go to the next sexp end COUNT times."
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-backward-sexp-end (- count))
    (cl-dotimes (_ count)
      (let* ((orig-pos (point))
             (atom-pos (save-excursion
                         (lispyville-forward-atom-end 1)
                         (unless (= (point) orig-pos)
                           (point))))
             (list-pos (save-excursion
                         (lispyville-forward-list-end 1)
                         (unless (= (point) orig-pos)
                           (point))))
             (sexp-pos (lispyville--min-distance-pos (point) atom-pos list-pos)))
        (if sexp-pos
            (goto-char sexp-pos)
          (cl-return))))))

(evil-define-motion lispyville-backward-sexp-end (count)
  "Go to the previous sexp end COUNT times."
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-forward-sexp-end (- count))
    (cl-dotimes (_ count)
      (let* ((orig-pos (point))
             (atom-pos (save-excursion
                         (lispyville-backward-atom-end 1)
                         (unless (= (point) orig-pos)
                           (point))))
             (list-pos (save-excursion
                         (lispyville-backward-list-end 1)
                         (unless (= (point) orig-pos)
                           (point))))
             (sexp-pos (lispyville--min-distance-pos (point) atom-pos list-pos)))
        (if sexp-pos
            (goto-char sexp-pos)
          (cl-return))))))

;; TODO replace other `lispyville-forward-sexp'?
(evil-define-motion lispyville-forward-sexp-alt (count)
  "Move forward across a sexp COUNT times"
  (or count (setq count 1))
  (let ((orig-pos (point)))
    (if (< count 0)
        (lispyville-backward-sexp-begin (- count))
      (lispyville-forward-sexp-end count))
    (if (= (point) orig-pos)
        1
      ;; evil wants 0 on success
      0)))
(put 'lispyville-sexp 'forward-op #'lispyville-forward-sexp-alt)

(evil-define-motion lispyville-forward-sexp-alt2 (count)
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-backward-sexp-begin (- count))
    (lispyville-forward-sexp-begin count)))
(put 'lispyville-sexp 'targets-seek-op #'lispyville-forward-sexp-alt2)
(put 'lispyville-sexp 'targets-seeks-forward-begin t)

(evil-define-text-object lispyville-inner-sexp (count &optional beg end type)
  "Select inner sexp."
  (let ((range
         (evil-select-inner-object 'lispyville-sexp beg end type count)))
    (when range
      (lispyville--shrink-sexp range))))

(evil-define-text-object lispyville-a-sexp (count &optional beg end type)
  "Select a sexp."
  (evil-select-inner-object 'lispyville-sexp beg end type count))

;; ** Function/Top-level Form (non-nestable)
(put 'lispyville-function 'targets-no-extend t)

(put 'lispyville-function 'targets-shrink-inner-op #'lispyville--shrink-list)

(evil-define-motion lispyville-forward-function-begin (count)
  "Go to the next function beginning COUNT times."
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-backward-function-begin (- count))
    (cl-dotimes (_ count)
      (let ((orig-pos (point)))
        (unless (beginning-of-defun -1)
          (goto-char orig-pos)
          (cl-return))))))

(evil-define-motion lispyville-backward-function-begin (count)
  "Go to the previous function beginning COUNT times."
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-forward-function-begin (- count))
    (cl-dotimes (_ count)
      (let ((orig-pos (point)))
        (unless (beginning-of-defun)
          (goto-char orig-pos)
          (cl-return))))))
(put 'lispyville-function 'beginning-op #'lispyville-backward-function-begin)

(evil-define-motion lispyville-forward-function-end (count)
  "Go to the next function end COUNT times."
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-backward-function-end (- count))
    (let ((orig-pos (point)))
      (end-of-defun)
      (unless (= (point) (1+ orig-pos))
        (cl-decf count))
      (cl-dotimes (_ count)
        (let ((orig-pos (point)))
          (end-of-defun)
          ;; `end-of-defun' may move the point on failure
          (unless (bounds-of-thing-at-point 'defun)
            (goto-char orig-pos)
            (cl-return))))
      (unless (= (point) (1+ orig-pos))
        (backward-char)))))
(put 'lispyville-function 'end-op #'lispyville-forward-function-end)

(evil-define-motion lispyville-backward-function-end (count)
  "Go to the next previous end COUNT times."
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-backward-function-end (- count))
    (let ((orig-pos (point)))
      (cl-dotimes (_ count)
        (let ((pos (point)))
          (end-of-defun -1)
          (unless (bounds-of-thing-at-point 'defun)
            (goto-char pos)
            (cl-return))))
      (unless (= (point) orig-pos)
        (backward-char)))))

(evil-define-motion lispyville-forward-function (count)
  "Move forward across a function COUNT times."
  (or count (setq count 1))
  (let ((orig-pos (point)))
    (if (< count 0)
        (lispyville-backward-function-begin (- count))
      (lispyville-forward-function-end count))
    (if (= (point) orig-pos)
        1
      ;; evil wants 0 on success
      0)))
(put 'lispyville-function 'forward-op #'lispyville-forward-function)

(evil-define-text-object lispyville-inner-function
  (count &optional beg end type)
  "Select inner function."
  (let ((range
         (evil-select-inner-object 'lispyville-function beg end type count)))
    (when range
      (lispyville--shrink-list range))))

(evil-define-text-object lispyville-a-function (count &optional beg end type)
  "Select a function."
  (evil-select-inner-object 'lispyville-function beg end type count))

;; ** Comment (non-nestable)
(put 'lispyville-comment 'targets-no-extend t)

(put 'lispyville-comment 'bounds-of-thing-at-point #'lispy--bounds-comment)

(evil-define-motion lispyville-forward-comment-begin (count)
  "Go to the next comment beginning COUNT times."
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-backward-comment-begin (- count))
    (cl-dotimes (_ count)
      (let ((bounds (lispy--bounds-comment)))
        (when bounds
          (goto-char (1+ bounds))))
      (if (re-search-forward "\\s<" nil t)
          (goto-char (match-beginning 0))
        (cl-return)))))

(evil-define-motion lispyville-backward-comment-begin (count)
  "Go to the previous comment beginning COUNT times."
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-forward-comment-begin (- count))
    (cl-dotimes (_ count)
      (let (bounds)
        (if (and (re-search-backward "\\s<" nil t)
                 (setq bounds (lispy--bounds-comment)))
            (goto-char (car bounds))
          (cl-return))))))

(evil-define-motion lispyville-forward-comment-end (count)
  "Go to the next comment end COUNT times."
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-backward-comment-end (- count))
    (cl-dotimes (_ count)
      (let ((bounds (lispy--bounds-comment)))
        (unless (and bounds (not (= (point) (cdr bounds))))
          (unless (and (re-search-forward "\\s<" nil t)
                       (setq bounds (lispy--bounds-comment)))
            (cl-return)))
        (goto-char (cdr bounds))))))

(evil-define-motion lispyville-backward-comment-end (count)
  "Go to the previous comment end COUNT times."
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-forward-comment-begin (- count))
    (cl-dotimes (_ count)
      ;; exit current comment to the left
      (let ((bounds (lispy--bounds-comment)))
        (when bounds
          (goto-char (car bounds))))
      (let (bounds)
        (if (and (re-search-backward "\\s<" nil t)
                 (setq bounds (lispy--bounds-comment)))
            (goto-char (cdr bounds))
          (cl-return))))))

(evil-define-motion lispyville-forward-comment (count)
  "Move forward across a comment COUNT times."
  (or count (setq count 1))
  (let ((orig-pos (point)))
    (if (< count 0)
        (lispyville-backward-comment-begin (- count))
      (lispyville-forward-comment-end count))
    (if (= (point) orig-pos)
        1
      ;; evil wants 0 on success
      0)))
(put 'lispyville-comment 'forward-op #'lispyville-forward-comment)

(evil-define-text-object lispyville-inner-comment (count &optional beg end type)
  "Select inner comment."
  (evil-select-inner-object 'lispyville-comment beg end type count))

(evil-define-text-object lispyville-a-comment (count &optional beg end type)
  "Select a comment."
  (evil-select-an-object 'lispyville-comment beg end type count))

;; ** String (non-nestable, custom inner)
;; prevent selection of area in between strings
(put 'lispyville-string 'targets-no-extend t)

(defvar lispyville-string-regexp (rx (1+ (or (syntax string-quote)
                                             (syntax string-delimiter)))))

(defun lispyville--bounds-string ()
  "Return the bounds of the string at the point or nil. "
  ;; `lispy--bounds-string' doesn't work before single quote
  ;; string beginnings in python for example, or just after
  ;; a string quote
  (or (lispy--bounds-string)
      (save-excursion
        (when (and (looking-at lispyville-string-regexp)
                   (not (lispy--in-string-p)))
          (forward-char))
        (when (and (lispy-looking-back lispyville-string-regexp)
                   (not (lispy--in-string-p)))
          (backward-char))
        (lispy--bounds-string))))
(put 'lispyville-string 'bounds-of-thing-at-point #'lispyville--bounds-string)

(defun lispyville--shrink-string (range)
  "Shrink evil RANGE to exclude string delimiters."
  (save-excursion
    (goto-char (car range))
    ;; can't use `skip-chars-forward' with \\s|
    (when (looking-at lispyville-string-regexp)
      (goto-char (match-end 0)))
    (setf (car range) (point))
    (goto-char (cadr range))
    (while (lispy-looking-back lispyville-string-regexp)
      (backward-char))
    (setf (cadr range) (point))
    range))
(put 'lispyville-string 'targets-shrink-inner-op #'lispyville--shrink-string)

(evil-define-motion lispyville-forward-string-begin (count)
  "Go to the next string beginning COUNT times."
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-backward-string-begin (- count))
    (cl-dotimes (_ count)
      (ignore-errors (end-of-thing 'lispyville-string))
      (let ((orig-pos (point)))
        (while (and (re-search-forward lispyville-string-regexp nil t)
                    (not (lispy--in-string-p))))
        (if (lispy--in-string-p)
            (goto-char (match-beginning 0))
          (goto-char orig-pos)
          (cl-return))))))

(evil-define-motion lispyville-backward-string-begin (count)
  "Go to the previous string beginning COUNT times.  "
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-forward-string-begin (- count))
    (cl-dotimes (_ count)
      (let ((orig-pos (point)))
        (while (and (re-search-backward lispyville-string-regexp nil t)
                    (not (lispy--in-string-p))))
        (let ((beg (lispy--in-string-p)))
          (if beg
              (goto-char beg)
            (goto-char orig-pos)
            (cl-return)))))))

(evil-define-motion lispyville-forward-string-end (count)
  "Go the next string end COUNT times."
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-backward-string-end (- count))
    (cl-dotimes (_ count)
      (let ((orig-pos (point))
            (bounds (lispyville--bounds-string)))
        (unless (and bounds (not (= (point) (cdr bounds))))
          (setq bounds nil)
          (while (and (re-search-forward lispyville-string-regexp nil t)
                      (not (setq bounds (lispyville--bounds-string))))))
        (if bounds
            (goto-char (cdr bounds))
          (goto-char orig-pos)
          (cl-return))))))

(evil-define-motion lispyville-backward-string-end (count)
  "Go the previous string end COUNT times."
  (or count (setq count 1))
  (if (< count 0)
      (lispyville-forward-string-end (- count))
    (cl-dotimes (_ count)
      (let ((orig-pos (point)))
        ;; exit string on the left
        (let ((bounds (lispyville--bounds-string)))
          (when bounds
            (goto-char (car bounds))))
        (let (bounds)
          (while (and (re-search-backward lispyville-string-regexp nil t)
                      (not (setq bounds (lispyville--bounds-string)))))
          (if bounds
              (goto-char (cdr bounds))
            (goto-char orig-pos)
            (cl-return)))))))

(evil-define-motion lispyville-forward-string (count)
  "Move forward across a string COUNT times."
  (or count (setq count 1))
  (let ((orig-pos (point)))
    (if (< count 0)
        (lispyville-backward-string-begin (- count))
      (lispyville-forward-string-end count))
    (if (= (point) orig-pos)
        1
      ;; evil wants 0 on success
      0)))
(put 'lispyville-string 'forward-op #'lispyville-forward-string)

(evil-define-text-object lispyville-inner-string (count &optional beg end type)
  "Select inner string."
  (let ((range
         (evil-select-inner-object 'lispyville-string beg end type count)))
    (when range
      (lispyville--shrink-string range))))

(evil-define-text-object lispyville-a-string (count &optional beg end type)
  "Select a string."
  (evil-select-inner-object 'lispyville-string beg end type count))

;; * Commands
;; TODO make motion
(defun lispyville-first-non-blank ()
  "Like `evil-first-non-blank' but skips opening delimiters.
This is lispyville equivalent of `evil-cp-first-non-blank-non-opening'."
  (interactive)
  (evil-first-non-blank)
  (while (and (<= (point) (point-at-eol))
              (lispyville--at-left-p))
    (forward-char)))

;; ** Slurp/barf Key Themes
(evil-define-command lispyville-> (count)
  "Slurp or barf COUNT times.
When the point is before a opening paren, barf. When the point is before a
closing paren, slurp. Slurp on the right without moving the point when before
neither (unless `lispyville-commands-put-into-special' is non-nil). This is the
lispyville equivalent of `evil-cp->' and `lispy-slurp-or-barf-right'."
  (interactive "<c>")
  (setq count (or count 1))
  (cond ((looking-at lispy-left)
         (lispy-barf count))
        ((looking-at lispy-right)
         (forward-char)
         (lispy-slurp count)
         (backward-char))
        (t
         (save-excursion
           (lispy-right 1)
           (lispy-slurp count))))
  (lispyville--maybe-enter-special t))

(evil-define-command lispyville-< (count)
  "Slurp or barf COUNT times.
When the point is before a opening paren, slurp. When the point is before a
closing paren, Barf. Barf on the right without moving the point when before
neither (unless `lispyville-commands-put-into-special' is non-nil). This is
the lispyville equivalent of `evil-cp-<' and `lispy-slurp-or-barf-left'."
  (interactive "<c>")
  (setq count (or count 1))
  (cond ((looking-at lispy-left)
         (lispy-slurp count))
        ((looking-at lispy-right)
         (forward-char)
         (lispy-barf count)
         (backward-char))
        (t
         (let (saved-pos)
           (save-excursion
             (lispy-right 1)
             (lispy-barf count)
             (setq saved-pos (point)))
           (cond (lispyville-commands-put-into-special
                  (goto-char saved-pos))
                 ((and lispyville-barf-stay-with-closing
                       (< saved-pos (point)))
                  (goto-char (1- saved-pos)))))))
  (lispyville--maybe-enter-special t))

(evil-define-command lispyville-slurp (count)
  "Slurp COUNT times.
When not at an opening or closing paren, slurp on the right without moving the
point (unless `lispyville-commands-put-into-special' is non-nil). This is the
lispyville equivalent of `lispy-slurp'."
  (interactive "<c>")
  (setq count (or count 1))
  (cond ((looking-at lispy-left)
         (lispy-slurp count))
        ((looking-at lispy-right)
         (forward-char)
         (lispy-slurp count)
         (backward-char))
        (t
         (save-excursion
           (lispy-right 1)
           (lispy-slurp count))))
  (lispyville--maybe-enter-special t))

(evil-define-command lispyville-barf (count)
  "Barf COUNT times.
When not at an opening or closing paren, barf on the right without moving the
point (unless `lispyville-commands-put-into-special' is non-nil). This is the
lispyville equivalent of `lispy-barf'."
  (interactive "<c>")
  (setq count (or count 1))
  (cond ((looking-at lispy-left)
         (lispy-barf count))
        ((looking-at lispy-right)
         (forward-char)
         (lispy-barf count)
         (backward-char))
        (t
         (let (saved-pos)
           (save-excursion
             (lispy-right 1)
             (lispy-barf count)
             (setq saved-pos (point)))
           (cond (lispyville-commands-put-into-special
                  (goto-char saved-pos))
                 ((and lispyville-barf-stay-with-closing
                       (< saved-pos (point)))
                  (goto-char (1- saved-pos)))))))
  (lispyville--maybe-enter-special t))

;; ** Additional Commands Key Theme
(defun lispyville--drag (func count)
  "Helper for `lispyville-drag-backward' and `lispyville-drag-forward'."
  (cond ((region-active-p)
         (funcall func count)
         ;; as this should only be used in visual state
         (when (= (point) (region-end))
           (backward-char)))
        ((or (looking-at lispy-left)
             (looking-at lispy-right)
             (lispyville--in-string-p)
             (region-active-p))
         (forward-char)
         (funcall func count)
         (backward-char))
        (t
         (let ((tick (buffer-chars-modified-tick))
               (saved-pos (point))
               offset)
           (lispy-mark-symbol)
           (setq offset (- (point) saved-pos))
           (ignore-errors (funcall func count))
           (deactivate-mark)
           (backward-char offset)
           (when (= tick (buffer-chars-modified-tick))
             (funcall func count))))))

(evil-define-command lispyville-drag-forward (count)
  "Move the current atom or list forward COUNT times.
If not possible to move the current atom forward, move the current list forward.
This is the lispyville equivalent of `lispy-move-down' and
`evil-cp-drag-forward'."
  (interactive "<c>")
  (lispyville--drag #'lispy-move-down (or count 1)))

(defalias 'lispyville-move-down 'lispyville-drag-forward)

(evil-define-command lispyville-drag-backward (count)
  "Move the current atom or list backward COUNT times.
If not possible to move the current atom back, move the current list back.
This is the lispyville equivalent of `lispy-move-up' and
`evil-cp-drag-backward'."
  (interactive "<c>")
  (lispyville--drag #'lispy-move-up (or count 1)))

(defalias 'lispyville-move-up 'lispyville-drag-backward)

(evil-define-command lispyville-raise-list (count)
  "Raise the current list COUNT times.
This is the lispyville equivalent of `evil-cp-raise-form' except for lists
only."
  (interactive "<c>")
  (save-excursion
    ;; unlike `backward-up-list', works in string
    (when (lispy--out-backward 1)
      (lispy-raise (or count 1)))))

;; ** Additional Insert Key Theme
(evil-define-command lispyville-insert-at-beginning-of-list (count)
  "Enter `lispyville-preferred-state' at the beginning of the current list.
With COUNT, move backward/out COUNT lists first. This is the lispyville
equivalent of `evil-cp-insert-at-beginning-of-form' except for lists only."
  (interactive "<c>")
  (when (lispy--out-backward (or count 1))
    (forward-char)
    (evil-change-state lispyville-preferred-state)))

(defun lispyville--out-forward (count)
  "Like `lispyville--out-forward' but don't return nil if move at least once.
COUNT is passed to `lispy--out-forward'."
  (let ((orig-pos (point)))
    (lispy--out-forward count)
    (not (= (point) orig-pos))))

(evil-define-command lispyville-insert-at-end-of-list (count)
  "Enter `lispyville-preferred-state' at the end of the current list.
With COUNT, move forward/out COUNT lists first. This is the lispyville
equivalent of `evil-cp-insert-at-end-of-form' except for lists only."
  (interactive "<c>")
  (when (lispyville--out-forward (or count 1))
    (backward-char)
    (evil-change-state lispyville-preferred-state)))

(defun lispyville--top-level-p ()
  "Return whether the point is at the top level."
  (= (car (syntax-ppss)) 0))

(evil-define-command lispyville-open-below-list (count)
  "Enter `lispyville-preferred-state' below the current list and indent.
With COUNT, move forward/out COUNT lists first. When exiting to the top-level,
insert in between two newlines. This is the lispyville equivalent of
`evil-cp-open-below-form' except for lists only. This is somewhat comparable to
`lispy-out-forward-newline' as well"
  (interactive "<c>")
  (when (lispyville--out-forward (or count 1))
    (newline-and-indent)
    (when (lispyville--top-level-p)
      (insert "\n"))
    (evil-change-state lispyville-preferred-state)))

(evil-define-command lispyville-open-above-list (count)
  "Enter `lispyville-preferred-state' above the current list and indent.
With COUNT, move backward/out COUNT lists first. When exiting to the top-level,
insert in between two newlines. This is the lispyville equivalent of
`evil-cp-open-above-form' except for lists only."
  (interactive "<c>")
  (when (lispy--out-backward (or count 1))
    (save-excursion
      (insert "\n")
      (lispy--indent-for-tab))
    (lispy--indent-for-tab)
    (when (lispyville--top-level-p)
      (save-excursion
        (insert "\n")))
    (evil-change-state lispyville-preferred-state)))

;; ** Additional Wrap Key Theme
(defun lispyville-wrap-round (arg)
  "Forward arg to `lispy-wrap-round'.
Never insert a space after the opening delimiter unless in a state in
`lispyville-insert-states'."
  (interactive "P")
  (let ((lispy-insert-space-after-wrap
         (when (memq evil-state lispyville-insert-states)
           lispy-insert-space-after-wrap)))
    (lispy-wrap-round arg)))

(defun lispyville-wrap-brackets (arg)
  "Forward ARG to `lispy-wrap-brackets'.
Never insert a space after the opening delimiter unless in a state in
`lispyville-insert-states'."
  (interactive "P")
  (let ((lispy-insert-space-after-wrap
         (when (memq evil-state lispyville-insert-states)
           lispy-insert-space-after-wrap)))
    (lispy-wrap-brackets arg)))

(defun lispyville-wrap-braces (arg)
  "Forward ARG to `lispy-wrap-braces'.
Never insert a space after the opening delimiter unless in a state in
`lispyville-insert-states'."
  (interactive "P")
  (let ((lispy-insert-space-after-wrap
         (when (memq evil-state lispyville-insert-states)
           lispy-insert-space-after-wrap)))
    (lispy-wrap-braces arg)))

;; * Integration Between Visual State and Lispy's Special Mark State
;; ** Using Both Separately
(defun lispyville-normal-state ()
  "The same as `evil-normal-state' but won't ever enter visual state.
When the mark has been set by some lispy command, this will exit to normal state
instead of entering visual state like `evil-normal-state' would."
  (interactive)
  (deactivate-mark)
  (evil-normal-state))

(defmacro lispyville-wrap-command (command state)
  "Return a function that executes COMMAND and then enters STATE.
The main purpose of this macro is to wrap lispy commands, so that they can be
used in normal state and end up in lispy special or in visual state with a
correct selection. STATE can be any evil state, but it is mainly meant to be
'special' or 'visual', which it will handle differently. When STATE is special,
the resulting command will enter `lispyville-preferred-lispy-state' and move the
point (if necessary) to get to lispy special (like motions will when
`lispyville-motions-put-into-special' is non-nil). When STATE is visual, the
resulting command will ensure that the selection resulting from COMMAND is
correct."
  (let ((name (intern (concat "lispyville-wrap-" (symbol-name command)
                              "-" (symbol-name state)))))
    `(progn
       (defun ,name ()
         ,(concat "Call `" (symbol-name command) "' and then enter "
                  (cl-case state
                    (special "lispy special.")
                    (visual "visual state with a corrected selection.")
                    (t
                     (concat (symbol-name state) " state."))))
         (interactive)
         (call-interactively #',command)
         ,(cl-case state
            (special
             '(lispyville--state-transition t))
            (visual
             '(lispyville--state-transition))
            (t
             `(evil-change-state ',state))))
       #',name)))

(defun lispyville-toggle-mark-type (arg)
  "Switch between evil visual state and lispy special with an active region.
`lispyville-preferred-lispy-state' is used to determine the state to switch to
from visual state. ARG is passed to `lispy-mark-list' when this command is
run in lispy special without an active region or when it is not the default 1."
  (interactive "p")
  (if (region-active-p)
      (cond ((evil-visual-state-p)
             (lispyville--state-transition t))
            ((memq evil-state lispyville-insert-states)
             (cond ((= arg 1)
                    (setq lispyville--inhibit-next-special-force t)
                    (lispyville--state-transition))
                   (t
                    (lispy-mark-list arg)))))
    (lispy-mark-list arg)))

(defun lispyville-escape (arg)
  "Cancel the active region.
If in lispy special with an active region, call `lispy-mark-list' with ARG.
Otherwise enter evil normal state."
  (interactive "p")
  (if (region-active-p)
      (lispy-mark-list arg)
    (evil-normal-state)))

;; ** Using Just Lispy Special
(defvar lispyville--inhibit-next-special-force nil)

(defun lispyville--enter-special ()
  "Enter insert or emacs state based on `lispyville-preferred-lispy-state'."
  (when lispyville-mode
    (if lispyville--inhibit-next-special-force
        (setq lispyville--inhibit-next-special-force nil)
      (evil-change-state lispyville-preferred-lispy-state))))

(defun lispyville-enter-special-when-marking ()
  "Add a hook to enter insert or emacs state after entering visual state.
This is potentially useful for those who want to always use lispy's commands
when the region is active instead of evil's visual states."
  (add-hook 'evil-visual-state-entry-hook #'lispyville--enter-special))

;; ** Using Just Visual State
(defun lispyville--enter-visual ()
  "Enter visual state if not already in visual state.
Meant to be added to `activate-mark-hook'."
  (unless (or (evil-visual-state-p)
              (not lispyville-mode))
    (evil-delay nil
        ;; the activation may only be momentary, so re-check
        ;; in `post-command-hook' before entering Visual state
        '(unless (evil-visual-state-p)
           (when (and (region-active-p)
                      (not deactivate-mark))
             (lispyville--state-transition)))
      'post-command-hook nil t
      "lispyville--enter-visual")))

(defun lispyville-enter-visual-when-marking ()
  "Add a local hook to enter normal state whenever the mark is activated.
This is potentially useful for those who want to enter visual state after
marking something using a command like `lispy-mark' from special."
  (add-hook 'activate-mark-hook #'lispyville--enter-visual))

;; ** To Undo Changes After Testing
(defun lispyville-remove-marking-hooks ()
  "Remove lispyville marking related hooks."
  (interactive)
  (remove-hook 'evil-visual-state-entry-hook #'lispyville--enter-special)
  (remove-hook 'activate-mark-hook #'lispyville--enter-visual))

;; * Insert Key Theme
(defun lispyville-insert-enter ()
  "Insert a space when inserting after an opening delimiter.
Do nothing if entering an insert state (see `lispyville-insert-states') from
visual state or if there is a space or newline after the point. "
  (when (and lispyville-mode
             (not (eq evil-previous-state 'visual))
             (not (memq evil-previous-state lispyville-insert-states))
             (not (lispyville--in-string-p))
             (lispyville--after-left-p)
             (not (looking-at "[[:space:]\n]")))
    (save-excursion
      (insert " "))))

(defun lispyville-insert-exit ()
  "Remove any spaces inserted after an opening delimiter.
Do nothing if entering an insert state from another insert state (see
`lispyville-insert-states')."
  (when (and lispyville-mode
             (not (memq evil-next-state lispyville-insert-states))
             (not (lispyville--in-string-p))
             (lispyville--after-left-p))
    (delete-region (point)
                   (progn
                     (skip-chars-forward "[:space:]")
                     (point)))))

(defun lispyville-space-after-insert (&optional undo)
  "Use state hooks to automatically insert spaces after opening delimiters.

If UNDO is non-nil, remove"
  ;; add-hook works fine even if hook doesn't exist
  (cond (undo
         (dolist (state lispyville-insert-states)
           (remove-hook (intern (format "evil-%s-state-entry-hook" state))
                        #'lispyville-insert-enter)
           (remove-hook (intern (format "evil-%s-state-exit-hook" state))
                        #'lispyville-insert-exit)))
        (t
         (dolist (state lispyville-insert-states)
           (add-hook (intern (format "evil-%s-state-entry-hook" state))
                     #'lispyville-insert-enter)
           (add-hook (intern (format "evil-%s-state-exit-hook" state))
                     #'lispyville-insert-exit)))))

;; * Keybindings
;; TODO update evil dependency on next release (evil-define-key*)
(defun lispyville--define-key (states &rest maps)
  "Helper function for defining keys in `lispyville-mode-map'."
  (declare (indent 1))
  (apply #'evil-define-key* states lispyville-mode-map maps))

;;;###autoload
(defun lispyville-set-key-theme (&optional theme)
  "Binds keys in lispyville-mode-map according to THEME.
When THEME is not given, `lispyville-key-theme' will be used instead."
  (or theme (setq theme lispyville-key-theme))
  (dolist (item theme)
    (let ((type (if (listp item)
                    (car item)
                  item))
          (states (when (listp item)
                    (cdr item))))
      (cl-case type
        (operators
         ;; no states necessary for remaps
         ;; (setq states (or states 'normal))
         (lispyville--define-key states
           [remap evil-yank] #'lispyville-yank
           [remap evil-delete] #'lispyville-delete
           [remap evil-change] #'lispyville-change
           [remap evil-yank-line] #'lispyville-yank-line
           [remap evil-delete-line] #'lispyville-delete-line
           [remap evil-change-line] #'lispyville-change-line
           [remap evil-delete-char] #'lispyville-delete-char-or-splice
           [remap evil-delete-backward-char]
           #'lispyville-delete-char-or-splice-backwards
           [remap evil-substitute] #'lispyville-substitute
           [remap evil-change-whole-line] #'lispyville-change-whole-line
           [remap evil-join] #'lispyville-join))
        (c-w
         ;; no states necessary for remaps
         ;; (setq states (or states '(insert emacs)))
         (lispyville--define-key states
           [remap evil-delete-backward-word]
           #'lispyville-delete-backward-word))
        (c-u
         (lispyville--define-key states
           [remap evil-delete-back-to-indentation]
           #'lispyville-delete-back-to-indentation))
        (prettify
         ;; no states necessary for remaps
         ;; (or states (setq states 'normal))
         (lispyville--define-key states
           [remap evil-indent] #'lispyville-prettify))
        (text-objects
         ;; TODO only define in `lispyville-mode-map'
         (evil-define-key nil evil-inner-text-objects-map
           "a" #'lispyville-inner-atom
           "l" #'lispyville-inner-list
           "x" #'lispyville-inner-sexp
           "f" #'lispyville-inner-function
           "c" #'lispyville-inner-comment
           "S" #'lispyville-inner-string)
         (evil-define-key nil evil-outer-text-objects-map
           "a" #'lispyville-a-atom
           "l" #'lispyville-a-list
           "x" #'lispyville-a-sexp
           "f" #'lispyville-a-function
           "c" #'lispyville-a-comment
           "S" #'lispyville-a-string))
        ((atom-motions atom-movement)
         (if states
             (lispyville--define-key nil
               [remap evil-forward-WORD-begin] #'lispyville-forward-atom-begin
               [remap evil-forward-WORD-end] #'lispyville-forward-atom-end
               [remap evil-backward-WORD-begin] #'lispyville-backward-atom-begin
               [remap evil-backward-WORD-end] #'lispyville-backward-atom-end)
           (lispyville--define-key nil
             [remap evil-forward-word-begin] #'lispyville-forward-atom-begin
             [remap evil-forward-word-end] #'lispyville-forward-atom-end
             [remap evil-backward-word-begin] #'lispyville-backward-atom-begin
             [remap evil-backward-word-end] #'lispyville-backward-atom-end)))
        ((additional-motions additional-movement)
         (or states (setq states 'motion))
         (lispyville--define-key states
           "H" #'lispyville-backward-sexp
           "L" #'lispyville-forward-sexp
           (kbd "M-h") #'lispyville-beginning-of-defun
           (kbd "M-l") #'lispyville-end-of-defun
           ;; reverse of lispy-flow
           "[" #'lispyville-previous-opening
           "]" #'lispyville-next-closing
           ;; like lispy-flow
           "{" #'lispyville-next-opening
           "}" #'lispyville-previous-closing
           ;; like lispy-left and lispy-right
           "(" #'lispyville-backward-up-list
           ")" #'lispyville-up-list))
        (commentary
         (or states (setq states 'normal))
         (lispyville--define-key states
           "gc" #'lispyville-comment-or-uncomment
           "gy" #'lispyville-comment-and-clone-dwim
           (kbd "s-/") #'lispyville-comment-or-uncomment-line))
        (slurp/barf-cp
         (or states (setq states 'normal))
         (lispyville--define-key states
           ">" #'lispyville->
           "<" #'lispyville-<))
        (slurp/barf-lispy
         (or states (setq states 'normal))
         (lispyville--define-key states
           ">" #'lispyville-slurp
           "<" #'lispyville-barf))
        (wrap
         (or states (setq states 'normal))
         (lispyville--define-key states
           (kbd "M-(") #'lispyville-wrap-with-round
           (kbd "M-[") #'lispyville-wrap-with-brackets
           (kbd "M-{") #'lispyville-wrap-with-braces))
        (additional
         (or states (setq states 'normal))
         (lispyville--define-key states
           (kbd "M-j") #'lispyville-drag-forward
           (kbd "M-k") #'lispyville-drag-backward
           (kbd "M-J") #'lispy-join
           (kbd "M-s") #'lispy-splice
           (kbd "M-S") #'lispy-split
           (kbd "M-r") #'lispy-raise-sexp
           (kbd "M-R") #'lispyville-raise-list
           (kbd "M-t") #'transpose-sexps
           (kbd "M-v") #'lispy-convolute-sexp))
        (additional-insert
         (or states (setq states 'normal))
         (lispyville--define-key states
           (kbd "M-i") #'lispyville-insert-at-beginning-of-list
           (kbd "M-a") #'lispyville-insert-at-end-of-list
           (kbd "M-o") #'lispyville-open-below-list
           (kbd "M-O") #'lispyville-open-above-list))
        (additional-wrap
         (or states (setq states 'normal))
         (lispyville--define-key states
           (kbd "M-(") #'lispyville-wrap-round
           (kbd "M-[") #'lispyville-wrap-brackets
           (kbd "M-{") #'lispyville-wrap-braces))
        (arrows
         (or states (setq states 'normal))
         (lispyville--define-key states
           "<i" #'lispyville-insert-at-beginning-of-list
           ">i" #'lispyville-insert-at-end-of-list))
        (insert
         (lispyville-space-after-insert))
        (escape
         (or states (setq states '(insert emacs)))
         (lispyville--define-key states
           (kbd "<escape>") #'lispyville-normal-state))
        (mark
         (or states (setq states '(normal visual)))
         (lispyville--define-key states
           "v" (lispyville-wrap-command lispy-mark-symbol visual)
           "V" (lispyville-wrap-command lispy-mark visual)
           (kbd "C-v") #'lispyville-wrap-lispy-mark-visual))
        (mark-special
         (or states (setq states '(normal visual)))
         (lispyville--define-key states
           "v" (lispyville-wrap-command lispy-mark-symbol special)
           "V" (lispyville-wrap-command lispy-mark special)
           (kbd "C-v") #'lispyville-wrap-lispy-mark-special))
        (mark-toggle
         (or states (setq states '(insert emacs)))
         (lispyville--define-key 'visual
           "v" #'lispyville-toggle-mark-type)
         (lispyville--define-key states
           (kbd "<escape>") #'lispyville-escape))))))

(lispyville-set-key-theme)

;; * Mode Line Integration
(defun lispyville--special-p ()
  "Return whether the point is in special."
  (or (region-active-p)
      (and (not (lispy--in-string-or-comment-p))
           (or (lispy-left-p)
               (lispy-right-p)
               (and (lispy-bolp)
                    (or (looking-at lispy-outline-header)
                        (looking-at lispy-outline)))))))

(defun lispyville--lispy-keybindings-active-p ()
  "Return whether lispy keybindings are active."
  (and lispy-mode
       (memq evil-state lispyville-insert-states)
       (lispyville--special-p)))

(cl-defun lispyville-mode-line-string (&optional (special-text "🍰-special ")
                                                 default-text)
  "When added to the mode line, show SPECIAL-TEXT when in special.
When not in special (or not in a state in `lispyville-insert-states'), show
DEFAULT-TEXT."
  `(:eval
    (if (lispyville--lispy-keybindings-active-p)
        (propertize ,special-text 'face 'lispyville-special-face)
      ,default-text)))

(provide 'lispyville)
;;; lispyville.el ends here
