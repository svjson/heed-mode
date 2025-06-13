;;; heed-mode.el --- summary -*- lexical-binding: t -*-

;; Author: Sven Johansson
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Homepage: https://www.github.com/svjson/heed-mode.el
;; Keywords: tools, convenience, heed, presentation, slide

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Major mode for editing .heed presentation files

;; commentary

;;; Code:

(defface heed-block-open-face
  '((t :weight bold :inherit font-lock-preprocessor-face))
  "Face for the :: delimiter at the start of Heed block contents.")

(defface head-block-close-face
  '((t :inherit heed-block-open-face))
  "Face for the -- delimiter at the end of Heed block contents.")

(defvar heed-font-lock-keywords
  (list '("^\\(::\\)\\s-*\\([a-zA-Z0-9_-]+\\)\\(?:\\s-*{[^}]*}\\)?"
          (1 'heed-block-open-face)
          (2 font-lock-keyword-face))
        (list #'heed--match-block-close
              '(0 'head-block-close-face)) ; double-quote to prevent eval
        '("^\\(@\\)\\([^=\n]+\\)\\(=\\)\\(.*\\)?"
          (1 font-lock-keyword-face)
          (2 'font-lock-constant-face)
          (3 font-lock-comment-face)
          (4 font-lock-variable-name-face))
        '("\\(^\\|[^\\]\\)\"[^\"]*\"" . font-lock-string-face)
        '("\\_<\\(true\\|false\\|::\\)\\_>" . font-lock-constant-face))
  "Basic font-lock keywords for `heed-mode`.")

(defvar-local heed-block-boundaries nil
  "List of (start-marker . end-marker) pairs for Heed content blocks.")

(defun heed--match-block-close (limit)
  "Match block-closing `--` lines that are real block terminators.

LIMIT ensures matching is not overly greedy."
  (let (matched)
    (while (and (not matched)
                (re-search-forward "^\\s-*--\\s-*$" limit t))
      (message "matching...")
      ;; Is this position between any known block markers?
      (let ((pos (match-beginning 0)))
        (setq matched
              (seq-some (lambda (range)
                          (and (<= (car range) pos)
                               (< pos (cdr range))))
                        heed-block-boundaries))))
    matched))


(defun heed--scan-and-mark-blocks ()
  "Scan buffer for :: blocks and record start/end markers."
  (setq-local heed-block-boundaries nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^::\\s-*\\([a-zA-Z0-9_-]+\\)" nil t)
      (let ((start (match-beginning 0)))
        (forward-line 1)
        (let ((end (or (save-excursion
                         (while (and (not (eobp))
                                     (not (looking-at "^::\\|\\'")))
                           (forward-line 1))
                         (point))
                       (point-max))))
          (let ((start-marker (copy-marker start))
                (end-marker (copy-marker end t)))
            (push (cons start-marker end-marker) heed-block-boundaries)))))))

(defun heed--after-change (_beg _end _len)
  "Rescan blocks on buffer changes."
  ;; Could and perhaps should be made this incremental, but this works for now
  (heed--scan-and-mark-blocks))

;;;###autoload
(define-derived-mode heed-mode prog-mode "Heed"
  "Major mode for editing Heed presentation files."
  (setq font-lock-defaults '(heed-font-lock-keywords))
  (heed--scan-and-mark-blocks)
  (add-hook 'after-change-functions #'heed--after-change nil t))

;;; heed-mode.el ends here
