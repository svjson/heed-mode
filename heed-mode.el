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
(defface heed-frontmatter-delimiter-face
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for the dashed lines delimiting frontmatter.")

(defface heed-frontmatter-key-face
  '((t :inherit font-lock-constant-face))
  "Face for frontmatter keys.")

(defface heed-frontmatter-value-face
  '((t :inherit font-lock-string-face))
  "Face for frontmatter values.")

(defface heed-block-open-face
  '((t :weight bold :inherit font-lock-preprocessor-face))
  "Face for the :: delimiter at the start of Heed block contents.")

(defface head-block-close-face
  '((t :inherit heed-block-open-face))
  "Face for the -- delimiter at the end of Heed block contents.")

(defvar-local heed--frontmatter-range nil
  "Cons cell of (start . end) buffer positions of frontmatter delimiter lines.")

(defvar-local heed--block-boundaries nil
  "List of (start-marker . end-marker) pairs for Heed content blocks.")

(defvar heed-font-lock-keywords
  (list '("^\\(::\\)\\s-*\\([a-zA-Z0-9_-]+\\)\\(?:\\s-*{[^}]*}\\)?"
          (1 'heed-block-open-face)
          (2 font-lock-keyword-face))
        (list #'heed--match-frontmatter-delimiter
              '(0 'heed-frontmatter-delimiter-face))
        (list #'heed--match-frontmatter-prop-line
              '(1 'heed-frontmatter-key-face)
              '(2 'heed-frontmatter-value-face))
        (list #'heed--match-block-close
        '("^\\(@\\)\\([^=\n]+\\)\\(=\\)\\(.*\\)?"
          (1 font-lock-keyword-face)
          (2 'font-lock-constant-face)
          (3 font-lock-comment-face)
          (4 font-lock-variable-name-face))
              '(0 'heed-block-close-face))
        '("\\(^\\|[^\\]\\)\"[^\"]*\"" . font-lock-string-face)
        '("\\_<\\(true\\|false\\|::\\)\\_>" . font-lock-constant-face))
  "Basic font-lock keywords for `heed-mode`.")

(defun heed--match-frontmatter-prop-line (limit)
  "Match key: value lines inside frontmatter only, within LIMIT."
  (when heed--frontmatter-range
    (let ((start (car heed--frontmatter-range))
          (end (cdr heed--frontmatter-range))
          matched)
      (while (and (not matched)
                  (re-search-forward "^\\([a-zA-Z0-9_-]+\\):\\s-*\\(.*\\)$" limit t))
        (let ((pos (match-beginning 0)))
          (when (and (>= pos start) (< pos end))
            (setq matched t))))
      matched)))

(defun heed--clear-block-overlays ()
  "Clear all existing block content overlays."
  (mapc #'delete-overlay heed--block-content-overlays)
  (setq heed--block-content-overlays nil))

(defun heed--match-block-close (limit)
  "Match block-closing `--` lines that are real block terminators.

LIMIT ensures matching is not overly greedy."
  (let (matched)
    (while (and (not matched)
                (re-search-forward "^\\s-*--\\s-*$" limit t))
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
(defun heed--detect-frontmatter ()
  "Detect and record the frontmatter region at the top of the buffer, if present."
  (setq heed--frontmatter-range nil)
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "^[-]+\\s-*$")
      (let ((start (match-beginning 0)))
        (forward-line 1)
        (while (and (not (eobp))
                    (not (looking-at "^[-]+\\s-*$")))
          (forward-line 1))
        (when (looking-at "^[-]+\\s-*$")
          (setq heed--frontmatter-range
                (cons start (match-end 0))))))))

(defun heed--match-frontmatter-delimiter (limit)
  "Match frontmatter delimiter lines only if part of valid header, within LIMIT."
  (when heed--frontmatter-range
    (let ((start (car heed--frontmatter-range))
          (end (cdr heed--frontmatter-range)))
      (when (and (>= (point) start)
                 (< (point) end))
        (when (re-search-forward "^[-]+\\s-*$" limit t)
          (let ((pos (match-beginning 0)))
            (when (and (>= pos start) (< pos end))
              t)))))))


(defun heed--after-change (_beg _end _len)
  "Rescan blocks on buffer changes."
  ;; Could and perhaps should be made this incremental, but this works for now
  (heed--detect-frontmatter)
  (heed--scan-and-mark-blocks))

;;;###autoload
(define-derived-mode heed-mode prog-mode "Heed"
  "Major mode for editing Heed presentation files."
  (setq font-lock-defaults '(heed-font-lock-keywords))
  (heed--detect-frontmatter)
  (heed--scan-and-mark-blocks)
  (add-hook 'after-change-functions #'heed--after-change nil t))

;;; heed-mode.el ends here
